{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into LaTeX.
-}
module Text.Pandoc.Writers.LaTeX (
    writeLaTeX
  , writeBeamer
  ) where
import Control.Monad.State.Strict
import Data.Char (isDigit)
import Data.List (intersperse, nubBy, (\\))
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, isNothing)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (unEscapeString)
import Text.DocTemplates (FromContext(lookupContext), renderTemplate,
                          Val(..), Context(..))
import Text.Collate.Lang (Lang (..), renderLang)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report, toLang)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (formatLaTeXBlock, formatLaTeXInline, highlight,
                                 styleToLaTeX)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Slides
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Writers.LaTeX.Caption (getCaption)
import Text.Pandoc.Writers.LaTeX.Table (tableToLaTeX)
import Text.Pandoc.Writers.LaTeX.Citation (citationsToNatbib,
                                           citationsToBiblatex)
import Text.Pandoc.Writers.LaTeX.Types (LW, WriterState (..), startingState)
import Text.Pandoc.Writers.LaTeX.Lang (toPolyglossia, toBabel)
import Text.Pandoc.Writers.LaTeX.Util (stringToLaTeX, StringContext(..),
                                       toLabel, inCmd,
                                       wrapDiv, hypertarget, labelFor,
                                       getListingsLanguage, mbBraced)
import Text.Pandoc.Writers.Shared
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann

-- | Convert Pandoc to LaTeX.
writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeLaTeX options document =
  evalStateT (pandocToLaTeX options document) $
    startingState options

-- | Convert Pandoc to LaTeX Beamer.
writeBeamer :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeBeamer options document =
  evalStateT (pandocToLaTeX options document) $
    (startingState options){ stBeamer = True }

pandocToLaTeX :: PandocMonad m
              => WriterOptions -> Pandoc -> LW m Text
pandocToLaTeX options (Pandoc meta blocks) = do
  -- Strip off final 'references' header if --natbib or --biblatex
  let method = writerCiteMethod options
  let blocks' = if method == Biblatex || method == Natbib
                   then case reverse blocks of
                             Div ("refs",_,_) _:xs -> reverse xs
                             _                     -> blocks
                   else blocks
  -- see if there are internal links
  let isInternalLink (Link _ _ (s,_))
        | Just ('#', xs) <- T.uncons s = [xs]
      isInternalLink _                 = []
  modify $ \s -> s{ stInternalLinks = query isInternalLink blocks' }
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToContext options
              blockListToLaTeX
              (fmap chomp . inlineListToLaTeX)
              meta
  let chaptersClasses = ["memoir","book","report","scrreprt","scrbook","extreport","extbook","tufte-book"]
  let frontmatterClasses = ["memoir","book","scrbook","extbook","tufte-book"]
  -- these have \frontmatter etc.
  beamer <- gets stBeamer
  let documentClass =
        case lookupContext "documentclass" (writerVariables options) `mplus`
              (stringify <$> lookupMeta "documentclass" meta) of
                 Just x -> x
                 Nothing | beamer    -> "beamer"
                         | otherwise -> case writerTopLevelDivision options of
                                          TopLevelPart    -> "book"
                                          TopLevelChapter -> "book"
                                          _               -> "article"
  when (documentClass `elem` chaptersClasses) $
     modify $ \s -> s{ stHasChapters = True }
  case lookupContext "csquotes" (writerVariables options) `mplus`
       (stringify <$> lookupMeta "csquotes" meta) of
     Nothing      -> return ()
     Just "false" -> return ()
     Just _       -> modify $ \s -> s{stCsquotes = True}
  let (blocks'', lastHeader) = if writerCiteMethod options == Citeproc then
                                 (blocks', [])
                               else case reverse blocks' of
                                 Header 1 _ il : _ -> (init blocks', il)
                                 _                 -> (blocks', [])
  blocks''' <- if beamer
                  then toSlides blocks''
                  else return $ makeSections False Nothing blocks''
  main <- blockListToLaTeX blocks'''
  biblioTitle <- inlineListToLaTeX lastHeader
  st <- get
  titleMeta <- stringToLaTeX TextString $ stringify $ docTitle meta
  authorsMeta <- mapM (stringToLaTeX TextString . stringify) $ docAuthors meta
  docLangs <- catMaybes <$>
      mapM (toLang . Just) (ordNub (query (extract "lang") blocks))
  let hasStringValue x = isJust (getField x metadata :: Maybe (Doc Text))
  let geometryFromMargins = mconcat $ intersperse ("," :: Doc Text) $
                            mapMaybe (\(x,y) ->
                                ((x <> "=") <>) <$> getField y metadata)
                              [("lmargin","margin-left")
                              ,("rmargin","margin-right")
                              ,("tmargin","margin-top")
                              ,("bmargin","margin-bottom")
                              ]
  let toPolyObj :: Lang -> Val Text
      toPolyObj lang = MapVal $ Context $
                        M.fromList [ ("name" , SimpleVal $ literal name)
                                   , ("options" , SimpleVal $ literal opts) ]
        where
          (name, opts) = toPolyglossia lang
  mblang <- toLang $ case getLang options meta of
                          Just l -> Just l
                          Nothing | null docLangs -> Nothing
                                  | otherwise     -> Just "en"
  -- we need a default here since lang is used in template conditionals

  let dirs = query (extract "dir") blocks

  let nociteIds = query (\case
                           Cite cs _ -> map citationId cs
                           _         -> [])
                    $ lookupMetaInlines "nocite" meta

  let context  =  defField "toc" (writerTableOfContents options) $
                  defField "toc-depth" (tshow
                                        (writerTOCDepth options -
                                              if stHasChapters st
                                                 then 1
                                                 else 0)) $
                  defField "body" main $
                  defField "title-meta" titleMeta $
                  defField "author-meta"
                        (T.intercalate "; " authorsMeta) $
                  defField "documentclass" documentClass $
                  defField "verbatim-in-note" (stVerbInNote st) $
                  defField "tables" (stTable st) $
                  defField "multirow" (stMultiRow st) $
                  defField "strikeout" (stStrikeout st) $
                  defField "url" (stUrl st) $
                  defField "numbersections" (writerNumberSections options) $
                  defField "lhs" (stLHS st) $
                  defField "graphics" (stGraphics st) $
                  defField "has-chapters" (stHasChapters st) $
                  defField "has-frontmatter" (documentClass `elem` frontmatterClasses) $
                  defField "listings" (writerListings options || stLHS st) $
                  defField "zero-width-non-joiner" (stZwnj st) $
                  defField "beamer" beamer $
                  (if stHighlighting st
                      then case writerHighlightStyle options of
                                Just sty ->
                                   defField "highlighting-macros"
                                      (T.stripEnd $ styleToLaTeX sty)
                                Nothing -> id
                      else id) $
                  (case writerCiteMethod options of
                         Natbib   -> defField "biblio-title" biblioTitle .
                                     defField "natbib" True .
                                     defField "nocite-ids" nociteIds
                         Biblatex -> defField "biblio-title" biblioTitle .
                                     defField "biblatex" True .
                                     defField "nocite-ids" nociteIds
                         _        -> id) $
                  defField "colorlinks" (any hasStringValue
                           ["citecolor", "urlcolor", "linkcolor", "toccolor",
                            "filecolor"]) $
                  (if null dirs
                     then id
                     else defField "dir" ("ltr" :: Text)) $
                  defField "section-titles" True $
                  defField "csl-refs" (stHasCslRefs st) $
                  defField "geometry" geometryFromMargins $
                  (case T.uncons . render Nothing <$>
                        getField "papersize" metadata of
                      -- uppercase a4, a5, etc.
                      Just (Just ('A', ds))
                        | not (T.null ds) && T.all isDigit ds
                          -> resetField "papersize" ("a" <> ds)
                      _   -> id)
                  metadata
  let context' =
          -- note: lang is used in some conditionals in the template,
          -- so we need to set it if we have any babel/polyglossia:
          maybe id (\l -> defField "lang"
                      (literal $ renderLang l)) mblang
        $ maybe id (\l -> defField "babel-lang"
                      (literal $ toBabel l)) mblang
        $ defField "babel-otherlangs"
             (map (literal . toBabel) docLangs)
        $ defField "babel-newcommands" (vcat $
           map (\(poly, babel) -> literal $
            -- \textspanish and \textgalician are already used by babel
            -- save them as \oritext... and let babel use that
            if poly `elem` ["spanish", "galician"]
               then "\\let\\oritext" <> poly <> "\\text" <> poly <> "\n" <>
                    "\\AddBabelHook{" <> poly <> "}{beforeextras}" <>
                      "{\\renewcommand{\\text" <> poly <> "}{\\oritext"
                      <> poly <> "}}\n" <>
                    "\\AddBabelHook{" <> poly <> "}{afterextras}" <>
                      "{\\renewcommand{\\text" <> poly <> "}[2][]{\\foreignlanguage{"
                      <> poly <> "}{##2}}}"
               else (if poly == "latin" -- see #4161
                        then "\\providecommand{\\textlatin}{}\n\\renewcommand"
                        else "\\newcommand") <> "{\\text" <> poly <>
                    "}[2][]{\\foreignlanguage{" <> babel <> "}{#2}}\n" <>
                    "\\newenvironment{" <> poly <>
                    "}[2][]{\\begin{otherlanguage}{" <>
                    babel <> "}}{\\end{otherlanguage}}"
            )
            -- eliminate duplicates that have same polyglossia name
            $ nubBy (\a b -> fst a == fst b)
            -- find polyglossia and babel names of languages used in the document
            $ map (\l -> (fst $ toPolyglossia l, toBabel l)) docLangs
          )
        $ maybe id (defField "polyglossia-lang" . toPolyObj) mblang
        $ defField "polyglossia-otherlangs"
             (ListVal (map toPolyObj docLangs :: [Val Text]))
        $
                  defField "latex-dir-rtl"
           ((render Nothing <$> getField "dir" context) ==
               Just ("rtl" :: Text)) context
  return $ render colwidth $
    case writerTemplate options of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context'

toSlides :: PandocMonad m => [Block] -> LW m [Block]
toSlides bs = do
  opts <- gets stOptions
  let slideLevel = fromMaybe (getSlideLevel bs) $ writerSlideLevel opts
  let bs' = prepSlides slideLevel bs
  walkM (elementToBeamer slideLevel) $ makeSections False Nothing bs'

-- this creates section slides and marks slides with class "slide","block"
elementToBeamer :: PandocMonad m => Int -> Block -> LW m Block
elementToBeamer slideLevel (Div (ident,"section":dclasses,dkvs)
                              xs@(h@(Header lvl _ _) : ys))
  | lvl >  slideLevel
    = return $ Div (ident,"block":dclasses,dkvs) xs
  | lvl <  slideLevel
    = do let isSlide (Div (_,"slide":_,_) _)   = True
             isSlide (Div (_,"section":_,_) _) = True
             isSlide _                         = False
         let (titleBs, slideBs) = break isSlide ys
         return $
           if null titleBs
              then Div (ident,"section":dclasses,dkvs) xs
              else Div (ident,"section":dclasses,dkvs)
                    (h : Div ("","slide":dclasses,dkvs) (h:titleBs) : slideBs)
  | otherwise
    = return $ Div (ident,"slide":dclasses,dkvs) xs
elementToBeamer _ x = return x

isListBlock :: Block -> Bool
isListBlock (BulletList _)     = True
isListBlock (OrderedList _ _)  = True
isListBlock (DefinitionList _) = True
isListBlock _                  = False

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: PandocMonad m
             => Block     -- ^ Block to convert
             -> LW m (Doc Text)
blockToLaTeX Null = return empty
blockToLaTeX (Div attr@(identifier,"block":dclasses,_)
             (Header _ _ ils : bs)) = do
  let blockname
        | "example" `elem` dclasses = "exampleblock"
        | "alert" `elem` dclasses = "alertblock"
        | otherwise = "block"
  ref <- toLabel identifier
  let anchor = if T.null identifier
                  then empty
                  else cr <> "\\protect\\hypertarget" <>
                       braces (literal ref) <> braces empty
  title' <- inlineListToLaTeX ils
  contents <- blockListToLaTeX bs
  wrapDiv attr $ ("\\begin" <> braces blockname <> braces title' <> anchor) $$
                 contents $$ "\\end" <> braces blockname
blockToLaTeX (Div (identifier,"slide":dclasses,dkvs)
               (Header _ (_,hclasses,hkvs) ils : bs)) = do
  -- note: [fragile] is required or verbatim breaks
  let hasCodeBlock (CodeBlock _ _) = [True]
      hasCodeBlock _               = []
  let hasCode (Code _ _) = [True]
      hasCode _          = []
  let classes = ordNub $ dclasses ++ hclasses
  let kvs = ordNub $ dkvs ++ hkvs
  let fragile = "fragile" `elem` classes ||
                not (null $ query hasCodeBlock bs ++ query hasCode bs)
  let frameoptions = ["allowdisplaybreaks", "allowframebreaks", "fragile",
                      "b", "c", "t", "environment",
                      "label", "plain", "shrink", "standout",
                      "noframenumbering"]
  let optionslist = ["fragile" | fragile
                               , isNothing (lookup "fragile" kvs)
                               , "fragile" `notElem` classes] ++
                    [k | k <- classes, k `elem` frameoptions] ++
                    [k <> "=" <> v | (k,v) <- kvs, k `elem` frameoptions]
  let options = if null optionslist
                   then empty
                   else brackets (literal (T.intercalate "," optionslist))
  slideTitle <- if ils == [Str "\0"] -- marker for hrule
                   then return empty
                   else braces <$> inlineListToLaTeX ils
  ref <- toLabel identifier
  let slideAnchor = if T.null identifier
                       then empty
                       else cr <> "\\protect\\hypertarget" <>
                            braces (literal ref) <> braces empty
  contents <- blockListToLaTeX bs >>= wrapDiv (identifier,classes,kvs)
  return $ ("\\begin{frame}" <> options <> slideTitle <> slideAnchor) $$
             contents $$ "\\end{frame}"
blockToLaTeX (Div (identifier@(T.uncons -> Just (_,_)),dclasses,dkvs)
               (Header lvl ("",hclasses,hkvs) ils : bs)) =
  -- move identifier from div to header
  blockToLaTeX (Div ("",dclasses,dkvs)
               (Header lvl (identifier,hclasses,hkvs) ils : bs))
blockToLaTeX (Div (identifier,classes,kvs) bs) = do
  beamer <- gets stBeamer
  oldIncremental <- gets stIncremental
  if beamer && "incremental" `elem` classes
     then modify $ \st -> st{ stIncremental = True }
     else when (beamer && "nonincremental" `elem` classes) $
             modify $ \st -> st { stIncremental = False }
  result <- if identifier == "refs" || -- <- for backwards compatibility
               "csl-bib-body" `elem` classes
               then do
                 modify $ \st -> st{ stHasCslRefs = True }
                 inner <- blockListToLaTeX bs
                 return $ "\\begin{CSLReferences}" <>
                          (if "hanging-indent" `elem` classes
                              then braces "1"
                              else braces "0") <>
                          (case lookup "entry-spacing" kvs of
                             Nothing -> braces "0"
                             Just s  -> braces (literal s))
                          $$ inner
                          $+$ "\\end{CSLReferences}"
               else blockListToLaTeX bs
  modify $ \st -> st{ stIncremental = oldIncremental }
  linkAnchor' <- hypertarget True identifier empty
  -- see #2704 for the motivation for adding \leavevmode
  -- and #7078 for \vadjust pre
  let linkAnchor =
        case bs of
          Para _ : _
            | not (isEmpty linkAnchor')
              -> "\\leavevmode\\vadjust pre{" <> linkAnchor' <> "}%"
          _ -> linkAnchor'
      wrapNotes txt = if beamer && "notes" `elem` classes
                         then "\\note" <> braces txt -- speaker notes
                         else linkAnchor $$ txt
  wrapNotes <$> wrapDiv (identifier,classes,kvs) result
blockToLaTeX (Plain lst) =
  inlineListToLaTeX lst
-- title beginning with fig: indicates that the image is a figure
blockToLaTeX (Para [Image attr@(ident, _, _) txt (src,tgt)])
  | Just tit <- T.stripPrefix "fig:" tgt
  = do
      (capt, captForLof, footnotes) <- getCaption inlineListToLaTeX True txt
      lab <- labelFor ident
      let caption = "\\caption" <> captForLof <> braces capt <> lab
      img <- inlineToLaTeX (Image attr txt (src,tit))
      innards <- hypertarget True ident $
                   "\\centering" $$ img $$ caption <> cr
      let figure = cr <> "\\begin{figure}" $$ innards $$ "\\end{figure}"
      st <- get
      return $ (if stInMinipage st
                 -- can't have figures in notes or minipage (here, table cell)
                 -- http://www.tex.ac.uk/FAQ-ouparmd.html
                then cr <> "\\begin{center}" $$ img $+$ capt $$
                       "\\end{center}"
                else figure) $$ footnotes
-- . . . indicates pause in beamer slides
blockToLaTeX (Para [Str ".",Space,Str ".",Space,Str "."]) = do
  beamer <- gets stBeamer
  if beamer
     then blockToLaTeX (RawBlock "latex" "\\pause")
     else inlineListToLaTeX [Str ".",Space,Str ".",Space,Str "."]
blockToLaTeX (Para lst) =
  inlineListToLaTeX lst
blockToLaTeX (LineBlock lns) =
  blockToLaTeX $ linesToPara lns
blockToLaTeX (BlockQuote lst) = do
  beamer <- gets stBeamer
  case lst of
       [b] | beamer && isListBlock b -> do
         oldIncremental <- gets stIncremental
         modify $ \s -> s{ stIncremental = not oldIncremental }
         result <- blockToLaTeX b
         modify $ \s -> s{ stIncremental = oldIncremental }
         return result
       _ -> do
         oldInQuote <- gets stInQuote
         modify (\s -> s{stInQuote = True})
         contents <- blockListToLaTeX lst
         modify (\s -> s{stInQuote = oldInQuote})
         return $ "\\begin{quote}" $$ contents $$ "\\end{quote}"
blockToLaTeX (CodeBlock (identifier,classes,keyvalAttr) str) = do
  opts <- gets stOptions
  lab <- labelFor identifier
  linkAnchor' <- hypertarget True identifier lab
  let linkAnchor = if isEmpty linkAnchor'
                      then empty
                      else linkAnchor' <> "%"
  let lhsCodeBlock = do
        modify $ \s -> s{ stLHS = True }
        return $ flush (linkAnchor $$ "\\begin{code}" $$ literal str $$
                            "\\end{code}") $$ cr
  let rawCodeBlock = do
        st <- get
        env <- if stInNote st
                  then modify (\s -> s{ stVerbInNote = True }) >>
                       return "Verbatim"
                  else return "verbatim"
        return $ flush (linkAnchor $$ literal ("\\begin{" <> env <> "}") $$
                 literal str $$ literal ("\\end{" <> env <> "}")) <> cr
  let listingsCodeBlock = do
        st <- get
        ref <- toLabel identifier
        kvs <- mapM (\(k,v) -> (k,) <$>
                       stringToLaTeX TextString v) keyvalAttr
        let params = if writerListings (stOptions st)
                     then (case getListingsLanguage classes of
                                Just l  -> [ "language=" <> mbBraced l ]
                                Nothing -> []) ++
                          [ "numbers=left" | "numberLines" `elem` classes
                             || "number" `elem` classes
                             || "number-lines" `elem` classes ] ++
                          [ (if key == "startFrom"
                                then "firstnumber"
                                else key) <> "=" <> mbBraced attr |
                                (key,attr) <- kvs,
                                key `notElem` ["exports", "tangle", "results"]
                                -- see #4889
                          ] ++
                          ["label=" <> ref | not (T.null identifier)]

                     else []
            printParams
                | null params = empty
                | otherwise   = brackets $ hcat (intersperse ", "
                      (map literal params))
        return $ flush ("\\begin{lstlisting}" <> printParams $$ literal str $$
                 "\\end{lstlisting}") $$ cr
  let highlightedCodeBlock =
        case highlight (writerSyntaxMap opts)
                 formatLaTeXBlock ("",classes,keyvalAttr) str of
               Left msg -> do
                 unless (T.null msg) $
                   report $ CouldNotHighlight msg
                 rawCodeBlock
               Right h -> do
                  st <- get
                  when (stInNote st) $ modify (\s -> s{ stVerbInNote = True })
                  modify (\s -> s{ stHighlighting = True })
                  return (flush $ linkAnchor $$ text (T.unpack h))
  case () of
     _ | isEnabled Ext_literate_haskell opts && "haskell" `elem` classes &&
         "literate" `elem` classes           -> lhsCodeBlock
       | writerListings opts                 -> listingsCodeBlock
       | not (null classes) && isJust (writerHighlightStyle opts)
                                             -> highlightedCodeBlock
       | otherwise                           -> rawCodeBlock
blockToLaTeX b@(RawBlock f x) = do
  beamer <- gets stBeamer
  if f == Format "latex" || f == Format "tex" ||
       (f == Format "beamer" && beamer)
     then return $ literal x
     else do
       report $ BlockNotRendered b
       return empty
blockToLaTeX (BulletList []) = return empty  -- otherwise latex error
blockToLaTeX (BulletList lst) = do
  incremental <- gets stIncremental
  isFirstInDefinition <- gets stIsFirstInDefinition
  beamer <- gets stBeamer
  let inc = if beamer && incremental then "[<+->]" else ""
  items <- mapM listItemToLaTeX lst
  let spacing = if isTightList lst
                   then text "\\tightlist"
                   else empty
  return $ text ("\\begin{itemize}" <> inc) $$
             spacing $$
             -- force list at beginning of definition to start on new line
             (if isFirstInDefinition then "\\item[]" else mempty) $$
             vcat items $$
             "\\end{itemize}"
blockToLaTeX (OrderedList _ []) = return empty -- otherwise latex error
blockToLaTeX (OrderedList (start, numstyle, numdelim) lst) = do
  st <- get
  let inc = if stBeamer st && stIncremental st then "[<+->]" else ""
  let oldlevel = stOLLevel st
  isFirstInDefinition <- gets stIsFirstInDefinition
  put $ st {stOLLevel = oldlevel + 1}
  items <- mapM listItemToLaTeX lst
  modify (\s -> s {stOLLevel = oldlevel})
  let beamer = stBeamer st
  let tostyle x = case numstyle of
                       Decimal      -> "\\arabic" <> braces x
                       UpperRoman   -> "\\Roman" <> braces x
                       LowerRoman   -> "\\roman" <> braces x
                       UpperAlpha   -> "\\Alph" <> braces x
                       LowerAlpha   -> "\\alph" <> braces x
                       Example      -> "\\arabic" <> braces x
                       DefaultStyle -> "\\arabic" <> braces x
  let todelim x = case numdelim of
                       OneParen  -> x <> ")"
                       TwoParens -> parens x
                       Period    -> x <> "."
                       _         -> x <> "."
  let exemplar = case numstyle of
                       Decimal      -> "1"
                       UpperRoman   -> "I"
                       LowerRoman   -> "i"
                       UpperAlpha   -> "A"
                       LowerAlpha   -> "a"
                       Example      -> "1"
                       DefaultStyle -> "1"
  let enum = literal $ "enum" <> T.toLower (toRomanNumeral oldlevel)
  let stylecommand
        | numstyle == DefaultStyle && numdelim == DefaultDelim = empty
        | beamer && numstyle == Decimal && numdelim == Period = empty
        | beamer = brackets (todelim exemplar)
        | otherwise = "\\def" <> "\\label" <> enum <>
          braces (todelim $ tostyle enum)
  let resetcounter = if start == 1 || oldlevel > 4
                        then empty
                        else "\\setcounter" <> braces enum <>
                              braces (text $ show $ start - 1)
  let spacing = if isTightList lst
                   then text "\\tightlist"
                   else empty
  return $ text ("\\begin{enumerate}" <> inc)
         $$ stylecommand
         $$ resetcounter
         $$ spacing
         -- force list at beginning of definition to start on new line
         $$ (if isFirstInDefinition then "\\item[]" else mempty)
         $$ vcat items
         $$ "\\end{enumerate}"
blockToLaTeX (DefinitionList []) = return empty
blockToLaTeX (DefinitionList lst) = do
  incremental <- gets stIncremental
  beamer <- gets stBeamer
  let inc = if beamer && incremental then "[<+->]" else ""
  items <- mapM defListItemToLaTeX lst
  let spacing = if all (isTightList . snd) lst
                   then text "\\tightlist"
                   else empty
  return $ text ("\\begin{description}" <> inc) $$ spacing $$ vcat items $$
               "\\end{description}"
blockToLaTeX HorizontalRule =
            return
  "\\begin{center}\\rule{0.5\\linewidth}{0.5pt}\\end{center}"
blockToLaTeX (Header level (id',classes,_) lst) = do
  modify $ \s -> s{stInHeading = True}
  hdr <- sectionHeader classes id' level lst
  modify $ \s -> s{stInHeading = False}
  return hdr
blockToLaTeX (Table attr blkCapt specs thead tbodies tfoot) =
  tableToLaTeX inlineListToLaTeX blockListToLaTeX
               (Ann.toTable attr blkCapt specs thead tbodies tfoot)

blockListToLaTeX :: PandocMonad m => [Block] -> LW m (Doc Text)
blockListToLaTeX lst =
  vsep `fmap` mapM (\b -> setEmptyLine True >> blockToLaTeX b) lst

listItemToLaTeX :: PandocMonad m => [Block] -> LW m (Doc Text)
listItemToLaTeX lst
  -- we need to put some text before a header if it's the first
  -- element in an item. This will look ugly in LaTeX regardless, but
  -- this will keep the typesetter from throwing an error.
  | (Header{} :_) <- lst =
    (text "\\item ~" $$) . nest 2 <$> blockListToLaTeX lst
  | Plain (Str "☐":Space:is) : bs <- lst = taskListItem False is bs
  | Plain (Str "☒":Space:is) : bs <- lst = taskListItem True  is bs
  | Para  (Str "☐":Space:is) : bs <- lst = taskListItem False is bs
  | Para  (Str "☒":Space:is) : bs <- lst = taskListItem True  is bs
  | otherwise = (text "\\item" $$) . nest 2 <$> blockListToLaTeX lst
  where
    taskListItem checked is bs = do
      let checkbox  = if checked
                      then "$\\boxtimes$"
                      else "$\\square$"
      isContents <- inlineListToLaTeX is
      bsContents <- blockListToLaTeX bs
      return $ "\\item" <> brackets checkbox
        $$ nest 2 (isContents $+$ bsContents)

defListItemToLaTeX :: PandocMonad m => ([Inline], [[Block]]) -> LW m (Doc Text)
defListItemToLaTeX (term, defs) = do
    -- needed to turn off 'listings' because it breaks inside \item[...]:
    modify $ \s -> s{stInItem = True}
    term' <- inlineListToLaTeX term
    modify $ \s -> s{stInItem = False}
    -- put braces around term if it contains an internal link,
    -- since otherwise we get bad bracket interactions: \item[\hyperref[..]
    let isInternalLink (Link _ _ (src,_))
          | Just ('#', _) <- T.uncons src = True
        isInternalLink _                  = False
    let term'' = if any isInternalLink term
                    then braces term'
                    else term'
    def'  <- case concat defs of
               [] -> return mempty
               (x:xs) -> do
                 modify $ \s -> s{stIsFirstInDefinition = True }
                 firstitem <- blockToLaTeX x
                 modify $ \s -> s{stIsFirstInDefinition = False }
                 rest <- blockListToLaTeX xs
                 return $ firstitem $+$ rest
    return $ case defs of
     ((Header{} : _) : _)    ->
       "\\item" <> brackets term'' <> " ~ " $$ def'
     ((CodeBlock{} : _) : _) -> -- see #4662
       "\\item" <> brackets term'' <> " ~ " $$ def'
     _                       ->
       "\\item" <> brackets term'' $$ def'

-- | Craft the section header, inserting the secton reference, if supplied.
sectionHeader :: PandocMonad m
              => [Text]  -- classes
              -> Text
              -> Int
              -> [Inline]
              -> LW m (Doc Text)
sectionHeader classes ident level lst = do
  let unnumbered = "unnumbered" `elem` classes
  let unlisted = "unlisted" `elem` classes
  txt <- inlineListToLaTeX lst
  plain <- stringToLaTeX TextString $ T.concat $ map stringify lst
  let removeInvalidInline (Note _)             = []
      removeInvalidInline (Span (id', _, _) _) | not (T.null id') = []
      removeInvalidInline Image{}            = []
      removeInvalidInline x                    = [x]
  let lstNoNotes = foldr (mappend . (\x -> walkM removeInvalidInline x)) mempty lst
  txtNoNotes <- inlineListToLaTeX lstNoNotes
  -- footnotes in sections don't work (except for starred variants)
  -- unless you specify an optional argument:
  -- \section[mysec]{mysec\footnote{blah}}
  optional <- if unnumbered || lstNoNotes == lst || null lstNoNotes
                 then return empty
                 else
                   return $ brackets txtNoNotes
  let contents = if render Nothing txt == plain
                    then braces txt
                    else braces (text "\\texorpdfstring"
                         <> braces txt
                         <> braces (literal plain))
  book <- gets stHasChapters
  opts <- gets stOptions
  let topLevelDivision = if book && writerTopLevelDivision opts == TopLevelDefault
                         then TopLevelChapter
                         else writerTopLevelDivision opts
  beamer <- gets stBeamer
  let level' = if beamer &&
                  topLevelDivision `elem` [TopLevelPart, TopLevelChapter]
               -- beamer has parts but no chapters
               then if level == 1 then -1 else level - 1
               else case topLevelDivision of
                      TopLevelPart    -> level - 2
                      TopLevelChapter -> level - 1
                      TopLevelSection -> level
                      TopLevelDefault -> level
  let sectionType = case level' of
                          -1 -> "part"
                          0  -> "chapter"
                          1  -> "section"
                          2  -> "subsection"
                          3  -> "subsubsection"
                          4  -> "paragraph"
                          5  -> "subparagraph"
                          _  -> ""
  inQuote <- gets stInQuote
  let prefix = if inQuote && level' >= 4
                  then text "\\mbox{}%"
                  -- needed for \paragraph, \subparagraph in quote environment
                  -- see http://tex.stackexchange.com/questions/169830/
                  else empty
  lab <- labelFor ident
  let star = if unnumbered then text "*" else empty
  let stuffing = star <> optional <> contents
  stuffing' <- hypertarget True ident $
                  text ('\\':sectionType) <> stuffing <> lab
  return $ if level' > 5
              then txt
              else prefix $$ stuffing'
                   $$ if unnumbered && not unlisted
                         then "\\addcontentsline{toc}" <>
                                braces (text sectionType) <>
                                braces txtNoNotes
                         else empty

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: PandocMonad m
                  => [Inline]  -- ^ Inlines to convert
                  -> LW m (Doc Text)
inlineListToLaTeX lst = hcat <$>
  mapM inlineToLaTeX (fixLineInitialSpaces . fixInitialLineBreaks $ lst)
    -- nonbreaking spaces (~) in LaTeX don't work after line breaks,
    -- so we turn nbsps after hard breaks to \hspace commands.
    -- this is mostly used in verse.
 where fixLineInitialSpaces [] = []
       fixLineInitialSpaces (LineBreak : Str s : xs)
         | Just ('\160', _) <- T.uncons s
         = LineBreak : fixNbsps s <> fixLineInitialSpaces xs
       fixLineInitialSpaces (x:xs) = x : fixLineInitialSpaces xs
       fixNbsps s = let (ys,zs) = T.span (=='\160') s
                    in  replicate (T.length ys) hspace <> [Str zs]
       hspace = RawInline "latex" "\\hspace*{0.333em}"
       -- We need \hfill\break for a line break at the start
       -- of a paragraph. See #5591.
       fixInitialLineBreaks (LineBreak:xs) =
         RawInline (Format "latex") "\\hfill\\break\n" :
           fixInitialLineBreaks xs
       fixInitialLineBreaks xs = xs

-- | Convert inline element to LaTeX
inlineToLaTeX :: PandocMonad m
              => Inline    -- ^ Inline to convert
              -> LW m (Doc Text)
inlineToLaTeX (Span (id',classes,kvs) ils) = do
  linkAnchor <- hypertarget False id' empty
  lang <- toLang $ lookup "lang" kvs
  let classToCmd "csl-no-emph" = Just "textup"
      classToCmd "csl-no-strong" = Just "textnormal"
      classToCmd "csl-no-smallcaps" = Just "textnormal"
      classToCmd "csl-block" = Just "CSLBlock"
      classToCmd "csl-left-margin" = Just "CSLLeftMargin"
      classToCmd "csl-right-inline" = Just "CSLRightInline"
      classToCmd "csl-indent" = Just "CSLIndent"
      classToCmd _ = Nothing
      kvToCmd ("dir","rtl") = Just "RL"
      kvToCmd ("dir","ltr") = Just "LR"
      kvToCmd _ = Nothing
      langCmds =
        case lang of
           Just lng -> let (l, o) = toPolyglossia lng
                           ops = if T.null o then "" else "[" <> o <> "]"
                       in  ["text" <> l <> ops]
           Nothing  -> []
  let cmds = mapMaybe classToCmd classes ++ mapMaybe kvToCmd kvs ++ langCmds
  contents <- inlineListToLaTeX ils
  return $
    (case classes of
              ["csl-block"] -> (cr <>)
              ["csl-left-margin"] -> (cr <>)
              ["csl-right-inline"] -> (cr <>)
              ["csl-indent"] -> (cr <>)
              _ -> id) $
    (if T.null id'
        then empty
        else "\\protect" <> linkAnchor) <>
    (if null cmds
        then braces contents
        else foldr inCmd contents cmds)
inlineToLaTeX (Emph lst) = inCmd "emph" <$> inlineListToLaTeX lst
inlineToLaTeX (Underline lst) = inCmd "underline" <$> inlineListToLaTeX lst
inlineToLaTeX (Strong lst) = inCmd "textbf" <$> inlineListToLaTeX lst
inlineToLaTeX (Strikeout lst) = do
  -- we need to protect VERB in an mbox or we get an error
  -- see #1294
  -- with regular texttt we don't get an error, but we get
  -- incorrect results if there is a space, see #5529
  contents <- inlineListToLaTeX $ walk (concatMap protectCode) lst
  modify $ \s -> s{ stStrikeout = True }
  return $ inCmd "sout" contents
inlineToLaTeX (Superscript lst) =
  inCmd "textsuperscript" <$> inlineListToLaTeX lst
inlineToLaTeX (Subscript lst) =
  inCmd "textsubscript" <$> inlineListToLaTeX lst
inlineToLaTeX (SmallCaps lst) =
  inCmd "textsc"<$> inlineListToLaTeX lst
inlineToLaTeX (Cite cits lst) = do
  st <- get
  let opts = stOptions st
  case writerCiteMethod opts of
     Natbib   -> citationsToNatbib inlineListToLaTeX cits
     Biblatex -> citationsToBiblatex inlineListToLaTeX cits
     _        -> inlineListToLaTeX lst

inlineToLaTeX (Code (_,classes,kvs) str) = do
  opts <- gets stOptions
  inHeading <- gets stInHeading
  inItem <- gets stInItem
  let listingsCode = do
        let listingsopts = (case getListingsLanguage classes of
                                Just l  -> (("language", mbBraced l):)
                                Nothing -> id)
                           [(k,v) | (k,v) <- kvs
                                  , k `notElem` ["exports","tangle","results"]]
        let listingsopt = if null listingsopts
                             then ""
                             else "[" <>
                                  T.intercalate ", "
                                  (map (\(k,v) -> k <> "=" <> v)
                                   listingsopts) <> "]"
        inNote <- gets stInNote
        when inNote $ modify $ \s -> s{ stVerbInNote = True }
        let chr = case "!\"'()*,-./:;?@" \\ T.unpack str of
                       (c:_) -> c
                       []    -> '!'
        let isEscapable '\\' = True
            isEscapable '{'  = True
            isEscapable '}'  = True
            isEscapable '%'  = True
            isEscapable '~'  = True
            isEscapable '_'  = True
            isEscapable '&'  = True
            isEscapable '#'  = True
            isEscapable '^'  = True
            isEscapable _    = False
        let escChar c | isEscapable c = T.pack ['\\',c]
                      | otherwise     = T.singleton c
        let str' = T.concatMap escChar str
        -- we always put lstinline in a dummy 'passthrough' command
        -- (defined in the default template) so that we don't have
        -- to change the way we escape characters depending on whether
        -- the lstinline is inside another command.  See #1629:
        return $ literal $ "\\passthrough{\\lstinline" <>
                        listingsopt <> T.singleton chr <> str' <> T.singleton chr <> "}"
  let rawCode = liftM (literal . (\s -> "\\texttt{" <> escapeSpaces s <> "}"))
                 $ stringToLaTeX CodeString str
                where escapeSpaces = T.concatMap
                         (\c -> if c == ' ' then "\\ " else T.singleton c)
  let highlightCode =
        case highlight (writerSyntaxMap opts)
                 formatLaTeXInline ("",classes,[]) str of
               Left msg -> do
                 unless (T.null msg) $ report $ CouldNotHighlight msg
                 rawCode
               Right h -> modify (\st -> st{ stHighlighting = True }) >>
                          return (text (T.unpack h))
  case () of
     _ | inHeading || inItem  -> rawCode  -- see #5574
       | writerListings opts  -> listingsCode
       | isJust (writerHighlightStyle opts) && not (null classes)
                              -> highlightCode
       | otherwise            -> rawCode
inlineToLaTeX (Quoted qt lst) = do
  contents <- inlineListToLaTeX lst
  csquotes <- liftM stCsquotes get
  opts <- gets stOptions
  if csquotes
     then return $ case qt of
               DoubleQuote -> "\\enquote" <> braces contents
               SingleQuote -> "\\enquote*" <> braces contents
     else do
       let s1 = if not (null lst) && isQuoted (head lst)
                   then "\\,"
                   else empty
       let s2 = if not (null lst) && isQuoted (last lst)
                   then "\\,"
                   else empty
       let inner = s1 <> contents <> s2
       return $ case qt of
                DoubleQuote ->
                   if isEnabled Ext_smart opts
                      then text "``" <> inner <> text "''"
                      else char '\x201C' <> inner <> char '\x201D'
                SingleQuote ->
                   if isEnabled Ext_smart opts
                      then char '`' <> inner <> char '\''
                      else char '\x2018' <> inner <> char '\x2019'
    where
      isQuoted (Span _ (x:_)) = isQuoted x
      isQuoted (Quoted _ _)   = True
      isQuoted _              = False
inlineToLaTeX (Str str) = do
  setEmptyLine False
  liftM literal $ stringToLaTeX TextString str
inlineToLaTeX (Math InlineMath str) = do
  setEmptyLine False
  return $ "\\(" <> literal (handleMathComment str) <> "\\)"
inlineToLaTeX (Math DisplayMath str) = do
  setEmptyLine False
  return $ "\\[" <> literal (handleMathComment str) <> "\\]"
inlineToLaTeX il@(RawInline f str) = do
  beamer <- gets stBeamer
  if f == Format "latex" || f == Format "tex" ||
        (f == Format "beamer" && beamer)
     then do
       setEmptyLine False
       return $ literal str
     else do
       report $ InlineNotRendered il
       return empty
inlineToLaTeX LineBreak = do
  emptyLine <- gets stEmptyLine
  setEmptyLine True
  return $ (if emptyLine then "\\strut " else "") <> "\\\\" <> cr
inlineToLaTeX SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapAuto     -> return space
       WrapNone     -> return space
       WrapPreserve -> return cr
inlineToLaTeX Space = return space
inlineToLaTeX (Link (id',_,_) txt (src,_)) =
   (case T.uncons src of
     Just ('#', ident) -> do
        contents <- inlineListToLaTeX txt
        lab <- toLabel ident
        return $ text "\\protect\\hyperlink" <> braces (literal lab) <> braces contents
     _ -> case txt of
          [Str x] | unEscapeString (T.unpack x) == unEscapeString (T.unpack src) ->  -- autolink
               do modify $ \s -> s{ stUrl = True }
                  src' <- stringToLaTeX URLString (escapeURI src)
                  return $ literal $ "\\url{" <> src' <> "}"
          [Str x] | Just rest <- T.stripPrefix "mailto:" src,
                    unEscapeString (T.unpack x) == unEscapeString (T.unpack rest) -> -- email autolink
               do modify $ \s -> s{ stUrl = True }
                  src' <- stringToLaTeX URLString (escapeURI src)
                  contents <- inlineListToLaTeX txt
                  return $ "\\href" <> braces (literal src') <>
                     braces ("\\nolinkurl" <> braces contents)
          _ -> do contents <- inlineListToLaTeX txt
                  src' <- stringToLaTeX URLString (escapeURI src)
                  return $ literal ("\\href{" <> src' <> "}{") <>
                           contents <> char '}')
     >>= (if T.null id'
             then return
             else \x -> do
               linkAnchor <- hypertarget False id' x
               return ("\\protect" <> linkAnchor))
inlineToLaTeX il@(Image _ _ (src, _))
  | Just _ <- T.stripPrefix "data:" src = do
      report $ InlineNotRendered il
      return empty
inlineToLaTeX (Image attr _ (source, _)) = do
  setEmptyLine False
  modify $ \s -> s{ stGraphics = True }
  opts <- gets stOptions
  let showDim dir = let d = text (show dir) <> "="
                    in case dimension dir attr of
                         Just (Pixel a)   ->
                           [d <> literal (showInInch opts (Pixel a)) <> "in"]
                         Just (Percent a) ->
                           [d <> literal (showFl (a / 100)) <>
                             case dir of
                                Width  -> "\\textwidth"
                                Height -> "\\textheight"
                           ]
                         Just dim         ->
                           [d <> text (show dim)]
                         Nothing          ->
                           case dir of
                                Width | isJust (dimension Height attr) ->
                                  [d <> "\\textwidth"]
                                Height | isJust (dimension Width attr) ->
                                  [d <> "\\textheight"]
                                _ -> []
      dimList = showDim Width <> showDim Height
      dims = if null dimList
                then empty
                else brackets $ mconcat (intersperse "," dimList)
      source' = if isURI source
                   then source
                   else T.pack $ unEscapeString $ T.unpack source
  source'' <- stringToLaTeX URLString source'
  inHeading <- gets stInHeading
  return $
    (if inHeading then "\\protect\\includegraphics" else "\\includegraphics") <>
    dims <> braces (literal source'')
inlineToLaTeX (Note contents) = do
  setEmptyLine False
  externalNotes <- gets stExternalNotes
  modify (\s -> s{stInNote = True, stExternalNotes = True})
  contents' <- blockListToLaTeX contents
  modify (\s -> s {stInNote = False, stExternalNotes = externalNotes})
  let optnl = case reverse contents of
                   (CodeBlock _ _ : _) -> cr
                   _                   -> empty
  let noteContents = nest 2 contents' <> optnl
  beamer <- gets stBeamer
  -- in beamer slides, display footnote from current overlay forward
  let beamerMark = if beamer
                      then text "<.->"
                      else empty
  if externalNotes
     then do
       modify $ \st -> st{ stNotes = noteContents : stNotes st }
       return "\\footnotemark{}"
       -- note: a \n before } needed when note ends with a Verbatim environment
       else return $ "\\footnote" <> beamerMark <> braces noteContents

-- A comment at the end of math needs to be followed by a newline,
-- or the closing delimiter gets swallowed.
handleMathComment :: Text -> Text
handleMathComment s =
  let (_, ys) = T.break (\c -> c == '\n' || c == '%') $ T.reverse s -- no T.breakEnd
  in  case T.uncons ys of
        Just ('%', ys') -> case T.uncons ys' of
          Just ('\\', _) -> s
          _              -> s <> "\n"
        _                -> s

protectCode :: Inline -> [Inline]
protectCode x@(Code _ _) = [ltx "\\mbox{" , x , ltx "}"]
  where ltx = RawInline (Format "latex")
protectCode x = [x]

setEmptyLine :: PandocMonad m => Bool -> LW m ()
setEmptyLine b = modify $ \st -> st{ stEmptyLine = b }

-- Extract a key from divs and spans
extract :: Text -> Block -> [Text]
extract key (Div attr _)     = lookKey key attr
extract key (Plain ils)      = query (extractInline key) ils
extract key (Para ils)       = query (extractInline key) ils
extract key (Header _ _ ils) = query (extractInline key) ils
extract _ _                  = []

-- Extract a key from spans
extractInline :: Text -> Inline -> [Text]
extractInline key (Span attr _) = lookKey key attr
extractInline _ _               = []

-- Look up a key in an attribute and give a list of its values
lookKey :: Text -> Attr -> [Text]
lookKey key (_,_,kvs) =  maybe [] T.words $ lookup key kvs
