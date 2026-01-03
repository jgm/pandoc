{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX
   Copyright   : Copyright (C) 2006-2025 John MacFarlane
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
    ( MonadState(get, put),
      gets,
      modify,
      evalStateT )
import Control.Monad
    ( MonadPlus(mplus),
      liftM,
      when,
      unless )
import Crypto.Hash (hashWith, MD5(MD5))
import Data.Containers.ListUtils (nubOrd)
import Data.Char (isDigit, isAscii, isLetter)
import Data.List (intersperse, (\\))
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, isNothing)
import Data.Monoid (Any (..))
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (unEscapeString)
import Text.DocTemplates (FromContext(lookupContext), Val(..), renderTemplate)
import Text.Collate.Lang (renderLang)
import Text.Pandoc.Class.PandocMonad (PandocMonad, getPOSIXTime, lookupEnv,
                                      report, toLang)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (formatLaTeXBlock, formatLaTeXInline, highlight,
                                 defaultStyle, styleToLaTeX)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.URI
import Text.Pandoc.Slides
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Writers.LaTeX.Caption (getCaption)
import Text.Pandoc.Writers.LaTeX.Table (tableToLaTeX)
import Text.Pandoc.Writers.LaTeX.Citation (citationsToNatbib,
                                           citationsToBiblatex)
import Text.Pandoc.Writers.LaTeX.Types (LW, WriterState (..), startingState)
import Text.Pandoc.Writers.LaTeX.Lang (toBabel)
import Text.Pandoc.Writers.LaTeX.Util (stringToLaTeX, StringContext(..),
                                       toLabel, inCmd,
                                       wrapDiv, hypertarget, labelFor,
                                       getListingsLanguage, mbBraced)
import Text.Pandoc.Writers.Shared
import qualified Data.Attoparsec.Text as A
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann
import Control.Applicative ((<|>))

-- Work around problems with notes inside emphasis (see #8982)
isolateBigNotes :: ([Inline] -> Inline) -> [Inline] -> [Inline]
isolateBigNotes constructor xs =
  let (before, after) = break isBigNote xs
  in case after of
       (noteInline:rest) -> constructor before :
                            noteInline :
                            isolateBigNotes constructor rest
       [] -> [constructor xs]

isBigNote :: Inline -> Bool
isBigNote (Note [Plain _]) = False  -- A small note
isBigNote (Note [Para _]) =  False  -- A small note
isBigNote (Note _) = True  -- A big note
isBigNote _ = False  -- Not a note

raiseBigNotes :: [Inline] -> [Inline]
raiseBigNotes (Emph inner : xs)
  = isolateBigNotes Emph (raiseBigNotes inner) ++ raiseBigNotes xs
raiseBigNotes (Strong inner : xs)
  = isolateBigNotes Strong (raiseBigNotes inner) ++ raiseBigNotes xs
raiseBigNotes (Underline inner : xs)
  = isolateBigNotes Underline (raiseBigNotes inner) ++ raiseBigNotes xs
raiseBigNotes (Strikeout inner : xs)
  = isolateBigNotes Strikeout (raiseBigNotes inner) ++ raiseBigNotes xs
raiseBigNotes (x : xs)          = x : raiseBigNotes xs
raiseBigNotes [] = []

-- | Convert Pandoc to LaTeX.
writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeLaTeX options document = do
  let Any hasBigNotes =
       query (\il -> if isBigNote il then Any True else Any False) document
  let document' = if hasBigNotes
                     then walk raiseBigNotes document
                     else document
  evalStateT (pandocToLaTeX options document') $ startingState options

-- | Convert Pandoc to LaTeX Beamer.
writeBeamer :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeBeamer options document =
  evalStateT (pandocToLaTeX options document) $
    (startingState options){ stBeamer = True }

pandocToLaTeX :: PandocMonad m
              => WriterOptions -> Pandoc -> LW m Text
pandocToLaTeX options (Pandoc meta blocks) = do
  -- Strip off 'references' header if --natbib or --biblatex
  let method = writerCiteMethod options
  let isRefsDiv (Div ("refs",_,_) _) = True
      isRefsDiv _ = False
  let blocks' = if method == Biblatex || method == Natbib
                   then filter (not . isRefsDiv) blocks
                   else blocks
  -- see if there are internal links
  let isInternalLink (Link _ _ (s,_))
        | Just ('#', xs) <- T.uncons s = [xs]
      isInternalLink _                 = []
  modify $ \s -> s{ stInternalLinks = query isInternalLink blocks' }
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  docLangs <- catMaybes <$>
      mapM (toLang . Just) (nubOrd (query (extract "lang") blocks))
  mblang <- toLang $ case getLang options meta of
                          Just l -> Just l
                          Nothing | null docLangs -> Nothing
                                  | otherwise     -> Just "en"
  modify $ \s -> s{ stLang = mblang }
  metadata <- metaToContext options
              blockListToLaTeX
              (fmap chomp . inlineListToLaTeX)
              meta
  let chaptersClasses = ["memoir","book","report","scrreprt","scrreport",
                        "scrbook","extreport","extbook","tufte-book",
                        "ctexrep","ctexbook","elegantbook"]
  let frontmatterClasses = ["memoir","book","scrbook","extbook","tufte-book",
                           "ctexbook","elegantbook"]
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
  let csquotes =
        case lookupContext "csquotes" (writerVariables options) of
          Just (BoolVal v) -> v
          Just (SimpleVal (Text _ t)) -> t /= ("false" :: Text)
          _ -> case stringify <$> lookupMeta "csquotes" meta of
                  Nothing -> False
                  Just "false" -> False
                  Just _ -> True
  when csquotes $ modify $ \s -> s{stCsquotes = True}
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
  titleMeta <- escapeCommas <$> -- see #10501
                stringToLaTeX TextString (stringify $ docTitle meta)
  subtitleMeta <- stringToLaTeX TextString (stringify $ lookupMetaInlines "subtitle" meta)
  authorsMeta <- mapM (stringToLaTeX TextString . stringify) $ docAuthors meta
  -- The trailer ID is as hash used to identify the PDF. Taking control of its
  -- value is important when aiming for reproducible PDF generation. Setting
  -- `SOURCE_DATE_EPOCH` is the traditional method used to control
  -- reproducible builds. There are no cryptographic requirements for the ID,
  -- so the 128bits (16 bytes) of MD5 are appropriate.
  reproduciblePDF <- isJust <$> lookupEnv "SOURCE_DATE_EPOCH"
  trailerID <- do
    time <- getPOSIXTime
    let hash = T.pack . show . hashWith MD5 $ mconcat
               [ UTF8.fromString $ show time
               , UTF8.fromText $ render Nothing main
               ]
    pure $ mconcat [ "<", hash, "> <", hash, ">" ]
  -- we need a default here since lang is used in template conditionals
  let hasStringValue x = isJust (getField x metadata :: Maybe (Doc Text))
  let geometryFromMargins = mconcat $ intersperse ("," :: Doc Text) $
                            mapMaybe (\(x,y) ->
                                ((x <> "=") <>) <$> getField y metadata)
                              [("lmargin","margin-left")
                              ,("rmargin","margin-right")
                              ,("tmargin","margin-top")
                              ,("bmargin","margin-bottom")
                              ]

  let dirs = query (extract "dir") blocks

  let nociteIds = query (\case
                           Cite cs _ -> map citationId cs
                           _         -> [])
                    $ lookupMetaInlines "nocite" meta

   -- see #7414, avoid escaped underscores
  let unescapeUnderscore = T.replace "\\_" "_"
  let bibliography' = map unescapeUnderscore <$>
                        getField "bibliography" metadata

  let context  =  (case bibliography' of
                     Nothing -> id
                     Just xs -> resetField "bibliography" xs) $
                  defField "toc" (writerTableOfContents options) $
                  defField "lof" (writerListOfFigures options) $
                  defField "lot" (writerListOfTables options) $
                  defField "toc-depth" (tshow
                                        (writerTOCDepth options -
                                              if stHasChapters st
                                                 then 1
                                                 else 0)) $
                  defField "body" main $
                  defField "title-meta" titleMeta $
                  defField "subtitle-meta" subtitleMeta $
                  defField "author-meta"
                        (T.intercalate "; " authorsMeta) $
                  defField "documentclass" documentClass $
                  defField "verbatim-in-note" (stVerbInNote st) $
                  defField "tables" (stTable st) $
                  defField "multirow" (stMultiRow st) $
                  defField "cancel" (stCancel st) $
                  defField "strikeout" (stStrikeout st) $
                  defField "url" (stUrl st) $
                  defField "numbersections" (writerNumberSections options) $
                  defField "lhs" (stLHS st) $
                  defField "graphics" (stGraphics st) $
                  defField "subfigure" (stSubfigure st) $
                  defField "svg" (stSVG st) $
                  defField "has-chapters" (stHasChapters st) $
                  defField "has-frontmatter" (documentClass `elem` frontmatterClasses) $
                  defField "listings" (writerHighlightMethod options ==
                                       IdiomaticHighlighting
                                       || stLHS st) $
                  defField "zero-width-non-joiner" (stZwnj st) $
                  defField "beamer" beamer $
                  (if stHighlighting st
                      then case writerHighlightMethod options of
                             Skylighting sty ->
                                   defField "highlighting-macros"
                                      (T.stripEnd $ styleToLaTeX sty)
                             DefaultHighlighting ->
                                   defField "highlighting-macros"
                                      (T.stripEnd $ styleToLaTeX defaultStyle)
                             _ -> id
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
                      _   -> id) .
                  (if reproduciblePDF
                    then defField "pdf-trailer-id" trailerID
                    else id) $
                  metadata
  let babelLang = mblang >>= toBabel
  let context' =
          -- note: lang is used in some conditionals in the template,
          -- so we need to set it if we have any babel/polyglossia:
          maybe id (\l -> defField "lang"
                      (literal $ renderLang l)) mblang
        $ maybe id (\l -> defField "babel-lang"
                      (literal l)) babelLang
        $ (case babelLang of -- see #8283
                Just l | l `notElem` ldfLanguages
                         -> defField "babeloptions" ("provide=*" :: Text)
                _ -> id)
        $ defField "babel-otherlangs"
             (map literal
               (filter (`elem` ldfLanguages) .
                nubOrd . catMaybes .
                filter (/= babelLang)
                $ map toBabel docLangs))
        $ defField "latex-dir-rtl"
           ((render Nothing <$> getField "dir" context) ==
               Just ("rtl" :: Text)) context
  return $ render colwidth $
    case writerTemplate options of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context'

-- Commas in title-meta need to be put in braces; see #10501
escapeCommas :: Text -> Text
escapeCommas = T.replace "," "{,}"

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
           case titleBs of
              [] -> Div (ident,"section":dclasses,dkvs) xs
              [Div (_,"notes":_,_) _] ->  -- see #7857, don't create frame
                    -- just for speaker notes after section heading
                    Div (ident,"section":dclasses,dkvs) xs
              _  -> Div (ident,"section":dclasses,dkvs)
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
blockToLaTeX (Div attr@(identifier,"block":dclasses,_)
             (Header _ _ ils : bs)) = do
  let blockname
        | "example" `elem` dclasses = "exampleblock"
        | "alert" `elem` dclasses = "alertblock"
        | otherwise = "block"
  anchor <- if T.null identifier
               then pure empty
               else (cr <>) <$> hypertarget identifier
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
  let classes = nubOrd $ dclasses ++ hclasses
  let kvs = nubOrd $ dkvs ++ hkvs
  let fragile = "fragile" `elem` classes ||
                not (null $ query hasCodeBlock bs ++ query hasCode bs)
  let frameoptions = ["allowdisplaybreaks", "allowframebreaks", "fragile",
                      "b", "c", "t", "environment", "s", "squeeze",
                      "label", "plain", "shrink", "standout",
                      "noframenumbering", "containsverbatim"]
  let optionslist = ["fragile" | fragile
                               , isNothing (lookup "fragile" kvs)
                               , "fragile" `notElem` classes
                               , "containsverbatim" `notElem` classes] ++
                    [k | k <- classes, k `elem` frameoptions] ++
                    [k <> "=" <> v | (k,v) <- kvs, k `elem` frameoptions] ++
                    [v | ("frameoptions", v) <- kvs]
  let options = if null optionslist
                   then empty
                   else brackets (literal (T.intercalate "," optionslist))
  slideTitle <- if ils == [Str "\0"] -- marker for hrule
                   then return empty
                   else braces <$> inlineListToLaTeX ils
  slideAnchor <- if T.null identifier
                    then pure empty
                    else (cr <>) <$> hypertarget identifier
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
  result <- if (identifier == "refs" || -- <- for backwards compatibility
                "csl-bib-body" `elem` classes) &&
               (not (null bs))
               then do
                 modify $ \st -> st{ stHasCslRefs = True }
                 inner <- blockListToLaTeX bs
                 return $ ("\\begin{CSLReferences}"
                            <> braces
                                (if "hanging-indent" `elem` classes
                                    then "1"
                                    else "0")
                            <> braces
                               (maybe "1" literal (lookup "entry-spacing" kvs)))
                          $$ inner
                          $+$ "\\end{CSLReferences}"
               else blockListToLaTeX bs
  modify $ \st -> st{ stIncremental = oldIncremental }
  let wrap txt
       | beamer && "notes" `elem` classes
         = pure ("\\note" <> braces txt) -- speaker notes
       | "ref-" `T.isPrefixOf` identifier
         = do
             lab <- toLabel identifier
             pure $ ("\\bibitem" <> brackets "\\citeproctext"
                      <> braces (literal lab)) $$ txt
         | otherwise = do
             linkAnchor <- hypertarget identifier
             pure $ linkAnchor $$ txt
  wrapDiv (identifier,classes,kvs) result >>= wrap
blockToLaTeX (Plain lst) =
  inlineListToLaTeX lst
-- . . . indicates pause in beamer slides
blockToLaTeX (Para [Str ".",Space,Str ".",Space,Str "."]) = do
  beamer <- gets stBeamer
  if beamer
     then blockToLaTeX (RawBlock "latex" "\\pause")
     else inlineListToLaTeX [Str ".",Space,Str ".",Space,Str "."]
blockToLaTeX (Para lst) =
  if null lst
     then do
       opts <- gets stOptions
       if isEnabled Ext_empty_paragraphs opts
          then pure "\\hfill\\par"
          else pure mempty
     else inlineListToLaTeX lst
blockToLaTeX (LineBlock lns) =
  blockToLaTeX $ linesToPara lns
blockToLaTeX (BlockQuote lst) = do
  beamer <- gets stBeamer
  csquotes <- liftM stCsquotes get
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
         let envname = if csquotes then "displayquote" else "quote"
         return $ ("\\begin" <> braces envname) $$
                  contents $$
                  ("\\end" <> braces envname)
blockToLaTeX (CodeBlock (identifier,classes,keyvalAttr) str) = do
  opts <- gets stOptions
  inNote <- stInNote <$> get
  linkAnchor <- if T.null identifier
                   then pure empty
                   else ((<> cr) . (<> "%")) <$> hypertarget identifier
  let lhsCodeBlock = do
        modify $ \s -> s{ stLHS = True }
        return $ flush (linkAnchor $$ "\\begin{code}" $$ literal str $$
                            "\\end{code}") $$ cr
  let rawCodeBlock = do
        env <- if inNote
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
        let params = if writerHighlightMethod (stOptions st)
                        == IdiomaticHighlighting
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
                 formatLaTeXBlock ("",classes ++ ["default"],keyvalAttr) str of
               Left msg -> do
                 unless (T.null msg) $
                   report $ CouldNotHighlight msg
                 rawCodeBlock
               Right h -> do
                  when inNote $ modify (\s -> s{ stVerbInNote = True })
                  modify (\s -> s{ stHighlighting = True })
                  return (flush $ linkAnchor $$ literal h)
  case () of
     _ | isEnabled Ext_literate_haskell opts && "haskell" `elem` classes &&
         "literate" `elem` classes           -> lhsCodeBlock
       | writerHighlightMethod opts == IdiomaticHighlighting
                                             -> listingsCodeBlock
       | not (null classes), Skylighting _ <- writerHighlightMethod opts
                                             -> highlightedCodeBlock
       | not (null classes), DefaultHighlighting <- writerHighlightMethod opts
                                             -> highlightedCodeBlock
       -- we don't want to use \begin{verbatim} if our code
       -- contains \end{verbatim}:
       | inNote
       , "\\end{Verbatim}" `T.isInfixOf` str -> highlightedCodeBlock
       | not inNote
       , "\\end{verbatim}" `T.isInfixOf` str -> highlightedCodeBlock
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
  items <- mapM (listItemToLaTeX False) lst
  let spacing = if isTightList lst
                   then text "\\tightlist"
                   else empty
  return $ -- force list to start on new line if in a defn list
             (if isFirstInDefinition then "\\hfill" else mempty) $$
             text ("\\begin{itemize}" <> inc) $$
             spacing $$
             -- force list at beginning of definition to start on new line
             vcat items $$
             "\\end{itemize}"
blockToLaTeX (OrderedList _ []) = return empty -- otherwise latex error
blockToLaTeX (OrderedList (start, numstyle, numdelim) lst) = do
  st <- get
  let inc = if stBeamer st && stIncremental st then "[<+->]" else ""
  let oldlevel = stOLLevel st
  isFirstInDefinition <- gets stIsFirstInDefinition
  put $ st {stOLLevel = oldlevel + 1}
  items <- mapM (listItemToLaTeX True) lst
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
  return $ -- force list at beginning of definition to start on new line
           (if isFirstInDefinition then "\\hfill" else mempty)
         $$ text ("\\begin{enumerate}" <> inc)
         $$ stylecommand
         $$ resetcounter
         $$ spacing
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
blockToLaTeX (Figure (ident, _, kvs) captnode body) = do
  opts <- gets stOptions
  (capt, captForLof, footnotes) <- getCaption inlineListToLaTeX True captnode
  lab <- labelFor ident
  let caption = "\\caption" <> captForLof <> braces capt <> lab
      placement = case lookup "latex-placement" kvs of
        Just p -> brackets (text (T.unpack p))
        _      -> text ""

  isSubfigure <- gets stInFigure
  modify $ \st -> st{ stInFigure = True }
  contents <- case body of
    [b] -> blockToLaTeX b
    bs  -> mconcat . intersperse (cr <> "\\hfill") <$>
           mapM (toSubfigure (length bs)) bs
  let innards = "\\centering" $$
                (case writerFigureCaptionPosition opts of
                  CaptionBelow -> contents $$ caption
                  CaptionAbove -> caption $$ contents) <> cr
  modify $ \st ->
    st{ stInFigure = isSubfigure
      , stSubfigure = stSubfigure st || isSubfigure
      }

  let containsTable = getAny . query (\case
        Table {}  -> Any True
        _         -> Any False)
  st <- get
  return $ (case () of
    _ | containsTable body ->
          -- placing a longtable in a figure or center environment does
          -- not make sense.
          cr <> contents
    _ | stInMinipage st ->
          -- can't have figures in notes or minipage (here, table cell)
          -- http://www.tex.ac.uk/FAQ-ouparmd.html
          cr <> "\\begin{center}" $$ contents $+$ capt $$ "\\end{center}"
    _ | isSubfigure ->
          innards
    _ ->  cr <> "\\begin{figure}" <> placement $$ innards $$ "\\end{figure}")
    $$ footnotes

toSubfigure :: PandocMonad m => Int -> Block -> LW m (Doc Text)
toSubfigure nsubfigs blk = do
  contents <- blockToLaTeX blk
  let linewidth = tshow @Double (0.9 / fromIntegral nsubfigs) <> "\\linewidth"
  return $ cr <> case blk of
    Figure {}    -> vcat
                    [ "\\begin{subfigure}[t]" <> braces (literal linewidth)
                    , contents
                    , "\\end{subfigure}"
                    ]
    _            -> vcat
                    [ "\\begin{minipage}[t]" <> braces (literal linewidth)
                    , contents
                    , "\\end{minipage}"
                    ]

blockListToLaTeX :: PandocMonad m => [Block] -> LW m (Doc Text)
blockListToLaTeX lst =
  vsep `fmap` mapM (\b -> setEmptyLine True >> blockToLaTeX b) lst

listItemToLaTeX :: PandocMonad m => Bool -> [Block] -> LW m (Doc Text)
listItemToLaTeX isOrdered lst
  -- we need to put some text before a header if it's the first
  -- element in an item. This will look ugly in LaTeX regardless, but
  -- this will keep the typesetter from throwing an error.
  | (Header{} :_) <- lst =
    (text "\\item ~" $$) . nest 2 <$> blockListToLaTeX lst
  | not isOrdered
  , Just (checked, bs) <- toTaskListItem lst
   = taskListItem checked bs
  | otherwise = (text "\\item" $$) . nest 2 <$> blockListToLaTeX lst
  where
    taskListItem checked bs = do
      let checkbox  = if checked
                      then "$\\boxtimes$"
                      else "$\\square$"
      let bs' = case bs of
                  Plain ils : xs -> Para ils : xs
                  _ -> bs
      bsContents <- blockListToLaTeX bs'
      return $ "\\item" <> brackets checkbox $$ nest 2 bsContents

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

-- | Craft the section header, inserting the section reference, if supplied.
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
  txtNoLinksNoNotes <- inlineListToLaTeX (removeLinks lstNoNotes)
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
  let prefix = if inQuote
                  then text "\\mbox{}%"
                  -- needed for \paragraph, \subparagraph in quote environment
                  -- see http://tex.stackexchange.com/questions/169830/
                  else empty
  lab <- labelFor ident
  let star = if unnumbered then text "*" else empty
  let title = star <> optional <> contents
  return $ if level' > 5
              then txt
              else prefix
                   $$ text ('\\':sectionType) <> title <> lab
                   $$ if unnumbered && not unlisted
                         then "\\addcontentsline{toc}" <>
                                braces (text sectionType) <>
                                braces txtNoLinksNoNotes
                         else empty

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: PandocMonad m
                  => [Inline]  -- ^ Inlines to convert
                  -> LW m (Doc Text)
inlineListToLaTeX lst = hcat <$>
  mapM inlineToLaTeX
    (addKerns . fixLineInitialSpaces . fixInitialLineBreaks $ lst)
    -- nonbreaking spaces (~) in LaTeX don't work after line breaks,
    -- so we insert a strut: this is mostly used in verse.
 where fixLineInitialSpaces [] = []
       fixLineInitialSpaces (LineBreak : Str s : xs)
         | Just ('\160', _) <- T.uncons s
         = LineBreak : RawInline "latex" "\\strut " : Str s
            : fixLineInitialSpaces xs
       fixLineInitialSpaces (x:xs) = x : fixLineInitialSpaces xs
       -- We need \hfill\break for a line break at the start
       -- of a paragraph. See #5591.
       fixInitialLineBreaks (LineBreak:xs) =
         RawInline (Format "latex") "\\hfill\\break\n" :
           fixInitialLineBreaks xs
       fixInitialLineBreaks xs = xs
       addKerns [] = []
       addKerns (Str s : q@Quoted{} : rest)
         | isQuote (T.takeEnd 1 s) =
           Str s : RawInline (Format "latex") "\\," : addKerns (q:rest)
       addKerns (q@Quoted{} : Str s : rest)
         | isQuote (T.take 1 s) =
           q : RawInline (Format "latex") "\\," : addKerns (Str s : rest)
       addKerns (x:xs) = x : addKerns xs
       isQuote "\"" = True
       isQuote "'" = True
       isQuote "\x2018" = True
       isQuote "\x2019" = True
       isQuote "\x201C" = True
       isQuote "\x201D" = True
       isQuote _ = False

-- | Convert inline element to LaTeX
inlineToLaTeX :: PandocMonad m
              => Inline    -- ^ Inline to convert
              -> LW m (Doc Text)
inlineToLaTeX (Span ("",["mark"],[]) lst) = do
  modify $ \st -> st{ stStrikeout = True } -- this gives us the soul package
  inCmd "hl" <$> inSoulCommand (inlineListToLaTeX lst)
inlineToLaTeX (Span (id',classes,kvs) ils) = do
  linkAnchor <- hypertarget id'
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
        case lang >>= toBabel of
           Just l  -> ["foreignlanguage{" <> l <> "}"]
           Nothing -> []
  let cmds = mapMaybe classToCmd classes ++ mapMaybe kvToCmd kvs ++ langCmds
  contents <- inlineListToLaTeX ils
  return $
    (if "csl-right-inline" `elem` classes
        then ("%" <>) -- see #7932
        else id) $
    (if any (`elem` classes)
            ["csl-block","csl-left-margin","csl-right-inline","csl-indent"]
        then (cr <>)
        else id) $
    (if T.null id'
        then empty
        else linkAnchor) <>
    (if null cmds
        then braces contents
        else foldr inCmd contents cmds)
inlineToLaTeX (Emph lst) = inCmd "emph" <$> inlineListToLaTeX lst
inlineToLaTeX (Underline lst) = do
  modify $ \st -> st{ stStrikeout = True } -- this gives us the soul package
  inCmd "ul" <$> inSoulCommand (inlineListToLaTeX lst)
inlineToLaTeX (Strong lst) = inCmd "textbf" <$> inlineListToLaTeX lst
inlineToLaTeX (Strikeout lst) = do
  modify $ \s -> s{ stStrikeout = True }
  inCmd "st" <$> inSoulCommand (inlineListToLaTeX lst)
inlineToLaTeX (Superscript lst) =
  inCmd "textsuperscript" <$> inlineListToLaTeX lst
inlineToLaTeX (Subscript lst) =
  inCmd "textsubscript" <$> inlineListToLaTeX lst
inlineToLaTeX (SmallCaps lst) =
  inCmd "textsc"<$> inlineListToLaTeX lst
inlineToLaTeX (Cite cits lst) = do
  opts <- gets stOptions
  modify $ \st -> st{ stInCite = True }
  res <- case writerCiteMethod opts of
           Natbib   -> citationsToNatbib inlineListToLaTeX cits
           Biblatex -> citationsToBiblatex inlineListToLaTeX cits
           _        -> inlineListToLaTeX lst
  modify $ \st -> st{ stInCite = False }
  pure res

inlineToLaTeX (Code (_,classes,kvs) str) = do
  opts <- gets stOptions
  inHeading <- gets stInHeading
  inItem <- gets stInItem
  inSoul <- gets stInSoulCommand
  inCaption <- gets stInCaption
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
  -- for soul commands we need to protect VERB in an mbox or we get an error
  -- (see #1294). with regular texttt we don't get an error, but we get
  -- incorrect results if there is a space (see #5529).
  let inMbox x = "\\mbox" <> braces x

  -- for captions we need to protect VERB with \protect (see #6821)
  let protect x = "\\protect" <> x

  let optionalProtect = case () of _ | inSoul -> inMbox
                                     | inCaption -> protect
                                     | otherwise -> id
  optionalProtect <$>
   case writerHighlightMethod opts of
     _ | inHeading || inItem  -> rawCode  -- see #5574
     IdiomaticHighlighting    -> listingsCode
     Skylighting _ | not (null classes)
                              -> highlightCode
     DefaultHighlighting | not (null classes)
                              -> highlightCode
     _noHighlighting          -> rawCode
inlineToLaTeX (Quoted qt lst) = do
  contents <- inlineListToLaTeX lst
  csquotes <- liftM stCsquotes get
  opts <- gets stOptions
  if csquotes
     then return $ case qt of
               DoubleQuote -> "\\enquote" <> braces contents
               SingleQuote -> "\\enquote*" <> braces contents
     else do
       let endsWithQuote xs =
             case reverse xs of
                   Quoted{}:_ -> True
                   Span _ ys : _ -> endsWithQuote ys
                   Str s:_ -> T.takeEnd 1 s == "'"
                   _ -> False
       let beginsWithQuote xs =
             case xs of
                   Quoted{}:_ -> True
                   Span _ ys : _ -> beginsWithQuote ys
                   Str s:_ -> T.take 1 s == "`"
                   _ -> False
       let inner = (if beginsWithQuote lst then "\\," else mempty)
                    <> contents
                    <> (if endsWithQuote lst then "\\," else mempty)
       return $ case qt of
                DoubleQuote ->
                   if isEnabled Ext_smart opts
                      then text "``" <> inner <> text "''"
                      else char '\x201C' <> inner <> char '\x201D'
                SingleQuote ->
                   if isEnabled Ext_smart opts
                      then char '`' <> inner <> char '\''
                      else char '\x2018' <> inner <> char '\x2019'
inlineToLaTeX (Str str) = do
  setEmptyLine False
  liftM literal $ stringToLaTeX TextString str
inlineToLaTeX (Math _ str)
  | isMathEnv str -- see #9711
  = do setEmptyLine False
       when (needsCancel str) $ modify $ \st -> st{ stCancel = True }
       pure $ literal str
inlineToLaTeX (Math InlineMath str) = do
  setEmptyLine False
  inSoul <- gets stInSoulCommand
  let contents = literal (handleMathComment str)
  when (needsCancel str) $ modify $ \st -> st{ stCancel = True }
  return $
    if inSoul -- #9597
       then "$" <> contents <> "$"
       else "\\(" <> contents <> "\\)"
inlineToLaTeX (Math DisplayMath str) = do
  setEmptyLine False
  inSoul <- gets stInSoulCommand
  let contents = literal (handleMathComment str)
  when (needsCancel str) $ modify $ \st -> st{ stCancel = True }
  return $
    if inSoul -- # 9597
       then "$$" <> contents <> "$$"
       else "\\[" <> contents <> "\\]"
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
        inCite <- gets stInCite
        beamer <- gets stBeamer
        return $
          if inCite && "#ref-" `T.isPrefixOf` src
             then "\\citeproc" <> braces (literal lab) <> braces contents
             else if beamer
                     then "\\hyperlink" <> braces (literal lab) <> braces contents
                     else "\\hyperref" <> brackets (literal lab) <> braces contents
     _ -> case txt of
          -- For soul sommands we need to protect \url and \href in an mbox or
          -- we get an error (see #9366)
          [Str x] | T.all isAscii x  -- see #8802
                  , unEscapeString (T.unpack x) ==
                    unEscapeString (T.unpack src) ->  -- autolink
               do modify $ \s -> s{ stUrl = True }
                  src' <- stringToLaTeX URLString (escapeURI src)
                  protectInMboxIfInSoul $ literal $ "\\url{" <> src' <> "}"
          [Str x] | Just rest <- T.stripPrefix "mailto:" src,
                    unEscapeString (T.unpack x) == unEscapeString (T.unpack rest) -> -- email autolink
               do modify $ \s -> s{ stUrl = True }
                  src' <- stringToLaTeX URLString (escapeURI src)
                  contents <- inlineListToLaTeX txt
                  protectInMboxIfInSoul $ "\\href" <> braces (literal src') <>
                     braces ("\\nolinkurl" <> braces contents)
          _ -> do contents <- inlineListToLaTeX txt
                  src' <- stringToLaTeX URLString (escapeURI src)
                  protectInMboxIfInSoul $ literal ("\\href{" <> src' <> "}{") <>
                    contents <> char '}')
     >>= (if T.null id'
             then return
             else \x -> do
               linkAnchor <- hypertarget id'
               return (linkAnchor <> x))
inlineToLaTeX il@(Image _ _ (src, _))
  | Just _ <- T.stripPrefix "data:" src = do
      report $ InlineNotRendered il
      return empty
inlineToLaTeX (Image attr@(_,_,kvs) description (source, _)) = do
  setEmptyLine False
  let isSVG = ".svg" `T.isSuffixOf` source || ".SVG" `T.isSuffixOf` source
  modify $ \s -> s{ stGraphics = True
                  , stSVG = stSVG s || isSVG }
  opts <- gets stOptions
  mbalt <- if isSVG
              then pure Nothing
              else case lookup "alt" kvs of
                     Just x -> Just <$> stringToLaTeX TextString x
                     Nothing
                       | null description -> pure Nothing
                       | otherwise -> Just <$> stringToLaTeX TextString
                                                  (stringify description)
  let showDim dir = let d = text (show dir) <> "="
                    in case dimension dir attr of
                         Just (Pixel a)   ->
                           [d <> literal (showInInch opts (Pixel a)) <> "in"]
                         Just (Percent a) ->
                           [d <> literal (showFl (a / 100)) <>
                             case dir of
                                Width  -> "\\linewidth"
                                Height -> "\\textheight"
                           ]
                         Just dim         ->
                           [d <> text (show dim)]
                         Nothing          ->
                           case dir of
                                Width | isJust (dimension Height attr) ->
                                  [d <> "\\linewidth"]
                                Height | isJust (dimension Width attr) ->
                                  [d <> "\\textheight"]
                                _ -> []
      optList = showDim Width <> showDim Height <>
                (case (dimension Height attr, dimension Width attr) of
                  (Just _, Just _) -> []
                  _ -> ["keepaspectratio"]) <>
                maybe [] (\x -> ["page=" <> literal x]) (lookup "page" kvs) <>
                maybe [] (\x -> ["trim=" <> literal x]) (lookup "trim" kvs) <>
                maybe [] (\x -> ["alt=" <> braces (literal x)]) mbalt <>
                maybe [] (const ["clip"]) (lookup "clip" kvs)
      options = if null optList
                   then empty
                   else brackets $ mconcat (intersperse "," optList)
      source' = if isURI source
                   then source
                   else T.pack $ unEscapeString $ T.unpack source
  source'' <- stringToLaTeX URLString source'
  inHeading <- gets stInHeading
  return $
    (if inHeading then "\\protect" else "") <>
    (case dimension Width attr `mplus` dimension Height attr of
       Just _ -> id
       Nothing -> ("\\pandocbounded" <>) . braces)
      ((if isSVG then "\\includesvg" else "\\includegraphics") <>
        options <> braces (literal source''))
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
  -- and ensure that the note is on the frame, not e.g. the column (#5769, #5954)
  incremental <- gets stIncremental
  let beamerMark = if beamer
                      then if incremental
                           then text "<.->[frame]"
                           else text "<\\value{beamerpauses}->[frame]"
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

-- soul doesn't like \(..\) delimiters, so we change these to $ (#9597)
-- when processing their contents.
inSoulCommand :: PandocMonad m => LW m a -> LW m a
inSoulCommand pa = do
  oldInSoulCommand <- gets stInSoulCommand
  modify $ \st -> st{ stInSoulCommand = True }
  result <- pa
  modify $ \st -> st{ stInSoulCommand = oldInSoulCommand }
  pure result

-- Inside soul commands some commands need to be protected in an mbox
-- or we get an error (e.g. see #1294)
protectInMboxIfInSoul :: (PandocMonad m, HasChars a) => Doc a -> LW m (Doc a)
protectInMboxIfInSoul command = do
  inSoul <- gets stInSoulCommand
  return $ if inSoul
    then "\\mbox" <> braces command
    else command

-- Babel languages with a .ldf that works well with all engines (see #8283).
-- We follow the guidance from the Babel documentation:
-- "In general, you should do this for European languages written in Latin
-- and Cyrillic scripts, as well as for Vietnamese."
ldfLanguages :: [Text]
ldfLanguages =
  [ "magyar"
  , "croatian"
  , "ngerman"
  , "germanb"
  , "german"
  , "austrian"
  , "ngermanb"
  , "naustrian"
  , "nswissgerman"
  , "swissgerman"
  , "italian"
  , "greek"
  , "azerbaijani"
  , "american"
  , "newzealand"
  , "UKenglish"
  , "USenglish"
  , "australian"
  , "british"
  , "canadian"
  , "english"
  , "bahasa"
  , "slovak"
  , "finnish"
  , "occitan"
  , "swedish"
  , "brazil"
  , "portuguese"
  , "portuges"
  , "brazilian"
  , "spanish"
  , "norwegian"
  , "norsk"
  , "nynorsk"
  , "bulgarian"
  , "breton"
  , "belarusian"
  , "piedmontese"
  , "esperanto"
  , "lithuanian"
  , "ukraineb"
  , "scottishgaelic"
  , "scottish"
  , "dutch"
  , "afrikaans"
  , "czech"
  , "serbian"
  , "latvian"
  , "catalan"
  , "basque"
  , "albanian"
  , "irish"
  , "serbianc"
  , "interlingua"
  , "bosnian"
  , "friulan"
  , "romanian"
  , "icelandic"
  , "classiclatin"
  , "ecclesiasticlatin"
  , "medievallatin"
  , "latin"
  , "georgian"
  , "macedonian"
  , "welsh"
  , "vietnamese"
  , "romansh"
  , "danish"
  , "lsorbian"
  , "usorbian"
  , "polish-compat"
  , "polish"
  , "estonian"
  , "french"
  , "frenchb"
  , "canadien"
  , "acadian"
  , "francais"
  , "turkish"
  , "hindi"
  , "northernsami"
  , "samin"
  , "russianb"
  , "galician"
  , "slovene"
  ]

isMathEnv :: Text -> Bool
isMathEnv t =
  case T.stripPrefix "\\begin{" (T.dropWhile isSpaceChar t) of
    Nothing -> False
    Just t' -> T.takeWhile (/= '}') t' `elem`
      [ "align", "align*"
      , "flalign", "flalign*"
      , "alignat", "alignat*"
      , "dmath", "dmath*"
      , "dgroup", "dgroup*"
      , "darray", "darray*"
      , "gather", "gather*"
      , "multline", "multline*"
      , "subequations"
      , "equation", "equation*"
      , "eqnarray"
      , "displaymath"
      ]
 where
   isSpaceChar '\n' = True
   isSpaceChar '\r' = True
   isSpaceChar '\t' = True
   isSpaceChar ' ' = True
   isSpaceChar _ = False

-- True if the math needs the cancel package
needsCancel :: Text -> Bool
needsCancel t =
  case A.parseOnly pCancel t of
    Right True -> True
    _ -> False
 where
  pCancel = (False <$ A.endOfInput) <|> do
    c <- A.anyChar
    case c of
      '\\' -> do
        x <- A.takeWhile isLetter
        if x == "cancel" || x == "xcancel" || x == "bcancel"
           then return True
           else pCancel
      _ -> pCancel
