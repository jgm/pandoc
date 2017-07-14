{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.LaTeX
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
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
import Control.Applicative ((<|>))
import Control.Monad.State.Strict
import Data.Aeson (FromJSON, object, (.=))
import Data.Char (isAlphaNum, isAscii, isDigit, isLetter, isPunctuation, ord,
                  toLower)
import Data.List (foldl', intercalate, intersperse, isInfixOf, nubBy,
                  stripPrefix, (\\))
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (unEscapeString)
import Text.Pandoc.BCP47 (Lang(..), toLang, getLang, renderLang)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (formatLaTeXBlock, formatLaTeXInline, highlight,
                                 styleToLaTeX, toListingsLanguage)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Slides
import Text.Pandoc.Templates
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared
import qualified Text.Parsec as P
import Text.Printf (printf)

data WriterState =
  WriterState { stInNote        :: Bool          -- true if we're in a note
              , stInQuote       :: Bool          -- true if in a blockquote
              , stInMinipage    :: Bool          -- true if in minipage
              , stInHeading     :: Bool          -- true if in a section heading
              , stInItem        :: Bool          -- true if in \item[..]
              , stNotes         :: [Doc]         -- notes in a minipage
              , stOLLevel       :: Int           -- level of ordered list nesting
              , stOptions       :: WriterOptions -- writer options, so they don't have to be parameter
              , stVerbInNote    :: Bool          -- true if document has verbatim text in note
              , stTable         :: Bool          -- true if document has a table
              , stStrikeout     :: Bool          -- true if document has strikeout
              , stUrl           :: Bool          -- true if document has visible URL link
              , stGraphics      :: Bool          -- true if document contains images
              , stLHS           :: Bool          -- true if document has literate haskell code
              , stBook          :: Bool          -- true if document uses book or memoir class
              , stCsquotes      :: Bool          -- true if document uses csquotes
              , stHighlighting  :: Bool          -- true if document has highlighted code
              , stIncremental   :: Bool          -- true if beamer lists should be displayed bit by bit
              , stInternalLinks :: [String]      -- list of internal link targets
              , stUsesEuro      :: Bool          -- true if euro symbol used
              , stBeamer        :: Bool          -- produce beamer
              , stEmptyLine     :: Bool          -- true if no content on line
              }

startingState :: WriterOptions -> WriterState
startingState options = WriterState {
                  stInNote = False
                , stInQuote = False
                , stInMinipage = False
                , stInHeading = False
                , stInItem = False
                , stNotes = []
                , stOLLevel = 1
                , stOptions = options
                , stVerbInNote = False
                , stTable = False
                , stStrikeout = False
                , stUrl = False
                , stGraphics = False
                , stLHS = False
                , stBook = (case writerTopLevelDivision options of
                                 TopLevelPart    -> True
                                 TopLevelChapter -> True
                                 _               -> False)
                , stCsquotes = False
                , stHighlighting = False
                , stIncremental = writerIncremental options
                , stInternalLinks = []
                , stUsesEuro = False
                , stBeamer = False
                , stEmptyLine = True }

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

type LW m = StateT WriterState m

pandocToLaTeX :: PandocMonad m
              => WriterOptions -> Pandoc -> LW m Text
pandocToLaTeX options (Pandoc meta blocks) = do
  -- Strip off final 'references' header if --natbib or --biblatex
  let method = writerCiteMethod options
  let blocks' = if method == Biblatex || method == Natbib
                   then case reverse blocks of
                             (Div (_,["references"],_) _):xs -> reverse xs
                             _                               -> blocks
                   else blocks
  -- see if there are internal links
  let isInternalLink (Link _ _ ('#':xs,_)) = [xs]
      isInternalLink _                     = []
  modify $ \s -> s{ stInternalLinks = query isInternalLink blocks' }
  let template = maybe "" id $ writerTemplate options
  -- set stBook depending on documentclass
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  let render' :: Doc -> Text
      render' = render colwidth
  metadata <- metaToJSON options
              (fmap render' . blockListToLaTeX)
              (fmap render' . inlineListToLaTeX)
              meta
  let bookClasses = ["memoir","book","report","scrreprt","scrbook"]
  let documentClass = case P.parse pDocumentClass "template" template of
                              Right r -> r
                              Left _  -> ""
  case lookup "documentclass" (writerVariables options) `mplus`
        fmap stringify (lookupMeta "documentclass" meta) of
         Just x  | x `elem` bookClasses -> modify $ \s -> s{stBook = True}
                 | otherwise            -> return ()
         Nothing | documentClass `elem` bookClasses
                                        -> modify $ \s -> s{stBook = True}
                 | otherwise               -> return ()
  -- check for \usepackage...{csquotes}; if present, we'll use
  -- \enquote{...} for smart quotes:
  let headerIncludesField :: FromJSON a => Maybe a
      headerIncludesField = getField "header-includes" metadata
  let headerIncludes = fromMaybe [] $ mplus
                       (fmap return headerIncludesField)
                       headerIncludesField
  when (any (isInfixOf "{csquotes}") (template : headerIncludes)) $
    modify $ \s -> s{stCsquotes = True}
  let (blocks'', lastHeader) = if writerCiteMethod options == Citeproc then
                                 (blocks', [])
                               else case last blocks' of
                                 Header 1 _ il -> (init blocks', il)
                                 _             -> (blocks', [])
  beamer <- gets stBeamer
  blocks''' <- if beamer
                  then toSlides blocks''
                  else return blocks''
  body <- mapM (elementToLaTeX options) $ hierarchicalize blocks'''
  (biblioTitle :: Text) <- render' <$> inlineListToLaTeX lastHeader
  let main = render' $ vsep body
  st <- get
  titleMeta <- stringToLaTeX TextString $ stringify $ docTitle meta
  authorsMeta <- mapM (stringToLaTeX TextString . stringify) $ docAuthors meta
  docLangs <- catMaybes <$>
      mapM (toLang . Just) (ordNub (query (extract "lang") blocks))
  let hasStringValue x = isJust (getField x metadata :: Maybe String)
  let geometryFromMargins = intercalate [','] $ catMaybes $
                              map (\(x,y) ->
                                ((x ++ "=") ++) <$> getField y metadata)
                              [("lmargin","margin-left")
                              ,("rmargin","margin-right")
                              ,("tmargin","margin-top")
                              ,("bmargin","margin-bottom")
                              ]
  let toPolyObj lang = object [ "name"    .= T.pack name
                              , "options" .= T.pack opts ]
        where
          (name, opts) = toPolyglossia lang
  mblang <- toLang $ case getLang options meta of
                          Just l -> Just l
                          Nothing | null docLangs -> Nothing
                                  | otherwise     -> Just "en"
  -- we need a default here since lang is used in template conditionals

  let dirs = query (extract "dir") blocks

  let context  =  defField "toc" (writerTableOfContents options) $
                  defField "toc-depth" (show (writerTOCDepth options -
                                              if stBook st
                                                 then 1
                                                 else 0)) $
                  defField "body" main $
                  defField "title-meta" titleMeta $
                  defField "author-meta" (intercalate "; " authorsMeta) $
                  defField "documentclass" (if beamer
                                               then ("beamer" :: String)
                                               else if stBook st
                                                    then "book"
                                                    else "article") $
                  defField "verbatim-in-note" (stVerbInNote st) $
                  defField "tables" (stTable st) $
                  defField "strikeout" (stStrikeout st) $
                  defField "url" (stUrl st) $
                  defField "numbersections" (writerNumberSections options) $
                  defField "lhs" (stLHS st) $
                  defField "graphics" (stGraphics st) $
                  defField "book-class" (stBook st) $
                  defField "euro" (stUsesEuro st) $
                  defField "listings" (writerListings options || stLHS st) $
                  defField "beamer" beamer $
                  (if stHighlighting st
                      then case writerHighlightStyle options of
                                Just sty ->
                                   defField "highlighting-macros"
                                      (styleToLaTeX sty)
                                Nothing -> id
                      else id) $
                  (case writerCiteMethod options of
                         Natbib   -> defField "biblio-title" biblioTitle .
                                     defField "natbib" True
                         Biblatex -> defField "biblio-title" biblioTitle .
                                     defField "biblatex" True
                         _        -> id) $
                  defField "colorlinks" (any hasStringValue
                           ["citecolor", "urlcolor", "linkcolor", "toccolor"]) $
                  (if null dirs
                     then id
                     else defField "dir" ("ltr" :: String)) $
                  defField "section-titles" True $
                  defField "geometry" geometryFromMargins $
                  (case getField "papersize" metadata of
                        Just ("A4" :: String) -> resetField "papersize"
                                                    ("a4" :: String)
                        _                     -> id) $
                  metadata
  let context' =
          -- note: lang is used in some conditionals in the template,
          -- so we need to set it if we have any babel/polyglossia:
          maybe id (defField "lang" . renderLang) mblang
        $ maybe id (defField "babel-lang" . toBabel) mblang
        $ defField "babel-otherlangs" (map toBabel docLangs)
        $ defField "babel-newcommands" (concatMap (\(poly, babel) ->
            -- \textspanish and \textgalician are already used by babel
            -- save them as \oritext... and let babel use that
            if poly `elem` ["spanish", "galician"]
               then "\\let\\oritext" ++ poly ++ "\\text" ++ poly ++ "\n" ++
                    "\\AddBabelHook{" ++ poly ++ "}{beforeextras}" ++
                      "{\\renewcommand{\\text" ++ poly ++ "}{\\oritext"
                      ++ poly ++ "}}\n" ++
                    "\\AddBabelHook{" ++ poly ++ "}{afterextras}" ++
                      "{\\renewcommand{\\text" ++ poly ++ "}[2][]{\\foreignlanguage{"
                      ++ poly ++ "}{##2}}}\n"
               else "\\newcommand{\\text" ++ poly ++ "}[2][]{\\foreignlanguage{"
                      ++ babel ++ "}{#2}}\n" ++
                    "\\newenvironment{" ++ poly ++ "}[2][]{\\begin{otherlanguage}{"
                      ++ babel ++ "}}{\\end{otherlanguage}}\n"
            )
            -- eliminate duplicates that have same polyglossia name
            $ nubBy (\a b -> fst a == fst b)
            -- find polyglossia and babel names of languages used in the document
            $ map (\l -> (fst $ toPolyglossia l, toBabel l)) docLangs
          )
        $ maybe id (defField "polyglossia-lang" . toPolyObj) mblang
        $ defField "polyglossia-otherlangs" (map toPolyObj docLangs)
        $ defField "latex-dir-rtl"
           (getField "dir" context == Just ("rtl" :: String))
        $ context
  case writerTemplate options of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context'

-- | Convert Elements to LaTeX
elementToLaTeX :: PandocMonad m => WriterOptions -> Element -> LW m Doc
elementToLaTeX _ (Blk block) = blockToLaTeX block
elementToLaTeX opts (Sec level _ (id',classes,_) title' elements) = do
  modify $ \s -> s{stInHeading = True}
  header' <- sectionHeader ("unnumbered" `elem` classes) id' level title'
  modify $ \s -> s{stInHeading = False}
  innerContents <- mapM (elementToLaTeX opts) elements
  return $ vsep (header' : innerContents)

data StringContext = TextString
                   | URLString
                   | CodeString
                   deriving (Eq)

-- escape things as needed for LaTeX
stringToLaTeX :: PandocMonad m => StringContext -> String -> LW m String
stringToLaTeX  _     []     = return ""
stringToLaTeX  ctx (x:xs) = do
  opts <- gets stOptions
  rest <- stringToLaTeX ctx xs
  let ligatures = isEnabled Ext_smart opts && ctx == TextString
  let isUrl = ctx == URLString
  when (x == '€') $
     modify $ \st -> st{ stUsesEuro = True }
  return $
    case x of
       '€' -> "\\euro{}" ++ rest
       '{' -> "\\{" ++ rest
       '}' -> "\\}" ++ rest
       '`' | ctx == CodeString -> "\\textasciigrave{}" ++ rest
       '$' | not isUrl -> "\\$" ++ rest
       '%' -> "\\%" ++ rest
       '&' -> "\\&" ++ rest
       '_' | not isUrl -> "\\_" ++ rest
       '#' -> "\\#" ++ rest
       '-' | not isUrl -> case xs of
                   -- prevent adjacent hyphens from forming ligatures
                   ('-':_) -> "-\\/" ++ rest
                   _       -> '-' : rest
       '~' | not isUrl -> "\\textasciitilde{}" ++ rest
       '^' -> "\\^{}" ++ rest
       '\\'| isUrl     -> '/' : rest  -- NB. / works as path sep even on Windows
           | otherwise -> "\\textbackslash{}" ++ rest
       '|' | not isUrl -> "\\textbar{}" ++ rest
       '<' -> "\\textless{}" ++ rest
       '>' -> "\\textgreater{}" ++ rest
       '[' -> "{[}" ++ rest  -- to avoid interpretation as
       ']' -> "{]}" ++ rest  -- optional arguments
       '\'' | ctx == CodeString -> "\\textquotesingle{}" ++ rest
       '\160' -> "~" ++ rest
       '\x202F' -> "\\," ++ rest
       '\x2026' -> "\\ldots{}" ++ rest
       '\x2018' | ligatures -> "`" ++ rest
       '\x2019' | ligatures -> "'" ++ rest
       '\x201C' | ligatures -> "``" ++ rest
       '\x201D' | ligatures -> "''" ++ rest
       '\x2014' | ligatures -> "---" ++ rest
       '\x2013' | ligatures -> "--" ++ rest
       _        -> x : rest

toLabel :: PandocMonad m => String -> LW m String
toLabel z = go `fmap` stringToLaTeX URLString z
 where go [] = ""
       go (x:xs)
         | (isLetter x || isDigit x) && isAscii x = x:go xs
         | elem x ("_-+=:;." :: String) = x:go xs
         | otherwise = "ux" ++ printf "%x" (ord x) ++ go xs

-- | Puts contents into LaTeX command.
inCmd :: String -> Doc -> Doc
inCmd cmd contents = char '\\' <> text cmd <> braces contents

toSlides :: PandocMonad m => [Block] -> LW m [Block]
toSlides bs = do
  opts <- gets stOptions
  let slideLevel = fromMaybe (getSlideLevel bs) $ writerSlideLevel opts
  let bs' = prepSlides slideLevel bs
  concat `fmap` (mapM (elementToBeamer slideLevel) $ hierarchicalize bs')

elementToBeamer :: PandocMonad m => Int -> Element -> LW m [Block]
elementToBeamer _slideLevel (Blk b) = return [b]
elementToBeamer slideLevel  (Sec lvl _num (ident,classes,kvs) tit elts)
  | lvl >  slideLevel = do
      bs <- concat `fmap` mapM (elementToBeamer slideLevel) elts
      return $ Para ( RawInline "latex" "\\begin{block}{"
                    : tit ++ [RawInline "latex" "}"] )
             : bs ++ [RawBlock "latex" "\\end{block}"]
  | lvl <  slideLevel = do
      bs <- concat `fmap` mapM (elementToBeamer slideLevel) elts
      return $ (Header lvl (ident,classes,kvs) tit) : bs
  | otherwise = do -- lvl == slideLevel
      -- note: [fragile] is required or verbatim breaks
      let hasCodeBlock (CodeBlock _ _) = [True]
          hasCodeBlock _               = []
      let hasCode (Code _ _) = [True]
          hasCode _          = []
      let fragile = "fragile" `elem` classes ||
                    not (null $ query hasCodeBlock elts ++ query hasCode elts)
      let frameoptions = ["allowdisplaybreaks", "allowframebreaks",
                          "b", "c", "t", "environment",
                          "label", "plain", "shrink", "standout"]
      let optionslist = ["fragile" | fragile] ++
                        [k | k <- classes, k `elem` frameoptions] ++
                        [k ++ "=" ++ v | (k,v) <- kvs, k `elem` frameoptions]
      let options = if null optionslist
                       then ""
                       else "[" ++ intercalate "," optionslist ++ "]"
      let latex = RawInline (Format "latex")
      slideTitle <-
            if tit == [Str "\0"] -- marker for hrule
               then return []
               else
                 if null ident
                    then return $ latex "{" : tit ++ [latex "}"]
                    else do
                      ref <- toLabel ident
                      return $ latex ("{%\n\\protect\\hypertarget{" ++
                                ref ++ "}{%\n") : tit ++ [latex "}}"]
      let slideStart = Para $
              RawInline "latex" ("\\begin{frame}" ++ options) : slideTitle
      let slideEnd = RawBlock "latex" "\\end{frame}"
      -- now carve up slide into blocks if there are sections inside
      bs <- concat `fmap` mapM (elementToBeamer slideLevel) elts
      return $ slideStart : bs ++ [slideEnd]

isListBlock :: Block -> Bool
isListBlock (BulletList _)     = True
isListBlock (OrderedList _ _)  = True
isListBlock (DefinitionList _) = True
isListBlock _                  = False

isLineBreakOrSpace :: Inline -> Bool
isLineBreakOrSpace LineBreak = True
isLineBreakOrSpace SoftBreak = True
isLineBreakOrSpace Space     = True
isLineBreakOrSpace _         = False

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: PandocMonad m
             => Block     -- ^ Block to convert
             -> LW m Doc
blockToLaTeX Null = return empty
blockToLaTeX (Div (identifier,classes,kvs) bs) = do
  beamer <- gets stBeamer
  linkAnchor' <- hypertarget True identifier empty
  -- see #2704 for the motivation for adding \leavevmode:
  let linkAnchor =
       case bs of
            Para _ : _
              | not (isEmpty linkAnchor')
              -> "\\leavevmode" <> linkAnchor' <> "%"
            _ -> linkAnchor'
  let align dir txt = inCmd "begin" dir $$ txt $$ inCmd "end" dir
  lang <- toLang $ lookup "lang" kvs
  let wrapDir = case lookup "dir" kvs of
                  Just "rtl" -> align "RTL"
                  Just "ltr" -> align "LTR"
                  _          -> id
      wrapLang txt = case lang of
                       Just lng -> let (l, o) = toPolyglossiaEnv lng
                                       ops = if null o
                                                then ""
                                                else brackets $ text o
                                   in  inCmd "begin" (text l) <> ops
                                       $$ blankline <> txt <> blankline
                                       $$ inCmd "end" (text l)
                       Nothing  -> txt
      wrapNotes txt = if beamer && "notes" `elem` classes
                          then "\\note" <> braces txt -- speaker notes
                          else linkAnchor $$ txt
  fmap (wrapDir . wrapLang . wrapNotes) $ blockListToLaTeX bs
blockToLaTeX (Plain lst) =
  inlineListToLaTeX $ dropWhile isLineBreakOrSpace lst
-- title beginning with fig: indicates that the image is a figure
blockToLaTeX (Para [Image attr@(ident, _, _) txt (src,'f':'i':'g':':':tit)]) = do
  inNote <- gets stInNote
  modify $ \st -> st{ stInMinipage = True, stNotes = [] }
  capt <- inlineListToLaTeX txt
  notes <- gets stNotes
  modify $ \st -> st{ stInMinipage = False, stNotes = [] }

  -- We can't have footnotes in the list of figures, so remove them:
  captForLof <- if null notes
                   then return empty
                   else brackets <$> inlineListToLaTeX (walk deNote txt)
  img <- inlineToLaTeX (Image attr txt (src,tit))
  let footnotes = notesToLaTeX notes
  lab <- labelFor ident
  let caption = "\\caption" <> captForLof <> braces capt <> lab
  let figure = cr <> "\\begin{figure}" $$ "\\centering" $$ img $$
              caption $$ "\\end{figure}" <> cr
  figure' <- hypertarget True ident figure
  return $ if inNote
              -- can't have figures in notes
              then "\\begin{center}" $$ img $+$ capt $$ "\\end{center}"
              else figure' $$ footnotes
-- . . . indicates pause in beamer slides
blockToLaTeX (Para [Str ".",Space,Str ".",Space,Str "."]) = do
  beamer <- gets stBeamer
  if beamer
     then blockToLaTeX (RawBlock "latex" "\\pause")
     else inlineListToLaTeX [Str ".",Space,Str ".",Space,Str "."]
blockToLaTeX (Para lst) =
  inlineListToLaTeX $ dropWhile isLineBreakOrSpace lst
blockToLaTeX (LineBlock lns) = do
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
        return $ flush (linkAnchor $$ "\\begin{code}" $$ text str $$
                            "\\end{code}") $$ cr
  let rawCodeBlock = do
        st <- get
        env <- if stInNote st
                  then modify (\s -> s{ stVerbInNote = True }) >>
                       return "Verbatim"
                  else return "verbatim"
        return $ flush (linkAnchor $$ text ("\\begin{" ++ env ++ "}") $$
                 text str $$ text ("\\end{" ++ env ++ "}")) <> cr
  let listingsCodeBlock = do
        st <- get
        ref <- toLabel identifier
        let params = if writerListings (stOptions st)
                     then (case getListingsLanguage classes of
                                Just l  -> [ "language=" ++ mbBraced l ]
                                Nothing -> []) ++
                          [ "numbers=left" | "numberLines" `elem` classes
                             || "number" `elem` classes
                             || "number-lines" `elem` classes ] ++
                          [ (if key == "startFrom"
                                then "firstnumber"
                                else key) ++ "=" ++ mbBraced attr |
                                (key,attr) <- keyvalAttr ] ++
                          (if identifier == ""
                                then []
                                else [ "label=" ++ ref ])

                     else []
            printParams
                | null params = empty
                | otherwise   = brackets $ hcat (intersperse ", "
                      (map text params))
        return $ flush ("\\begin{lstlisting}" <> printParams $$ text str $$
                 "\\end{lstlisting}") $$ cr
  let highlightedCodeBlock =
        case highlight (writerSyntaxMap opts)
                 formatLaTeXBlock ("",classes,keyvalAttr) str of
               Left msg -> do
                 unless (null msg) $
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
blockToLaTeX b@(RawBlock f x)
  | f == Format "latex" || f == Format "tex"
                        = return $ text x
  | otherwise           = do
      report $ BlockNotRendered b
      return empty
blockToLaTeX (BulletList []) = return empty  -- otherwise latex error
blockToLaTeX (BulletList lst) = do
  incremental <- gets stIncremental
  beamer <- gets stBeamer
  let inc = if beamer && incremental then "[<+->]" else ""
  items <- mapM listItemToLaTeX lst
  let spacing = if isTightList lst
                   then text "\\tightlist"
                   else empty
  return $ text ("\\begin{itemize}" ++ inc) $$ spacing $$ vcat items $$
             "\\end{itemize}"
blockToLaTeX (OrderedList _ []) = return empty -- otherwise latex error
blockToLaTeX (OrderedList (start, numstyle, numdelim) lst) = do
  st <- get
  let inc = if stIncremental st then "[<+->]" else ""
  let oldlevel = stOLLevel st
  put $ st {stOLLevel = oldlevel + 1}
  items <- mapM listItemToLaTeX lst
  modify (\s -> s {stOLLevel = oldlevel})
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
  let enum = text $ "enum" ++ map toLower (toRomanNumeral oldlevel)
  let stylecommand = if numstyle == DefaultStyle && numdelim == DefaultDelim
                        then empty
                        else "\\def" <> "\\label" <> enum <>
                              braces (todelim $ tostyle enum)
  let resetcounter = if start == 1 || oldlevel > 4
                        then empty
                        else "\\setcounter" <> braces enum <>
                              braces (text $ show $ start - 1)
  let spacing = if isTightList lst
                   then text "\\tightlist"
                   else empty
  return $ text ("\\begin{enumerate}" ++ inc)
         $$ stylecommand
         $$ resetcounter
         $$ spacing
         $$ vcat items
         $$ "\\end{enumerate}"
blockToLaTeX (DefinitionList []) = return empty
blockToLaTeX (DefinitionList lst) = do
  incremental <- gets stIncremental
  let inc = if incremental then "[<+->]" else ""
  items <- mapM defListItemToLaTeX lst
  let spacing = if all isTightList (map snd lst)
                   then text "\\tightlist"
                   else empty
  return $ text ("\\begin{description}" ++ inc) $$ spacing $$ vcat items $$
               "\\end{description}"
blockToLaTeX HorizontalRule = return $
  "\\begin{center}\\rule{0.5\\linewidth}{\\linethickness}\\end{center}"
blockToLaTeX (Header level (id',classes,_) lst) = do
  modify $ \s -> s{stInHeading = True}
  hdr <- sectionHeader ("unnumbered" `elem` classes) id' level lst
  modify $ \s -> s{stInHeading = False}
  return hdr
blockToLaTeX (Table caption aligns widths heads rows) = do
  let toHeaders hs = do contents <- (tableRowToLaTeX True aligns widths) hs
                        return ("\\toprule" $$ contents $$ "\\midrule")
  let removeNote (Note _) = Span ("", [], []) []
      removeNote x        = x
  captionText <- inlineListToLaTeX caption
  firsthead <- if isEmpty captionText || all null heads
                  then return empty
                  else ($$ text "\\endfirsthead") <$> toHeaders heads
  head' <- if all null heads
              then return empty
              -- avoid duplicate notes in head and firsthead:
              else ($$ text "\\endhead") <$>
                   toHeaders (if isEmpty firsthead
                                 then heads
                                 else walk removeNote heads)
  let capt = if isEmpty captionText
                then empty
                else text "\\caption" <>
                      braces captionText <> "\\tabularnewline"
  rows' <- mapM (tableRowToLaTeX False aligns widths) rows
  let colDescriptors = text $ concat $ map toColDescriptor aligns
  modify $ \s -> s{ stTable = True }
  return $ "\\begin{longtable}[]" <>
              braces ("@{}" <> colDescriptors <> "@{}")
              -- the @{} removes extra space at beginning and end
         $$ capt
         $$ firsthead
         $$ (if all null heads then "\\toprule" else empty)
         $$ head'
         $$ vcat rows'
         $$ "\\bottomrule"
         $$ "\\end{longtable}"

toColDescriptor :: Alignment -> String
toColDescriptor align =
  case align of
         AlignLeft    -> "l"
         AlignRight   -> "r"
         AlignCenter  -> "c"
         AlignDefault -> "l"

blockListToLaTeX :: PandocMonad m => [Block] -> LW m Doc
blockListToLaTeX lst =
  vsep `fmap` mapM (\b -> setEmptyLine True >> blockToLaTeX b) lst

tableRowToLaTeX :: PandocMonad m
                => Bool
                -> [Alignment]
                -> [Double]
                -> [[Block]]
                -> LW m Doc
tableRowToLaTeX header aligns widths cols = do
  -- scale factor compensates for extra space between columns
  -- so the whole table isn't larger than columnwidth
  let scaleFactor = 0.97 ** fromIntegral (length aligns)
  let isSimple [Plain _] = True
      isSimple [Para  _] = True
      isSimple []        = True
      isSimple _         = False
  -- simple tables have to have simple cells:
  let widths' = if not (all isSimple cols)
                   then replicate (length aligns)
                          (0.97 / fromIntegral (length aligns))
                   else map (scaleFactor *) widths
  cells <- mapM (tableCellToLaTeX header) $ zip3 widths' aligns cols
  return $ hsep (intersperse "&" cells) <> "\\tabularnewline"

-- For simple latex tables (without minipages or parboxes),
-- we need to go to some lengths to get line breaks working:
-- as LineBreak bs = \vtop{\hbox{\strut as}\hbox{\strut bs}}.
fixLineBreaks :: Block -> Block
fixLineBreaks (Para ils)  = Para $ fixLineBreaks' ils
fixLineBreaks (Plain ils) = Plain $ fixLineBreaks' ils
fixLineBreaks x           = x

fixLineBreaks' :: [Inline] -> [Inline]
fixLineBreaks' ils = case splitBy (== LineBreak) ils of
                       []     -> []
                       [xs]   -> xs
                       chunks -> RawInline "tex" "\\vtop{" :
                                 concatMap tohbox chunks ++
                                 [RawInline "tex" "}"]
  where tohbox ys = RawInline "tex" "\\hbox{\\strut " : ys ++
                    [RawInline "tex" "}"]

-- We also change display math to inline math, since display
-- math breaks in simple tables.
displayMathToInline :: Inline -> Inline
displayMathToInline (Math DisplayMath x) = Math InlineMath x
displayMathToInline x                    = x

tableCellToLaTeX :: PandocMonad m => Bool -> (Double, Alignment, [Block])
                 -> LW m Doc
tableCellToLaTeX _      (0,     _,     blocks) =
  blockListToLaTeX $ walk fixLineBreaks $ walk displayMathToInline blocks
tableCellToLaTeX header (width, align, blocks) = do
  modify $ \st -> st{ stInMinipage = True, stNotes = [] }
  cellContents <- blockListToLaTeX blocks
  notes <- gets stNotes
  modify $ \st -> st{ stInMinipage = False, stNotes = [] }
  let valign = text $ if header then "[b]" else "[t]"
  let halign = case align of
               AlignLeft    -> "\\raggedright"
               AlignRight   -> "\\raggedleft"
               AlignCenter  -> "\\centering"
               AlignDefault -> "\\raggedright"
  return $ ("\\begin{minipage}" <> valign <>
            braces (text (printf "%.2f\\columnwidth" width)) <>
            (halign <> cr <> cellContents <> "\\strut" <> cr) <>
            "\\end{minipage}") $$
            notesToLaTeX notes

notesToLaTeX :: [Doc] -> Doc
notesToLaTeX [] = empty
notesToLaTeX ns = (case length ns of
                              n | n > 1 -> "\\addtocounter" <>
                                           braces "footnote" <>
                                           braces (text $ show $ 1 - n)
                                | otherwise -> empty)
                   $$
                   vcat (intersperse
                     ("\\addtocounter" <> braces "footnote" <> braces "1")
                     $ map (\x -> "\\footnotetext" <> braces x)
                     $ reverse ns)

listItemToLaTeX :: PandocMonad m => [Block] -> LW m Doc
listItemToLaTeX lst
  -- we need to put some text before a header if it's the first
  -- element in an item. This will look ugly in LaTeX regardless, but
  -- this will keep the typesetter from throwing an error.
  | ((Header _ _ _) :_) <- lst =
    blockListToLaTeX lst >>= return . (text "\\item ~" $$) . (nest 2)
  | otherwise = blockListToLaTeX lst >>= return .  (text "\\item" $$) .
                      (nest 2)

defListItemToLaTeX :: PandocMonad m => ([Inline], [[Block]]) -> LW m Doc
defListItemToLaTeX (term, defs) = do
    -- needed to turn off 'listings' because it breaks inside \item[...]:
    modify $ \s -> s{stInItem = True}
    term' <- inlineListToLaTeX term
    modify $ \s -> s{stInItem = False}
    -- put braces around term if it contains an internal link,
    -- since otherwise we get bad bracket interactions: \item[\hyperref[..]
    let isInternalLink (Link _ _ ('#':_,_)) = True
        isInternalLink _                    = False
    let term'' = if any isInternalLink term
                    then braces term'
                    else term'
    def'  <- liftM vsep $ mapM blockListToLaTeX defs
    return $ case defs of
     (((Header _ _ _) : _) : _) ->
       "\\item" <> brackets term'' <> " ~ " $$ def'
     _                          ->
       "\\item" <> brackets term'' $$ def'

-- | Craft the section header, inserting the secton reference, if supplied.
sectionHeader :: PandocMonad m
              => Bool    -- True for unnumbered
              -> [Char]
              -> Int
              -> [Inline]
              -> LW m Doc
sectionHeader unnumbered ident level lst = do
  txt <- inlineListToLaTeX lst
  plain <- stringToLaTeX TextString $ concatMap stringify lst
  let removeInvalidInline (Note _)             = []
      removeInvalidInline (Span (id', _, _) _) | not (null id') = []
      removeInvalidInline (Image _ _ _)        = []
      removeInvalidInline x                    = [x]
  let lstNoNotes = foldr (mappend . (\x -> walkM removeInvalidInline x)) mempty lst
  txtNoNotes <- inlineListToLaTeX lstNoNotes
  -- footnotes in sections don't work (except for starred variants)
  -- unless you specify an optional argument:
  -- \section[mysec]{mysec\footnote{blah}}
  optional <- if unnumbered || lstNoNotes == lst || lstNoNotes == []
                 then return empty
                 else do
                   return $ brackets txtNoNotes
  let contents = if render Nothing txt == plain
                    then braces txt
                    else braces (text "\\texorpdfstring"
                         <> braces txt
                         <> braces (text plain))
  book <- gets stBook
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
  let star = if unnumbered && level' < 4 then text "*" else empty
  let stuffing = star <> optional <> contents
  stuffing' <- hypertarget True ident $
                  text ('\\':sectionType) <> stuffing <> lab
  return $ if level' > 5
              then txt
              else prefix $$ stuffing'
                   $$ if unnumbered
                         then "\\addcontentsline{toc}" <>
                                braces (text sectionType) <>
                                braces txtNoNotes
                         else empty

hypertarget :: PandocMonad m => Bool -> String -> Doc -> LW m Doc
hypertarget _ "" x    = return x
hypertarget addnewline ident x = do
  ref <- text `fmap` toLabel ident
  return $ text "\\hypertarget"
              <> braces ref
              <> braces ((if addnewline && not (isEmpty x)
                             then ("%" <> cr)
                             else empty) <> x)

labelFor :: PandocMonad m => String -> LW m Doc
labelFor ""    = return empty
labelFor ident = do
  ref <- text `fmap` toLabel ident
  return $ text "\\label" <> braces ref

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: PandocMonad m
                  => [Inline]  -- ^ Inlines to convert
                  -> LW m Doc
inlineListToLaTeX lst =
  mapM inlineToLaTeX (fixLineInitialSpaces lst)
    >>= return . hcat
    -- nonbreaking spaces (~) in LaTeX don't work after line breaks,
    -- so we turn nbsps after hard breaks to \hspace commands.
    -- this is mostly used in verse.
 where fixLineInitialSpaces [] = []
       fixLineInitialSpaces (LineBreak : Str s@('\160':_) : xs) =
         LineBreak : fixNbsps s ++ fixLineInitialSpaces xs
       fixLineInitialSpaces (x:xs) = x : fixLineInitialSpaces xs
       fixNbsps s = let (ys,zs) = span (=='\160') s
                    in  replicate (length ys) hspace ++ [Str zs]
       hspace = RawInline "latex" "\\hspace*{0.333em}"

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted _            = False

-- | Convert inline element to LaTeX
inlineToLaTeX :: PandocMonad m
              => Inline    -- ^ Inline to convert
              -> LW m Doc
inlineToLaTeX (Span (id',classes,kvs) ils) = do
  linkAnchor <- hypertarget False id' empty
  lang <- toLang $ lookup "lang" kvs
  let cmds = ["textup" | "csl-no-emph" `elem` classes] ++
             ["textnormal" | "csl-no-strong" `elem` classes ||
                             "csl-no-smallcaps" `elem` classes] ++
             ["RL" | ("dir", "rtl") `elem` kvs] ++
             ["LR" | ("dir", "ltr") `elem` kvs] ++
             (case lang of
                Just lng -> let (l, o) = toPolyglossia lng
                                ops = if null o then "" else ("[" ++ o ++ "]")
                            in  ["text" ++ l ++ ops]
                Nothing  -> [])
  contents <- inlineListToLaTeX ils
  return $ (if null id'
               then empty
               else "\\protect" <> linkAnchor) <>
           (if null cmds
               then braces contents
               else foldr inCmd contents cmds)
inlineToLaTeX (Emph lst) =
  inlineListToLaTeX lst >>= return . inCmd "emph"
inlineToLaTeX (Strong lst) =
  inlineListToLaTeX lst >>= return . inCmd "textbf"
inlineToLaTeX (Strikeout lst) = do
  -- we need to protect VERB in an mbox or we get an error
  -- see #1294
  contents <- inlineListToLaTeX $ protectCode lst
  modify $ \s -> s{ stStrikeout = True }
  return $ inCmd "sout" contents
inlineToLaTeX (Superscript lst) =
  inlineListToLaTeX lst >>= return . inCmd "textsuperscript"
inlineToLaTeX (Subscript lst) = do
  inlineListToLaTeX lst >>= return . inCmd "textsubscript"
inlineToLaTeX (SmallCaps lst) =
  inlineListToLaTeX lst >>= return . inCmd "textsc"
inlineToLaTeX (Cite cits lst) = do
  st <- get
  let opts = stOptions st
  case writerCiteMethod opts of
     Natbib   -> citationsToNatbib cits
     Biblatex -> citationsToBiblatex cits
     _        -> inlineListToLaTeX lst

inlineToLaTeX (Code (_,classes,_) str) = do
  opts <- gets stOptions
  inHeading <- gets stInHeading
  inItem <- gets stInItem
  let listingsCode = do
        let listingsopt = case getListingsLanguage classes of
                               Just l -> "[language=" ++ mbBraced l ++ "]"
                               Nothing -> ""
        inNote <- gets stInNote
        when inNote $ modify $ \s -> s{ stVerbInNote = True }
        let chr = case "!\"&'()*,-./:;?@_" \\ str of
                       (c:_) -> c
                       []    -> '!'
        let str' = escapeStringUsing (backslashEscapes "\\{}%") str
        -- we always put lstinline in a dummy 'passthrough' command
        -- (defined in the default template) so that we don't have
        -- to change the way we escape characters depending on whether
        -- the lstinline is inside another command.  See #1629:
        return $ text $ "\\passthrough{\\lstinline" ++ listingsopt ++ [chr] ++ str' ++ [chr] ++ "}"
  let rawCode = liftM (text . (\s -> "\\texttt{" ++ escapeSpaces s ++ "}"))
                 $ stringToLaTeX CodeString str
                where escapeSpaces =  concatMap
                         (\c -> if c == ' ' then "\\ " else [c])
  let highlightCode = do
        case highlight (writerSyntaxMap opts)
                 formatLaTeXInline ("",classes,[]) str of
               Left msg -> do
                 unless (null msg) $ report $ CouldNotHighlight msg
                 rawCode
               Right h -> modify (\st -> st{ stHighlighting = True }) >>
                          return (text (T.unpack h))
  case () of
     _ | writerListings opts  && not (inHeading || inItem) -> listingsCode
       | isJust (writerHighlightStyle opts) && not (null classes)
                                                    -> highlightCode
       | otherwise                                  -> rawCode
inlineToLaTeX (Quoted qt lst) = do
  contents <- inlineListToLaTeX lst
  csquotes <- liftM stCsquotes get
  opts <- gets stOptions
  if csquotes
     then return $ "\\enquote" <> braces contents
     else do
       let s1 = if (not (null lst)) && (isQuoted (head lst))
                   then "\\,"
                   else empty
       let s2 = if (not (null lst)) && (isQuoted (last lst))
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
inlineToLaTeX (Str str) = do
  setEmptyLine False
  liftM text $ stringToLaTeX TextString str
inlineToLaTeX (Math InlineMath str) = do
  setEmptyLine False
  return $ "\\(" <> text str <> "\\)"
inlineToLaTeX (Math DisplayMath str) = do
  setEmptyLine False
  return $ "\\[" <> text str <> "\\]"
inlineToLaTeX il@(RawInline f str)
  | f == Format "latex" || f == Format "tex"
                        = do
      setEmptyLine False
      return $ text str
  | otherwise           = do
      report $ InlineNotRendered il
      return empty
inlineToLaTeX (LineBreak) = do
  emptyLine <- gets stEmptyLine
  setEmptyLine True
  return $ (if emptyLine then "~" else "") <> "\\\\" <> cr
inlineToLaTeX SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapAuto     -> return space
       WrapNone     -> return space
       WrapPreserve -> return cr
inlineToLaTeX Space = return space
inlineToLaTeX (Link _ txt ('#':ident, _)) = do
  contents <- inlineListToLaTeX txt
  lab <- toLabel ident
  return $ text "\\protect\\hyperlink" <> braces (text lab) <> braces contents
inlineToLaTeX (Link _ txt (src, _)) =
  case txt of
        [Str x] | escapeURI x == src ->  -- autolink
             do modify $ \s -> s{ stUrl = True }
                src' <- stringToLaTeX URLString (escapeURI src)
                return $ text $ "\\url{" ++ src' ++ "}"
        [Str x] | Just rest <- stripPrefix "mailto:" src,
                  escapeURI x == rest -> -- email autolink
             do modify $ \s -> s{ stUrl = True }
                src' <- stringToLaTeX URLString (escapeURI src)
                contents <- inlineListToLaTeX txt
                return $ "\\href" <> braces (text src') <>
                   braces ("\\nolinkurl" <> braces contents)
        _ -> do contents <- inlineListToLaTeX txt
                src' <- stringToLaTeX URLString (escapeURI src)
                return $ text ("\\href{" ++ src' ++ "}{") <>
                         contents <> char '}'
inlineToLaTeX il@(Image _ _ ('d':'a':'t':'a':':':_, _)) = do
  report $ InlineNotRendered il
  return empty
inlineToLaTeX (Image attr _ (source, _)) = do
  setEmptyLine False
  modify $ \s -> s{ stGraphics = True }
  opts <- gets stOptions
  let showDim dir = let d = text (show dir) <> "="
                    in case (dimension dir attr) of
                         Just (Pixel a)   ->
                           [d <> text (showInInch opts (Pixel a)) <> "in"]
                         Just (Percent a) ->
                           [d <> text (showFl (a / 100)) <> "\\textwidth"]
                         Just dim         ->
                           [d <> text (show dim)]
                         Nothing          ->
                           []
      dimList = showDim Width ++ showDim Height
      dims = if null dimList
                then empty
                else brackets $ cat (intersperse "," dimList)
      source' = if isURI source
                   then source
                   else unEscapeString source
  source'' <- stringToLaTeX URLString source'
  inHeading <- gets stInHeading
  return $
    (if inHeading then "\\protect\\includegraphics" else "\\includegraphics") <>
    dims <> braces (text source'')
inlineToLaTeX (Note contents) = do
  setEmptyLine False
  inMinipage <- gets stInMinipage
  modify (\s -> s{stInNote = True})
  contents' <- blockListToLaTeX contents
  modify (\s -> s {stInNote = False})
  let optnl = case reverse contents of
                   (CodeBlock _ _ : _) -> cr
                   _                   -> empty
  let noteContents = nest 2 contents' <> optnl
  beamer <- gets stBeamer
  -- in beamer slides, display footnote from current overlay forward
  let beamerMark = if beamer
                      then text "<.->"
                      else empty
  modify $ \st -> st{ stNotes = noteContents : stNotes st }
  return $
    if inMinipage
       then "\\footnotemark{}"
       -- note: a \n before } needed when note ends with a Verbatim environment
       else "\\footnote" <> beamerMark <> braces noteContents

protectCode :: [Inline] -> [Inline]
protectCode [] = []
protectCode (x@(Code ("",[],[]) _) : xs) = x : protectCode xs
protectCode (x@(Code _ _) : xs) = ltx "\\mbox{" : x : ltx "}" : xs
  where ltx = RawInline (Format "latex")
protectCode (x : xs) = x : protectCode xs

setEmptyLine :: PandocMonad m => Bool -> LW m ()
setEmptyLine b = modify $ \st -> st{ stEmptyLine = b }

citationsToNatbib :: PandocMonad m => [Citation] -> LW m Doc
citationsToNatbib (one:[])
  = citeCommand c p s k
  where
    Citation { citationId = k
             , citationPrefix = p
             , citationSuffix = s
             , citationMode = m
             }
      = one
    c = case m of
             AuthorInText   -> "citet"
             SuppressAuthor -> "citeyearpar"
             NormalCitation -> "citep"

citationsToNatbib cits
  | noPrefix (tail cits) && noSuffix (init cits) && ismode NormalCitation cits
  = citeCommand "citep" p s ks
  where
     noPrefix  = all (null . citationPrefix)
     noSuffix  = all (null . citationSuffix)
     ismode m  = all (((==) m)  . citationMode)
     p         = citationPrefix  $ head $ cits
     s         = citationSuffix  $ last $ cits
     ks        = intercalate ", " $ map citationId cits

citationsToNatbib (c:cs) | citationMode c == AuthorInText = do
     author <- citeCommand "citeauthor" [] [] (citationId c)
     cits   <- citationsToNatbib (c { citationMode = SuppressAuthor } : cs)
     return $ author <+> cits

citationsToNatbib cits = do
  cits' <- mapM convertOne cits
  return $ text "\\citetext{" <> foldl' combineTwo empty cits' <> text "}"
  where
    combineTwo a b | isEmpty a = b
                   | otherwise = a <> text "; " <> b
    convertOne Citation { citationId = k
                        , citationPrefix = p
                        , citationSuffix = s
                        , citationMode = m
                        }
        = case m of
               AuthorInText   -> citeCommand "citealt"  p s k
               SuppressAuthor -> citeCommand "citeyear" p s k
               NormalCitation -> citeCommand "citealp"  p s k

citeCommand :: PandocMonad m
            => String -> [Inline] -> [Inline] -> String -> LW m Doc
citeCommand c p s k = do
  args <- citeArguments p s k
  return $ text ("\\" ++ c) <> args

citeArguments :: PandocMonad m
              => [Inline] -> [Inline] -> String -> LW m Doc
citeArguments p s k = do
  let s' = case s of
        (Str (x:[]) : r) | isPunctuation x -> dropWhile (== Space) r
        (Str (x:xs) : r) | isPunctuation x -> Str xs : r
        _                -> s
  pdoc <- inlineListToLaTeX p
  sdoc <- inlineListToLaTeX s'
  let optargs = case (isEmpty pdoc, isEmpty sdoc) of
                     (True, True ) -> empty
                     (True, False) -> brackets sdoc
                     (_   , _    ) -> brackets pdoc <> brackets sdoc
  return $ optargs <> braces (text k)

citationsToBiblatex :: PandocMonad m => [Citation] -> LW m Doc
citationsToBiblatex (one:[])
  = citeCommand cmd p s k
    where
       Citation { citationId = k
                , citationPrefix = p
                , citationSuffix = s
                , citationMode = m
                } = one
       cmd = case m of
                  SuppressAuthor -> "autocite*"
                  AuthorInText   -> "textcite"
                  NormalCitation -> "autocite"

citationsToBiblatex (c:cs) = do
  args <- mapM convertOne (c:cs)
  return $ text cmd <> foldl' (<>) empty args
    where
       cmd = case citationMode c of
                  SuppressAuthor -> "\\autocites*"
                  AuthorInText   -> "\\textcites"
                  NormalCitation -> "\\autocites"
       convertOne Citation { citationId = k
                           , citationPrefix = p
                           , citationSuffix = s
                           }
              = citeArguments p s k

citationsToBiblatex _ = return empty

-- Determine listings language from list of class attributes.
getListingsLanguage :: [String] -> Maybe String
getListingsLanguage []     = Nothing
getListingsLanguage (x:xs) = toListingsLanguage x <|> getListingsLanguage xs

mbBraced :: String -> String
mbBraced x = if not (all isAlphaNum x)
                then "{" <> x <> "}"
                else x

-- Extract a key from divs and spans
extract :: String -> Block -> [String]
extract key (Div attr _)     = lookKey key attr
extract key (Plain ils)      = query (extractInline key) ils
extract key (Para ils)       = query (extractInline key) ils
extract key (Header _ _ ils) = query (extractInline key) ils
extract _ _                  = []

-- Extract a key from spans
extractInline :: String -> Inline -> [String]
extractInline key (Span attr _) = lookKey key attr
extractInline _ _               = []

-- Look up a key in an attribute and give a list of its values
lookKey :: String -> Attr -> [String]
lookKey key (_,_,kvs) =  maybe [] words $ lookup key kvs

-- In environments \Arabic instead of \arabic is used
toPolyglossiaEnv :: Lang -> (String, String)
toPolyglossiaEnv l =
  case toPolyglossia l of
    ("arabic", o) -> ("Arabic", o)
    x             -> x

-- Takes a list of the constituents of a BCP 47 language code and
-- converts it to a Polyglossia (language, options) tuple
-- http://mirrors.ctan.org/macros/latex/contrib/polyglossia/polyglossia.pdf
toPolyglossia :: Lang -> (String, String)
toPolyglossia (Lang "ar" _ "DZ" _)        = ("arabic", "locale=algeria")
toPolyglossia (Lang "ar" _ "IQ" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "JO" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "LB" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "LY" _)        = ("arabic", "locale=libya")
toPolyglossia (Lang "ar" _ "MA" _)        = ("arabic", "locale=morocco")
toPolyglossia (Lang "ar" _ "MR" _)        = ("arabic", "locale=mauritania")
toPolyglossia (Lang "ar" _ "PS" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "SY" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "TN" _)        = ("arabic", "locale=tunisia")
toPolyglossia (Lang "de" _ _ vars)
  | "1901" `elem` vars                    = ("german", "spelling=old")
toPolyglossia (Lang "de" _ "AT" vars)
  | "1901" `elem` vars                    = ("german", "variant=austrian, spelling=old")
toPolyglossia (Lang "de" _ "AT" _)        = ("german", "variant=austrian")
toPolyglossia (Lang "de" _ "CH" vars)
  | "1901" `elem` vars                    = ("german", "variant=swiss, spelling=old")
toPolyglossia (Lang "de" _ "CH" _)        = ("german", "variant=swiss")
toPolyglossia (Lang "de" _ _ _)           = ("german", "")
toPolyglossia (Lang "dsb" _ _ _)          = ("lsorbian", "")
toPolyglossia (Lang "el" _ "polyton" _)   = ("greek",   "variant=poly")
toPolyglossia (Lang "en" _ "AU" _)        = ("english", "variant=australian")
toPolyglossia (Lang "en" _ "CA" _)        = ("english", "variant=canadian")
toPolyglossia (Lang "en" _ "GB" _)        = ("english", "variant=british")
toPolyglossia (Lang "en" _ "NZ" _)        = ("english", "variant=newzealand")
toPolyglossia (Lang "en" _ "UK" _)        = ("english", "variant=british")
toPolyglossia (Lang "en" _ "US" _)        = ("english", "variant=american")
toPolyglossia (Lang "grc" _ _ _)          = ("greek",   "variant=ancient")
toPolyglossia (Lang "hsb" _ _  _)         = ("usorbian", "")
toPolyglossia (Lang "la" _ _ vars)
  | "x-classic" `elem` vars               = ("latin", "variant=classic")
toPolyglossia (Lang "sl" _ _ _)           = ("slovenian", "")
toPolyglossia x                           = (commonFromBcp47 x, "")

-- Takes a list of the constituents of a BCP 47 language code and
-- converts it to a Babel language string.
-- http://mirrors.ctan.org/macros/latex/required/babel/base/babel.pdf
-- List of supported languages (slightly outdated):
-- http://tug.ctan.org/language/hyph-utf8/doc/generic/hyph-utf8/hyphenation.pdf
toBabel :: Lang -> String
toBabel (Lang "de" _ "AT" vars)
  | "1901" `elem` vars                  = "austrian"
  | otherwise                           = "naustrian"
toBabel (Lang "de" _ "CH" vars)
  | "1901" `elem` vars                  = "swissgerman"
  | otherwise                           = "nswissgerman"
toBabel (Lang "de" _ _ vars)
  | "1901" `elem` vars                  = "german"
  | otherwise                           = "ngerman"
toBabel (Lang "dsb" _ _ _)              = "lowersorbian"
toBabel (Lang "el" _ _ vars)
  | "polyton" `elem` vars               = "polutonikogreek"
toBabel (Lang "en" _ "AU" _)            = "australian"
toBabel (Lang "en" _ "CA" _)            = "canadian"
toBabel (Lang "en" _ "GB" _)            = "british"
toBabel (Lang "en" _ "NZ" _)            = "newzealand"
toBabel (Lang "en" _ "UK" _)            = "british"
toBabel (Lang "en" _ "US" _)            = "american"
toBabel (Lang "fr" _ "CA" _)            = "canadien"
toBabel (Lang "fra" _ _ vars)
  | "aca" `elem` vars                   = "acadian"
toBabel (Lang "grc" _ _ _)              = "polutonikogreek"
toBabel (Lang "hsb" _ _ _)              = "uppersorbian"
toBabel (Lang "la" _ _ vars)
  | "x-classic" `elem` vars             = "classiclatin"
toBabel (Lang "sl" _ _ _)               = "slovene"
toBabel x                               = commonFromBcp47 x

-- Takes a list of the constituents of a BCP 47 language code
-- and converts it to a string shared by Babel and Polyglossia.
-- https://tools.ietf.org/html/bcp47#section-2.1
commonFromBcp47 :: Lang -> String
commonFromBcp47 (Lang "pt" _ "BR" _)            = "brazil"
-- Note: documentation says "brazilian" works too, but it doesn't seem to work
-- on some systems.  See #2953.
commonFromBcp47 (Lang "sr" "Cyrl" _ _)          = "serbianc"
commonFromBcp47 (Lang "zh" "Latn" _ vars)
  | "pinyin" `elem` vars                        = "pinyin"
commonFromBcp47 (Lang l _ _ _) = fromIso l
  where
    fromIso "af"  = "afrikaans"
    fromIso "am"  = "amharic"
    fromIso "ar"  = "arabic"
    fromIso "as"  = "assamese"
    fromIso "ast" = "asturian"
    fromIso "bg"  = "bulgarian"
    fromIso "bn"  = "bengali"
    fromIso "bo"  = "tibetan"
    fromIso "br"  = "breton"
    fromIso "ca"  = "catalan"
    fromIso "cy"  = "welsh"
    fromIso "cs"  = "czech"
    fromIso "cop" = "coptic"
    fromIso "da"  = "danish"
    fromIso "dv"  = "divehi"
    fromIso "el"  = "greek"
    fromIso "en"  = "english"
    fromIso "eo"  = "esperanto"
    fromIso "es"  = "spanish"
    fromIso "et"  = "estonian"
    fromIso "eu"  = "basque"
    fromIso "fa"  = "farsi"
    fromIso "fi"  = "finnish"
    fromIso "fr"  = "french"
    fromIso "fur" = "friulan"
    fromIso "ga"  = "irish"
    fromIso "gd"  = "scottish"
    fromIso "gez" = "ethiopic"
    fromIso "gl"  = "galician"
    fromIso "he"  = "hebrew"
    fromIso "hi"  = "hindi"
    fromIso "hr"  = "croatian"
    fromIso "hu"  = "magyar"
    fromIso "hy"  = "armenian"
    fromIso "ia"  = "interlingua"
    fromIso "id"  = "indonesian"
    fromIso "ie"  = "interlingua"
    fromIso "is"  = "icelandic"
    fromIso "it"  = "italian"
    fromIso "jp"  = "japanese"
    fromIso "km"  = "khmer"
    fromIso "kmr" = "kurmanji"
    fromIso "kn"  = "kannada"
    fromIso "ko"  = "korean"
    fromIso "la"  = "latin"
    fromIso "lo"  = "lao"
    fromIso "lt"  = "lithuanian"
    fromIso "lv"  = "latvian"
    fromIso "ml"  = "malayalam"
    fromIso "mn"  = "mongolian"
    fromIso "mr"  = "marathi"
    fromIso "nb"  = "norsk"
    fromIso "nl"  = "dutch"
    fromIso "nn"  = "nynorsk"
    fromIso "no"  = "norsk"
    fromIso "nqo" = "nko"
    fromIso "oc"  = "occitan"
    fromIso "pa"  = "panjabi"
    fromIso "pl"  = "polish"
    fromIso "pms" = "piedmontese"
    fromIso "pt"  = "portuguese"
    fromIso "rm"  = "romansh"
    fromIso "ro"  = "romanian"
    fromIso "ru"  = "russian"
    fromIso "sa"  = "sanskrit"
    fromIso "se"  = "samin"
    fromIso "sk"  = "slovak"
    fromIso "sq"  = "albanian"
    fromIso "sr"  = "serbian"
    fromIso "sv"  = "swedish"
    fromIso "syr" = "syriac"
    fromIso "ta"  = "tamil"
    fromIso "te"  = "telugu"
    fromIso "th"  = "thai"
    fromIso "ti"  = "ethiopic"
    fromIso "tk"  = "turkmen"
    fromIso "tr"  = "turkish"
    fromIso "uk"  = "ukrainian"
    fromIso "ur"  = "urdu"
    fromIso "vi"  = "vietnamese"
    fromIso _     = ""

pDocumentOptions :: P.Parsec String () [String]
pDocumentOptions = do
  P.char '['
  opts <- P.sepBy
    (P.many $ P.spaces *> P.noneOf (" ,]" :: String) <* P.spaces)
    (P.char ',')
  P.char ']'
  return opts

pDocumentClass :: P.Parsec String () String
pDocumentClass =
  do P.skipMany (P.satisfy (/='\\'))
     P.string "\\documentclass"
     classOptions <- pDocumentOptions <|> return []
     if ("article" :: String) `elem` classOptions
       then return "article"
       else do P.skipMany (P.satisfy (/='{'))
               P.char '{'
               P.manyTill P.letter (P.char '}')
