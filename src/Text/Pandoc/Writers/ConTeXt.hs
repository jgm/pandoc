{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.ConTeXt
   Copyright   : Copyright (C) 2007-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into ConTeXt.
-}
module Text.Pandoc.Writers.ConTeXt ( writeConTeXt ) where
import Control.Monad (liftM, unless)
import Control.Monad.State.Strict
    ( StateT, MonadState(put, get), gets, modify, evalStateT )
import Data.Char (ord, isDigit)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isNothing, mapMaybe, catMaybes)
import Data.Monoid (Any (Any, getAny))
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (unEscapeString)
import Text.Collate.Lang (Lang(..))
import Text.Pandoc.Class.PandocMonad (PandocMonad, report, toLang)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting
  (formatConTeXtBlock, formatConTeXtInline, highlight, styleToConTeXt)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.URI (isURI)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Walk (query)
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann

data WriterState =
  WriterState
  { stCslHangingIndent :: Bool -- CSL hanging indent
  , stHasCslRefs       :: Bool -- has CSL citations
  , stHighlighting     :: Bool -- has syntax-highlighted code blocks
  , stNextRef          :: Int  -- number of next URL reference
  , stOptions          :: WriterOptions -- writer options
  , stOrderedListLevel :: Int  -- level of ordered list
  , stEmphasisCommands :: Map.Map Text (Doc Text)
  }

-- | Table type
data Tabl = Xtb  -- ^ Extreme tables
          | Ntb  -- ^ Natural tables
  deriving (Show, Eq)

-- | Whether a heading belongs to a section environment or is standalone.
data HeadingType = SectionHeading | NonSectionHeading

orderedListStyles :: [Char]
orderedListStyles = cycle "narg"

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeConTeXt options document =
  let defaultWriterState = WriterState
        { stCslHangingIndent = False
        , stHasCslRefs = False
        , stHighlighting = False
        , stNextRef = 1
        , stOptions = options
        , stOrderedListLevel = 0
        , stEmphasisCommands = mempty
        }
  in evalStateT (pandocToConTeXt options document) defaultWriterState

type WM = StateT WriterState

pandocToConTeXt :: PandocMonad m => WriterOptions -> Pandoc -> WM m Text
pandocToConTeXt options (Pandoc meta blocks) = do
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToContext options
              blockListToConTeXt
              (fmap chomp . inlineListToConTeXt)
              meta
  main <- blockListToConTeXt $ makeSections False Nothing blocks
  let layoutFromMargins = mconcat $ intersperse ("," :: Doc Text) $
                          mapMaybe (\(x,y) ->
                                ((x <> "=") <>) <$> getField y metadata)
                              [("leftmargin","margin-left")
                              ,("rightmargin","margin-right")
                              ,("top","margin-top")
                              ,("bottom","margin-bottom")
                              ]
  mblang <- fromBCP47 (getLang options meta)
  st <- get
  let context =   defField "toc" (writerTableOfContents options)
                $ defField "placelist"
                   (mconcat . intersperse ("," :: Doc Text) $
                     take (writerTOCDepth options +
                           case writerTopLevelDivision options of
                             TopLevelPart    -> 0
                             TopLevelChapter -> 0
                             _               -> 1)
                       ["chapter","section","subsection","subsubsection",
                        "subsubsubsection","subsubsubsubsection"])
                $ defField "body" main
                $ defField "layout" layoutFromMargins
                $ defField "number-sections" (writerNumberSections options)
                $ defField "csl-refs" (stHasCslRefs st)
                $ defField "csl-hanging-indent" (stCslHangingIndent st)
                $ maybe id (\l ->
                     defField "context-lang" (literal l :: Doc Text)) mblang
                $ (case T.unpack . render Nothing <$>
                      getField "papersize" metadata of
                        Just (('a':d:ds) :: String)
                          | all isDigit (d:ds) -> resetField "papersize"
                                                   (T.pack ('A':d:ds))
                        _                     -> id)
                $ defField "emphasis-commands"
                    (mconcat $ Map.elems (stEmphasisCommands st))
                $ (case writerHighlightStyle options of
                        Just sty | stHighlighting st ->
                          defField "highlighting-commands" (styleToConTeXt sty)
                        _ -> id)
                $ (case T.toLower $ lookupMetaString "pdfa" meta of
                        "true" -> resetField "pdfa" (T.pack "1b:2005")
                        _                     -> id) metadata
  let context' = defField "context-dir" (maybe mempty toContextDir
                                         $ getField "dir" context) context
  return $ render colwidth $
    case writerTemplate options of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context'

-- change rtl to r2l, ltr to l2r
toContextDir :: Doc Text -> Doc Text
toContextDir = fmap (\t -> case t of
                              "ltr" -> "l2r"
                              "rtl" -> "r2l"
                              _     -> t)

-- | escape things as needed for ConTeXt
escapeCharForConTeXt :: WriterOptions -> Char -> Text
escapeCharForConTeXt opts ch =
 let ligatures = isEnabled Ext_smart opts in
 case ch of
    '{'      -> "\\{"
    '}'      -> "\\}"
    '\\'     -> "\\letterbackslash{}"
    '$'      -> "\\$"
    '|'      -> "\\letterbar{}"
    '%'      -> "\\letterpercent{}"
    '~'      -> "\\lettertilde{}"
    '#'      -> "\\#"
    '['      -> "{[}"
    ']'      -> "{]}"
    '\160'   -> "~"
    '\x2014' | ligatures -> "---"
    '\x2013' | ligatures -> "--"
    '\x2019' | ligatures -> "'"
    '\x2026' -> "\\ldots{}"
    x        -> T.singleton x

-- | Escape string for ConTeXt
stringToConTeXt :: WriterOptions -> Text -> Text
stringToConTeXt opts = T.concatMap (escapeCharForConTeXt opts)

-- | Sanitize labels
toLabel :: Text -> Text
toLabel z = T.concatMap go z
 where go x
         | x `elem` ("\\#[]\",{}%()|=" :: String) = "ux" <> T.pack (printf "%x" (ord x))
         | otherwise = T.singleton x

-- | Convert Pandoc block element to ConTeXt.
blockToConTeXt :: PandocMonad m => Block -> WM m (Doc Text)
blockToConTeXt (Div attr@(_,"section":_,_)
                 (Header level _ title' : xs)) = do
  header' <- sectionHeader attr level title' SectionHeading
  footer' <- sectionFooter attr level
  innerContents <- blockListToConTeXt xs
  return $ header' $$ innerContents $$ footer'
blockToConTeXt (Plain lst) = do
  opts <- gets stOptions
  contents <- inlineListToConTeXt lst
  return $
    if isEnabled Ext_tagging opts
    then "\\bpar{}" <> contents <> "\\epar{}"
    else contents
blockToConTeXt (Para lst) = do
  opts <- gets stOptions
  contents <- inlineListToConTeXt lst
  return $
    if isEnabled Ext_tagging opts
    then "\\bpar" $$ contents $$ "\\epar" <> blankline
    else contents <> blankline
blockToConTeXt (LineBlock lns) = do
  let emptyToBlankline doc = if isEmpty doc
                             then blankline
                             else doc
  doclines <- mapM inlineListToConTeXt lns
  let contextLines = vcat . map emptyToBlankline $ doclines
  return $ "\\startlines" $$ contextLines $$ "\\stoplines" <> blankline
blockToConTeXt (BlockQuote lst) = do
  contents <- blockListToConTeXt lst
  return $ "\\startblockquote" $$ nest 0 contents $$ "\\stopblockquote" <> blankline
blockToConTeXt (CodeBlock (_ident, classes, kv) str) = do
  opts <- gets stOptions
  let syntaxMap = writerSyntaxMap opts
  let attr' = ("", classes, kv)
  let unhighlighted = vcat ["\\starttyping", literal str, "\\stoptyping"]
  let highlighted =
        case highlight syntaxMap formatConTeXtBlock attr' str of
          Left msg -> do
            unless (T.null msg) $
              report (CouldNotHighlight msg)
            return unhighlighted
          Right h  -> do
            modify (\s -> s{ stHighlighting = True })
            return (literal h)
  -- blankline because \stoptyping can't have anything after it, inc. '}'
  ($$ blankline) . flush <$>
    if null classes || isNothing (writerHighlightStyle opts)
    then pure unhighlighted
    else highlighted
blockToConTeXt b@(RawBlock f str)
  | f == Format "context" || f == Format "tex" = return $ literal str <> blankline
  | otherwise = empty <$ report (BlockNotRendered b)
blockToConTeXt (Div ("refs",classes,_) bs) = do
  modify $ \st -> st{ stHasCslRefs = True
                    , stCslHangingIndent = "hanging-indent" `elem` classes }
  inner <- blockListToConTeXt bs
  return $ "\\startcslreferences" $$ inner $$ "\\stopcslreferences"
blockToConTeXt (Div (ident,_,kvs) bs) = do
  let align dir txt = "\\startalignment[" <> dir <> "]" $$ txt $$ "\\stopalignment"
  mblang <- fromBCP47 (lookup "lang" kvs)
  let wrapRef txt = if T.null ident
                       then txt
                       else ("\\reference" <> brackets (literal $ toLabel ident) <>
                              braces empty <> "%") $$ txt
      wrapDir = case lookup "dir" kvs of
                  Just "rtl" -> align "righttoleft"
                  Just "ltr" -> align "lefttoright"
                  _          -> id
      wrapLang txt = case mblang of
                       Just lng -> "\\start\\language["
                                     <> literal lng <> "]" $$ txt $$ "\\stop"
                       Nothing  -> txt
      wrapBlank txt = blankline <> txt <> blankline
  wrapBlank . wrapLang . wrapDir . wrapRef <$> blockListToConTeXt bs
blockToConTeXt (BulletList lst) = do
  contents <- mapM listItemToConTeXt lst
  return $ ("\\startitemize" <> if isTightList lst
                                   then brackets "packed"
                                   else empty) $$
    vcat contents $$ literal "\\stopitemize" <> blankline
blockToConTeXt (OrderedList (start, style', delim) lst) = do
    st <- get
    let level = stOrderedListLevel st
    put st {stOrderedListLevel = level + 1}
    contents <- mapM listItemToConTeXt lst
    put st {stOrderedListLevel = level}
    let start' = if start == 1 then "" else "start=" <> tshow start
    let delim' = case delim of
                        DefaultDelim -> ""
                        Period       -> "stopper=."
                        OneParen     -> "stopper=)"
                        TwoParens    -> "left=(,stopper=)"
    let specs2Items = filter (not . T.null) [start', delim']
    let specs2 = if null specs2Items
                    then ""
                    else "[" <> T.intercalate "," specs2Items <> "]"
    let style'' = '[': (case style' of
                          DefaultStyle -> orderedListStyles !! level
                          Decimal      -> 'n'
                          Example      -> 'n'
                          LowerRoman   -> 'r'
                          UpperRoman   -> 'R'
                          LowerAlpha   -> 'a'
                          UpperAlpha   -> 'A') :
                       if isTightList lst then ",packed]" else "]"
    let specs = T.pack style'' <> specs2
    return $ "\\startenumerate" <> literal specs $$ vcat contents $$
             "\\stopenumerate" <> blankline
blockToConTeXt (DefinitionList lst) =
  liftM vcat $ mapM defListItemToConTeXt lst
blockToConTeXt HorizontalRule = return $ "\\thinrule" <> blankline
-- If this is ever executed, provide a default for the reference identifier.
blockToConTeXt (Header level attr lst) =
  sectionHeader attr level lst NonSectionHeading
blockToConTeXt (Table attr caption colspecs thead tbody tfoot) =
  tableToConTeXt (Ann.toTable attr caption colspecs thead tbody tfoot)
blockToConTeXt (Figure (ident, _, _) (Caption cshort clong) body) = do
  title   <- inlineListToConTeXt (blocksToInlines clong)
  list    <- maybe (pure empty) inlineListToConTeXt cshort
  content <- blockListToConTeXt body

  let options =
           ["reference=" <> literal (toLabel ident) | not (T.null ident)]
        ++ ["title="     <> braces title | not (isEmpty title)]
        ++ ["list="      <> braces list  | not (isEmpty list)]
  let hasSubfigures = getAny $
        query (Any . \case {Figure {} -> True; _ -> False}) body
  return
     $ "\\startplacefigure" <> brackets (mconcat $ intersperse "," options)
    $$ (if hasSubfigures then "\\startfloatcombination" else empty)
    $$ content
    $$ (if hasSubfigures then "\\stopfloatcombination" else empty)
    $$ "\\stopplacefigure"
    $$ blankline

tableToConTeXt :: PandocMonad m => Ann.Table -> WM m (Doc Text)
tableToConTeXt (Ann.Table attr caption colspecs thead tbodies tfoot) = do
  opts <- gets stOptions
  let tabl = if isEnabled Ext_ntb opts
             then Ntb
             else Xtb
  captionText <- case caption of
                   Caption _ []       -> return mempty
                   Caption _ longCapt -> blockListToConTeXt longCapt
  head'  <- tableHeadToConTeXt tabl thead
  bodies <- mapM (tableBodyToConTeXt tabl) tbodies
  foot'  <- tableFootToConTeXt tabl tfoot
  let body = case tabl of
        Xtb -> "\\startxtable" $$
               head' $$
               "\\startxtablebody[body]" $$
               vcat bodies $$
               "\\stopxtablebody" $$
               foot' $$
               "\\stopxtable"
        Ntb -> setupCols colspecs $$
               "\\bTABLE" $$
               head' $$
               "\\bTABLEbody" $$
               vcat bodies $$
               "\\eTABLEbody" $$
               foot' $$
               "\\eTABLE"
  let (ident, _classes, _attribs) = attr
  let tblopts = filter (not . isEmpty)
             [ if isEmpty captionText
               then "location=none"
               else "title=" <> braces captionText
             , if T.null ident
               then empty
               else "reference=" <> braces (literal (toLabel ident))
             ]
  return $ vcat
    [ "\\startplacetable" <> brackets (mconcat $ intersperse "," tblopts)
    , body
    , "\\stopplacetable" <> blankline
    ]

setupCols :: [ColSpec] -> Doc Text
setupCols = vcat . zipWith toColSetup [1::Int ..]
  where
    toColSetup i (align, width) =
      let opts = filter (not . isEmpty)
                 [ case align of
                     AlignLeft    -> "align=right"
                     AlignRight   -> "align=left"
                     AlignCenter  -> "align=middle"
                     AlignDefault -> "align=left"
                 , case width of
                     ColWidthDefault -> empty
                     ColWidth w -> ("width=" <>) . braces . text $
                                   printf "%.2f\\textwidth" w
                 ]
      in "\\setupTABLE[column]" <> brackets (text $ show i)
                                <> brackets (mconcat $ intersperse "," opts)

tableBodyToConTeXt :: PandocMonad m
                   => Tabl
                   -> Ann.TableBody
                   -> WM m (Doc Text)
tableBodyToConTeXt tabl (Ann.TableBody _attr _rowHeadCols inthead rows) = do
  intermediateHead <-
    if null inthead
    then return mempty
    else headerRowsToConTeXt tabl Thead inthead
  bodyRows <- bodyRowsToConTeXt tabl rows
  return $ intermediateHead <> bodyRows

tableHeadToConTeXt :: PandocMonad m
                   => Tabl
                   -> Ann.TableHead
                   -> WM m (Doc Text)
tableHeadToConTeXt tabl (Ann.TableHead attr rows) =
  tablePartToConTeXt tabl Thead attr rows

tableFootToConTeXt :: PandocMonad m
                   => Tabl
                   -> Ann.TableFoot
                   -> WM m (Doc Text)
tableFootToConTeXt tbl (Ann.TableFoot attr rows) =
  tablePartToConTeXt tbl Tfoot attr rows

tablePartToConTeXt :: PandocMonad m
                   => Tabl
                   -> TablePart
                   -> Attr
                   -> [Ann.HeaderRow]
                   -> WM m (Doc Text)
tablePartToConTeXt tabl tblpart _attr rows = do
  let (startCmd, stopCmd) = case (tabl, tblpart) of
        (Ntb, Thead) -> ("\\bTABLEhead", "\\eTABLEhead")
        (Ntb, Tfoot) -> ("\\bTABLEfoot", "\\eTABLEfoot")
        (Xtb, Thead) -> ("\\startxtablehead[head]", "\\stopxtablehead")
        (Xtb, Tfoot) -> ("\\startxtablefoot[foot]", "\\stopxtablefoot")
        _            -> ("", "") -- this would be unexpected
  contents <- headerRowsToConTeXt tabl tblpart rows
  return $ startCmd $$ contents $$ stopCmd

-- | The part of a table; header, footer, or body.
data TablePart = Thead | Tfoot | Tbody
  deriving (Eq)

data CellType = HeaderCell | BodyCell

data TableRow = TableRow TablePart Attr Ann.RowHead Ann.RowBody

headerRowsToConTeXt :: PandocMonad m
                    => Tabl
                    -> TablePart
                    -> [Ann.HeaderRow]
                    -> WM m (Doc Text)
headerRowsToConTeXt tabl tablepart = rowListToConTeXt tabl . map toTableRow
  where
    toTableRow (Ann.HeaderRow attr _rownum rowbody) =
      TableRow tablepart attr [] rowbody

bodyRowsToConTeXt :: PandocMonad m
                  => Tabl
                  -> [Ann.BodyRow]
                  -> WM m (Doc Text)
bodyRowsToConTeXt tabl = rowListToConTeXt tabl . map toTableRow
  where
    toTableRow (Ann.BodyRow attr _rownum rowhead rowbody) =
      TableRow Tbody attr rowhead rowbody


rowListToConTeXt :: PandocMonad m
                 => Tabl
                 -> [TableRow]
                 -> WM m (Doc Text)
rowListToConTeXt = \case
  Ntb -> fmap vcat . mapM (tableRowToConTeXt Ntb)
  Xtb -> \rows -> do
    (butlast, lastrow) <-
      case reverse rows of
        []   -> pure ( []
                     , empty
                     )
        r:rs -> (,) <$> (mapM (tableRowToConTeXt Xtb) (reverse rs))
                    <*> tableRowToConTeXt Xtb r
    return $
      vcat butlast $$
      if isEmpty lastrow
      then empty
      else "\\startxrowgroup[lastrow]" $$ lastrow $$ "\\stopxrowgroup"

tableRowToConTeXt :: PandocMonad m
               => Tabl
               -> TableRow
               -> WM m (Doc Text)
tableRowToConTeXt tabl (TableRow tblpart _attr rowhead rowbody) = do
  let celltype = case tblpart of
                   Thead -> HeaderCell
                   _     -> BodyCell
  headcells <- mapM (tableCellToConTeXt tabl HeaderCell) rowhead
  bodycells <- mapM (tableCellToConTeXt tabl celltype) rowbody
  let cells = vcat headcells $$ vcat bodycells
  return $ case tabl of
    Xtb -> "\\startxrow" $$ cells $$ "\\stopxrow"
    Ntb -> "\\bTR" $$ cells $$ "\\eTR"

tableCellToConTeXt :: PandocMonad m
                   => Tabl
                   -> CellType
                   -> Ann.Cell -> WM m (Doc Text)
tableCellToConTeXt tabl celltype (Ann.Cell colspecs _colnum cell) = do
  let Cell _attr cellalign rowspan colspan blocks = cell
  let (colalign, _) :| _ = colspecs
  let halign = alignToConTeXt $
               case (cellalign, tabl) of
                 (AlignDefault, Xtb) -> colalign
                 _                   -> cellalign
  let nx = case colspan of
             ColSpan 1 -> empty
             ColSpan n -> "nc=" <> literal (tshow n)
  let ny = case rowspan of
             RowSpan 1 -> empty
             RowSpan n -> "nr=" <> literal (tshow n)
  let widths = map snd (NonEmpty.toList colspecs)
  let mbcolwidth = flip map widths $ \case
        ColWidthDefault -> Nothing
        ColWidth w      -> Just w
  let colwidth = case catMaybes mbcolwidth of
                   [] -> empty
                   ws -> ("width=" <>) . braces . text $
                         printf "%.2f\\textwidth" (sum ws)
  let keys = hcat . intersperse "," $ filter (not . isEmpty) $
             case tabl of
               Xtb -> [halign, colwidth, nx, ny]
               Ntb -> [halign, nx, ny]  -- no need for a column width
  let options = (if isEmpty keys
                 then empty
                 else brackets keys) <> space
  cellContents <- blockListToConTeXt blocks
  return $ case tabl of
             Xtb -> "\\startxcell" <> options <> cellContents <> " \\stopxcell"
             Ntb -> case celltype of
               BodyCell   -> "\\bTD" <> options <> cellContents <> "\\eTD"
               HeaderCell -> "\\bTH" <> options <> cellContents <> "\\eTH"

alignToConTeXt :: Alignment -> Doc Text
alignToConTeXt = \case
  AlignLeft    -> "align=right"
  AlignRight   -> "align=left"
  AlignCenter  -> "align=middle"
  AlignDefault -> empty


---
--- Lists
--

listItemToConTeXt :: PandocMonad m => [Block] -> WM m (Doc Text)
listItemToConTeXt list = ("\\item" $$) . nest 2 <$> blockListToConTeXt list

defListItemToConTeXt :: PandocMonad m => ([Inline], [[Block]]) -> WM m (Doc Text)
defListItemToConTeXt (term, defs) = do
  term' <- inlineListToConTeXt term
  def'  <- liftM vsep $ mapM blockListToConTeXt defs
  return $ "\\startdescription" <> braces term' $$ nest 2 def' $$
           "\\stopdescription" <> blankline

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: PandocMonad m => [Block] -> WM m (Doc Text)
blockListToConTeXt lst = liftM vcat $ mapM blockToConTeXt lst

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: PandocMonad m
                    => [Inline]  -- ^ Inlines to convert
                    -> WM m (Doc Text)
inlineListToConTeXt lst = liftM hcat $ mapM inlineToConTeXt $ addStruts lst
  -- We add a \strut after a line break that precedes a space,
  -- or the space gets swallowed
  where addStruts (LineBreak : s : xs) | isSpacey s =
           LineBreak : RawInline (Format "context") "\\strut " : s :
             addStruts xs
        addStruts (x:xs) = x : addStruts xs
        addStruts [] = []
        isSpacey Space                               = True
        isSpacey (Str (T.uncons -> Just ('\160',_))) = True
        isSpacey _                                   = False

highlightInlines :: PandocMonad m
                 => Text -> (Doc Text) -> [Inline]
                 -> WM m (Doc Text)
highlightInlines name style inlines = do
  opts <- gets stOptions
  contents <- inlineListToConTeXt inlines
  if not (isEnabled Ext_tagging opts)
    then return $ braces (style <> space <> contents)
    else do
      let cmd = "\\definehighlight " <> brackets (literal name) <>
                brackets ("style=" <> braces style)
      modify (\st -> st{ stEmphasisCommands =
                         Map.insert name cmd (stEmphasisCommands st) })
      return $ "\\" <> literal name <> braces contents

-- | Convert inline element to ConTeXt
inlineToConTeXt :: PandocMonad m
                => Inline    -- ^ Inline to convert
                -> WM m (Doc Text)
inlineToConTeXt (Emph lst)      = highlightInlines "emph"      "\\em" lst
inlineToConTeXt (Strong lst)    = highlightInlines "strong"    "\\bf" lst
inlineToConTeXt (SmallCaps lst) = highlightInlines "smallcaps" "\\sc" lst
inlineToConTeXt (Underline lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\underbar" <> braces contents
inlineToConTeXt (Strikeout lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\overstrikes" <> braces contents
inlineToConTeXt (Superscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\high" <> braces contents
inlineToConTeXt (Subscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\low" <> braces contents
inlineToConTeXt (Code (_ident, classes, _kv) str) = do
  let rawCode =
        pure . literal $
        case typeDelim str of
          Just (open, close) ->
            "\\type" <> (open `T.cons` str) `T.snoc` close
          Nothing ->
            "\\type[escape=yes]{" <>
            (T.replace "{" "/BTEX\\letteropenbrace /ETEX" .
             T.replace "}" "/BTEX\\letterclosebrace /ETEX" $
             str) `T.snoc` '}'
  opts <- gets stOptions
  let syntaxMap = writerSyntaxMap opts
  let attr' = ("", classes, [])
  let highlightCode =
        case highlight syntaxMap formatConTeXtInline attr' str of
          Left msg -> do
            unless (T.null msg) $ report (CouldNotHighlight msg)
            rawCode
          Right h -> do
            modify (\st -> st{ stHighlighting = True })
            return (text (T.unpack h))
  if isNothing (writerHighlightStyle opts) || null classes
    then rawCode
    else highlightCode
inlineToConTeXt (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quote" <> braces contents
inlineToConTeXt (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quotation" <> braces contents
inlineToConTeXt (Cite _ lst) = inlineListToConTeXt lst
inlineToConTeXt (Str str) = do
  opts <- gets stOptions
  return $ literal $ stringToConTeXt opts str
inlineToConTeXt (Math InlineMath str) =
  return $ char '$' <> literal str <> char '$'
inlineToConTeXt (Math DisplayMath str) =
  return $ literal "\\startformula "  <> literal str <> literal " \\stopformula" <> space
inlineToConTeXt il@(RawInline f str)
  | f == Format "tex" || f == Format "context" = return $ literal str
  | otherwise = empty <$ report (InlineNotRendered il)
inlineToConTeXt LineBreak = return $ literal "\\crlf" <> cr
inlineToConTeXt SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  return $ case wrapText of
               WrapAuto     -> space
               WrapNone     -> space
               WrapPreserve -> cr
inlineToConTeXt Space = return space
inlineToConTeXt (Link _ txt (src, _)) = do
  let isAutolink = txt == [Str (T.pack $ unEscapeString $ T.unpack src)]
  let escConTeXtURL = T.concatMap $ \case
        '#' -> "\\#"
        '%' -> "\\%"
        c   -> T.singleton c
  if isAutolink
    then do
      next <- gets stNextRef
      modify $ \st -> st {stNextRef = next + 1}
      let ref = "url" <> tshow next
      return $ mconcat
        [ "\\useURL"
        , brackets (literal ref)
        , brackets (literal $ escConTeXtURL src)
        , "\\from"
        , brackets (literal ref)
        ]
    else do
      contents <- inlineListToConTeXt txt
      -- Handle HTML-like internal document references to sections
      reference <- case T.uncons src of
        Just ('#', ref) -> toLabel <$>
                           (stringToConTeXt <$> gets stOptions <*> pure ref)
        _               -> pure $ "url(" <> escConTeXtURL src <> ")"
      return $ mconcat
        [ "\\goto"
        , braces contents
        , brackets (literal reference)
        ]
inlineToConTeXt (Image attr@(_,cls,_) _ (src, _)) = do
  opts <- gets stOptions
  let showDim dir = let d = literal (tshow dir) <> "="
                    in case dimension dir attr of
                         Just (Pixel a)   ->
                           [d <> literal (showInInch opts (Pixel a)) <> "in"]
                         Just (Percent a) ->
                           [d <> literal (showFl (a / 100)) <> "\\textwidth"]
                         Just dim         ->
                           [d <> literal (tshow dim)]
                         Nothing          ->
                           []
      dimList = showDim Width ++ showDim Height
      dims = if null dimList
                then empty
                else brackets $ mconcat (intersperse "," dimList)
      clas = if null cls
                then empty
                else brackets $ literal $ toLabel $ head cls
      -- Use / for path separators on Windows; see #4918
      fixPathSeparators = T.map $ \c -> case c of
                                          '\\' -> '/'
                                          _    -> c
      src' = fixPathSeparators $
             if isURI src
                then src
                else T.pack $ unEscapeString $ T.unpack src
  return $ braces $ "\\externalfigure" <> brackets (literal src') <> dims <> clas
inlineToConTeXt (Note contents) = do
  contents' <- blockListToConTeXt contents
  let codeBlock x@(CodeBlock _ _) = [x]
      codeBlock _                 = []
  let codeBlocks = query codeBlock contents
  return $ if null codeBlocks
              then literal "\\footnote{" <> nest 2 (chomp contents') <> char '}'
              else literal "\\startbuffer " <> nest 2 (chomp contents') <>
                   literal "\\stopbuffer\\footnote{\\getbuffer}"
inlineToConTeXt (Span (ident,_,kvs) ils) = do
  mblang <- fromBCP47 (lookup "lang" kvs)
  let wrapDir txt = case lookup "dir" kvs of
                      Just "rtl" -> braces $ "\\righttoleft " <> txt
                      Just "ltr" -> braces $ "\\lefttoright " <> txt
                      _          -> txt
      wrapLang txt = case mblang of
                       Just lng -> braces ("\\language" <>
                                           brackets (literal lng) <> txt)
                       Nothing -> txt
      addReference =
        if T.null ident
        then id
        else (("\\reference" <> brackets (literal ident) <> "{}") <>)
  addReference . wrapLang . wrapDir <$> inlineListToConTeXt ils

-- | Craft the section header, inserting the section reference, if supplied.
sectionHeader :: PandocMonad m
              => Attr
              -> Int
              -> [Inline]
              -> HeadingType
              -> WM m (Doc Text)
sectionHeader (ident,classes,kvs) hdrLevel lst secenv = do
  opts <- gets stOptions
  contents <- inlineListToConTeXt lst
  levelText <- sectionLevelToText opts (ident,classes,kvs) hdrLevel secenv
  let optsList = mconcat . filter (not . null) $
        [ ["title=" <> braces contents | not (isEmpty contents)]
        , ["reference=" <> braces (literal (toLabel ident)) | not (T.null ident)]
        , ["number=no"          | "unnumbered" `elem` classes]
        , ["incrementnumber=no" | "unnumbered" `elem` classes]
        ]
  let starter = case secenv of
                  SectionHeading -> "\\start"
                  NonSectionHeading -> "\\"
  let options = if null optsList || isEmpty levelText
                then empty
                else brackets $ hcat (intersperse "," optsList)
  return $ starter <> levelText <> options <> blankline

-- | Craft the section footer
sectionFooter :: PandocMonad m => Attr -> Int -> WM m (Doc Text)
sectionFooter attr hdrLevel = do
  opts <- gets stOptions
  levelText <- sectionLevelToText opts attr hdrLevel SectionHeading
  return $ "\\stop" <> levelText <> blankline

-- | Generate a textual representation of the section level
sectionLevelToText :: PandocMonad m
                   => WriterOptions -> Attr -> Int -> HeadingType
                   -> WM m (Doc Text)
sectionLevelToText opts (_,classes,_) hdrLevel headingType = do
  let unlisted = "unlisted" `elem` classes
  let semanticSection shift = do
        let (section, chapter) = if unlisted
                                 then (literal "subject", literal "title")
                                 else (literal "section", literal "chapter")
        return $ case hdrLevel + shift of
                   -1         -> literal "part"
                   0          -> chapter
                   n | n >= 1 -> text (concat (replicate (n - 1) "sub"))
                                 <> section
                   _          -> empty -- cannot happen

  case writerTopLevelDivision opts of
    TopLevelPart    -> semanticSection (-2)
    TopLevelChapter -> semanticSection (-1)
    TopLevelSection -> semanticSection 0
    TopLevelDefault -> if unlisted
                       then semanticSection 0
                       else return . literal $
                            case headingType of
                              SectionHeading    -> "sectionlevel"
                              NonSectionHeading -> ""

-- | Finds a pair of symbols that can be used as delimiters.
typeDelim :: Text -> Maybe (Char, Char)
typeDelim t =
  let delimChars = "{\"'`()-+=%,.:;"
      go delims '}' = go delims '{'
      go delims c = T.filter (/= c) delims
  in case fmap fst . T.uncons $ T.foldl' go delimChars t of
       Just '{' -> Just ('{', '}')
       Just c   -> Just (c, c)
       Nothing  -> Nothing

fromBCP47 :: PandocMonad m => Maybe Text -> WM m (Maybe Text)
fromBCP47 mbs = fromBCP47' <$> toLang mbs

-- Takes a list of the constituents of a BCP 47 language code
-- and irons out ConTeXt's exceptions
-- https://tools.ietf.org/html/bcp47#section-2.1
-- http://wiki.contextgarden.net/Language_Codes
fromBCP47' :: Maybe Lang -> Maybe Text
fromBCP47' (Just (Lang "ar" _ (Just "SY") _ _ _)) = Just "ar-sy"
fromBCP47' (Just (Lang "ar" _ (Just "IQ") _ _ _)) = Just "ar-iq"
fromBCP47' (Just (Lang "ar" _ (Just "JO") _ _ _)) = Just "ar-jo"
fromBCP47' (Just (Lang "ar" _ (Just "LB") _ _ _)) = Just "ar-lb"
fromBCP47' (Just (Lang "ar" _ (Just "DZ") _ _ _)) = Just "ar-dz"
fromBCP47' (Just (Lang "ar" _ (Just "MA") _ _ _)) = Just "ar-ma"
fromBCP47' (Just (Lang "de" _ _ ["1901"] _ _))    = Just "deo"
fromBCP47' (Just (Lang "de" _ (Just "DE") _ _ _)) = Just "de-de"
fromBCP47' (Just (Lang "de" _ (Just "AT") _ _ _)) = Just "de-at"
fromBCP47' (Just (Lang "de" _ (Just "CH") _ _ _)) = Just "de-ch"
fromBCP47' (Just (Lang "el" _ _ ["poly"] _ _))    = Just "agr"
fromBCP47' (Just (Lang "en" _ (Just "US") _ _ _)) = Just "en-us"
fromBCP47' (Just (Lang "en" _ (Just "GB") _ _ _)) = Just "en-gb"
fromBCP47' (Just (Lang "grc"_ _ _ _ _))           = Just "agr"
fromBCP47' (Just (Lang "el" _ _ _ _ _))           = Just "gr"
fromBCP47' (Just (Lang "eu" _ _ _ _ _))           = Just "ba"
fromBCP47' (Just (Lang "he" _ _ _ _ _))           = Just "il"
fromBCP47' (Just (Lang "jp" _ _ _ _ _))           = Just "ja"
fromBCP47' (Just (Lang "uk" _ _ _ _ _))           = Just "ua"
fromBCP47' (Just (Lang "vi" _ _ _ _ _))           = Just "vn"
fromBCP47' (Just (Lang "zh" _ _ _ _ _))           = Just "cn"
fromBCP47' (Just (Lang l _ _ _ _ _))              = Just l
fromBCP47' Nothing                                = Nothing
