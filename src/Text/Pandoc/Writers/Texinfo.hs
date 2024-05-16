{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Texinfo
   Copyright   : Copyright (C) 2008-2024 John MacFarlane
                               2012 Peter Wang
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into Texinfo.
-}
module Text.Pandoc.Writers.Texinfo ( writeTexinfo ) where
import Control.Monad (zipWithM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
    ( StateT, MonadState(get), gets, modify, evalStateT )
import Data.Char (chr, ord, isAlphaNum)
import Data.List (maximumBy, transpose, foldl')
import Data.List.NonEmpty (nonEmpty)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (unEscapeString)
import System.FilePath
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.URI
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

data WriterState =
  WriterState { stStrikeout   :: Bool  -- document contains strikeout
              , stContext     :: Context
              , stNodes       :: M.Map Text Int -- maps node to number of duplicates
              , stHeadings    :: M.Map Text Text -- header ids to node texts
              , stOptions     :: WriterOptions -- writer options
              }

data Context = NormalContext | NodeContext
  deriving (Eq, Show)

withContext :: PandocMonad m => Context -> TI m a -> TI m a
withContext context pa = do
  oldContext <- gets stContext
  modify $ \s -> s{ stContext = context }
  res <- pa
  modify $ \s -> s{ stContext = oldContext }
  pure res

disallowedInNode :: Char -> Bool
disallowedInNode c = c `elem` ['.',':',',','(',')']

{- TODO:
 - internal cross references a la HTML
 - generated .texi files don't work when run through texi2dvi
 -}

type TI m = StateT WriterState m

-- | Convert Pandoc to Texinfo.
writeTexinfo :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTexinfo options document =
  evalStateT (pandocToTexinfo options $ wrapTop document)
  WriterState { stStrikeout = False,
                stContext = NormalContext,
                stNodes = mempty,
                stHeadings = mempty,
                stOptions = options}

-- | Add a "Top" node around the document, needed by Texinfo.
wrapTop :: Pandoc -> Pandoc
wrapTop (Pandoc meta blocks) =
  Pandoc meta (Header 0 nullAttr (docTitle meta) : blocks)

addNodeText :: PandocMonad m => Block -> TI m Block
addNodeText (Header lev (ident,_,_) ils) | lev >= 1 && lev <= 4 = do
  node <- render Nothing <$> withContext NodeContext (inlineListToTexinfo ils)
  nodes <- gets stNodes
  node' <- case M.lookup node nodes of
                Just i -> do
                  modify $ \st -> st{ stNodes = M.adjust (+ 1) node nodes }
                  pure $ node <> " " <> tshow (i + 1)
                Nothing -> do
                  modify $ \st -> st{ stNodes = M.insert node 1 nodes }
                  pure node
  unless (T.null ident) $
    modify $ \st -> st{ stHeadings = M.insert ident node' (stHeadings st) }
  pure $ Header lev (ident,[],[("node", node')]) ils
addNodeText x = pure  x

pandocToTexinfo :: PandocMonad m => WriterOptions -> Pandoc -> TI m Text
pandocToTexinfo options (Pandoc meta blocks') = do
  blocks <- walkM addNodeText blocks'
  let titlePage = not $ all null
                      $ docTitle meta : docDate meta : docAuthors meta
  let colwidth = if writerWrapText options == WrapAuto
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToContext options
              blockListToTexinfo
              (fmap chomp .inlineListToTexinfo)
              meta
  body <- blockListToTexinfo blocks
  st <- get
  let context = defField "body" body
              $ defField "toc" (writerTableOfContents options)
              $ defField "titlepage" titlePage
              $ defField "strikeout" (stStrikeout st) metadata
  return $ render colwidth $
    case writerTemplate options of
       Nothing  -> body
       Just tpl -> renderTemplate tpl context

-- | Escape things as needed for Texinfo.
stringToTexinfo :: PandocMonad m => Text -> TI m Text
stringToTexinfo t
  | T.all isAlphaNum t = pure t
  | otherwise = do
      context <- gets stContext
      let escChar '{'      = "@{"
          escChar '}'      = "@}"
          escChar '@'      = "@@"
          escChar '\160'   = "@ "
          escChar '\x2014' = "---"
          escChar '\x2013' = "--"
          escChar '\x2026' = "@dots{}"
          escChar '\x2019' = "'"
          escChar ',' | context == NodeContext = ""
          escChar ':' | context == NodeContext = ""
          escChar '.' | context == NodeContext = ""
          escChar '(' | context == NodeContext = ""
          escChar ')' | context == NodeContext = ""
          escChar c        = T.singleton c
      pure $ T.concatMap escChar t

-- | Puts contents into Texinfo command.
inCmd :: Text -> Doc Text -> Doc Text
inCmd cmd contents = char '@' <> literal cmd <> braces contents

-- | Convert Pandoc block element to Texinfo.
blockToTexinfo :: PandocMonad m
               => Block     -- ^ Block to convert
               -> TI m (Doc Text)

blockToTexinfo (Div _ bs) = blockListToTexinfo bs

blockToTexinfo (Plain lst) =
  inlineListToTexinfo lst

blockToTexinfo (Para lst) =
  inlineListToTexinfo lst    -- this is handled differently from Plain in blockListToTexinfo

blockToTexinfo (LineBlock lns) =
  blockToTexinfo $ linesToPara lns

blockToTexinfo (BlockQuote lst) = do
  contents <- blockListToTexinfo lst
  return $ text "@quotation" $$
           contents $$
           text "@end quotation"

blockToTexinfo (CodeBlock _ str) =
  return $ blankline $$
         text "@verbatim" $$
         flush (literal str) $$
         text "@end verbatim" <> blankline

blockToTexinfo b@(RawBlock f str)
  | f == "texinfo" = return $ literal str
  | f == "latex" || f == "tex" =
                      return $ text "@tex" $$ literal str $$ text "@end tex"
  | otherwise      = do
      report $ BlockNotRendered b
      return empty

blockToTexinfo (BulletList lst) = do
  items <- mapM listItemToTexinfo lst
  return $ text "@itemize" $$
           vcat items $$
           text "@end itemize" <> blankline

blockToTexinfo (OrderedList (start, numstyle, _) lst) = do
  items <- mapM listItemToTexinfo lst
  return $ text "@enumerate " <> exemplar $$
           vcat items $$
           text "@end enumerate" <> blankline
  where
    exemplar = case numstyle of
                DefaultStyle -> decimal
                Decimal      -> decimal
                Example      -> decimal
                UpperRoman   -> decimal   -- Roman numerals not supported
                LowerRoman   -> decimal
                UpperAlpha   -> upperAlpha
                LowerAlpha   -> lowerAlpha
    decimal = if start == 1
                 then empty
                 else text (show start)
    upperAlpha = text [chr $ ord 'A' + start - 1]
    lowerAlpha = text [chr $ ord 'a' + start - 1]

blockToTexinfo (DefinitionList lst) = do
  items <- mapM defListItemToTexinfo lst
  return $ text "@table @asis" $$
           vcat items $$
           text "@end table" <> blankline

blockToTexinfo HorizontalRule =
    -- XXX can't get the equivalent from LaTeX.hs to work
    return $ text "@iftex" $$
             text "@bigskip@hrule@bigskip" $$
             text "@end iftex" $$
             text "@ifnottex" $$
             text (replicate 72 '-') $$
             text "@end ifnottex"

blockToTexinfo (Header 0 _ lst) = do
  txt <- if null lst
            then return $ text "Top"
            else inlineListToTexinfo lst
  return $ text "@node Top" $$
           text "@top " <> txt <> blankline

blockToTexinfo (Header level (_,_,[("node",node)]) lst) = do
    txt <- inlineListToTexinfo lst
    sec <- seccmd level
    return $ if (level > 0) && (level <= 4)
                then blankline <> text "@node " <> literal node $$
                     literal sec <> txt
                else txt
    where
      seccmd :: PandocMonad m => Int -> TI m Text
      seccmd 1 = return "@chapter "
      seccmd 2 = return "@section "
      seccmd 3 = return "@subsection "
      seccmd 4 = return "@subsubsection "
      seccmd _ = throwError $ PandocSomeError "illegal seccmd level"

-- non-node header:
blockToTexinfo (Header _ _ lst) = blockToTexinfo (Para lst)

blockToTexinfo (Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, widths, heads, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  headers <- if all null heads
                then return empty
                else tableHeadToTexinfo aligns heads
  captionText <- inlineListToTexinfo caption
  rowsText <- mapM (tableRowToTexinfo aligns) rows
  colDescriptors <-
    if all (== 0) widths
       then do -- use longest entry instead of column widths
            cols <- mapM (mapM (fmap (T.unpack . render Nothing . hcat) .
                           mapM blockToTexinfo)) $
                        transpose $ heads : rows
            return $ concatMap
                ((\x -> "{"++x++"} ") .
                        maybe "" (maximumBy (comparing length)) . nonEmpty)
                cols
       else return $ "@columnfractions " ++ concatMap (printf "%.2f ") widths
  let tableBody = text ("@multitable " ++ colDescriptors) $$
                  headers $$
                  vcat rowsText $$
                  text "@end multitable"
  return $ if isEmpty captionText
              then tableBody <> blankline
              else text "@float Table" $$
                   tableBody $$
                   inCmd "caption" captionText $$
                   text "@end float"

blockToTexinfo (Figure _ caption [SimpleFigure attr figCaption tgt]) = do
  let capt = if null figCaption
             then let (Caption _ cblks) = caption
                  in blocksToInlines cblks
             else figCaption
  captionText <- if null capt
                 then return empty
                 else (text "@caption" <>) . braces <$> inlineListToTexinfo capt
  img  <- inlineToTexinfo (Image attr figCaption tgt)
  return $ text "@float Figure" $$ img $$ captionText $$ text "@end float"

blockToTexinfo (Figure _ fCaption [
    Table attr tCaption@(Caption _ cbody) specs thead tbody tfoot]) = do
  let caption = case cbody of
                  [] -> fCaption
                  _  -> tCaption
  blockToTexinfo (Table attr caption specs thead tbody tfoot)

blockToTexinfo (Figure _ (Caption _ caption) body) = do
  captionText <- inlineListToTexinfo $ blocksToInlines caption
  content <- blockListToTexinfo body
  return $ text ("@float" ++ floatType body) $$ content $$ (
      if isEmpty captionText
         then empty
         else inCmd "caption" captionText
    ) $$ text "@end float"
  where
  -- floatType according to
  -- https://www.gnu.org/software/texinfo/manual/texinfo/html_node/_0040float.html
  floatType [SimpleFigure {}] = " Figure"
  floatType [Table {}] = " Table"
  floatType _ = ""

tableHeadToTexinfo :: PandocMonad m
                   => [Alignment]
                   -> [[Block]]
                   -> TI m (Doc Text)
tableHeadToTexinfo = tableAnyRowToTexinfo "@headitem "

tableRowToTexinfo :: PandocMonad m
                  => [Alignment]
                  -> [[Block]]
                  -> TI m (Doc Text)
tableRowToTexinfo = tableAnyRowToTexinfo "@item "

tableAnyRowToTexinfo :: PandocMonad m
                     => Text
                     -> [Alignment]
                     -> [[Block]]
                     -> TI m (Doc Text)
tableAnyRowToTexinfo itemtype aligns cols =
  (literal itemtype $$) . foldl' (\row item -> row $$
  (if isEmpty row then empty else text " @tab ") <> item) empty <$> zipWithM alignedBlock aligns cols

alignedBlock :: PandocMonad m
             => Alignment
             -> [Block]
             -> TI m (Doc Text)
-- XXX @flushleft and @flushright text won't get word wrapped.  Since word
-- wrapping is more important than alignment, we ignore the alignment.
alignedBlock _ = blockListToTexinfo
{-
alignedBlock AlignLeft col = do
  b <- blockListToTexinfo col
  return $ text "@flushleft" $$ b $$ text "@end flushleft"
alignedBlock AlignRight col = do
  b <- blockListToTexinfo col
  return $ text "@flushright" $$ b $$ text "@end flushright"
alignedBlock _ col = blockListToTexinfo col
-}

-- | Convert Pandoc block elements to Texinfo.
blockListToTexinfo :: PandocMonad m
                   => [Block]
                   -> TI m (Doc Text)
blockListToTexinfo [] = return empty
blockListToTexinfo (x:xs) = do
  x' <- blockToTexinfo x
  case x of
    Header level _ _ -> do
      -- We need need to insert a menu for this node.
      let (before, after) = break isHeaderBlock xs
      before' <- blockListToTexinfo before
      let menu = if level < 4
                    then collectNodes (level + 1) after
                    else []
      lines' <- mapM makeMenuLine menu
      let menu' = if null lines'
                    then empty
                    else blankline $$
                         text "@menu" $$
                         vcat lines' $$
                         text "@end menu"
      after' <- blockListToTexinfo after
      return $ x' $$ before' $$ menu' $$ after'
    Para _ -> do
      xs' <- blockListToTexinfo xs
      case xs of
           (CodeBlock _ _:_) -> return $ x' $$ xs'
           _                 -> return $ x' $+$ xs'
    _ -> do
      xs' <- blockListToTexinfo xs
      return $ x' $$ xs'

collectNodes :: Int -> [Block] -> [Block]
collectNodes _ [] = []
collectNodes level (x:xs) =
  case x of
    (Header hl _ _)
      | hl < level -> []
      | hl == level -> x : collectNodes level xs
      | otherwise -> collectNodes level xs
    _ ->
      collectNodes level xs

makeMenuLine :: PandocMonad m
             => Block
             -> TI m (Doc Text)
makeMenuLine (Header _ (_,_,[("node", node)]) lst) = do
  txt <- withContext NodeContext $ inlineListToTexinfo lst
  pure $ nowrap $ text "* " <>
    if render Nothing txt == node
       then literal node <> "::"
       else txt <> ": " <> literal node <> "."
makeMenuLine _ = throwError $ PandocSomeError "makeMenuLine called with non-node"

listItemToTexinfo :: PandocMonad m
                  => [Block]
                  -> TI m (Doc Text)
listItemToTexinfo lst = do
  contents <- blockListToTexinfo lst
  let spacer = case reverse lst of
                    (Para{}:_) -> blankline
                    _          -> empty
  return $ text "@item" $$ contents <> spacer

defListItemToTexinfo :: PandocMonad m
                     => ([Inline], [[Block]])
                     -> TI m (Doc Text)
defListItemToTexinfo (term, defs) = do
    term' <- inlineListToTexinfo term
    let defToTexinfo bs = do d <- blockListToTexinfo bs
                             case reverse bs of
                                  (Para{}:_) -> return $ d <> blankline
                                  _          -> return d
    defs' <- mapM defToTexinfo defs
    return $ text "@item " <> term' $+$ vcat defs'

-- | Convert list of inline elements to Texinfo.
inlineListToTexinfo :: PandocMonad m
                    => [Inline]  -- ^ Inlines to convert
                    -> TI m (Doc Text)
inlineListToTexinfo lst = hcat <$> mapM inlineToTexinfo lst

-- | Convert inline element to Texinfo
inlineToTexinfo :: PandocMonad m
                => Inline    -- ^ Inline to convert
                -> TI m (Doc Text)

inlineToTexinfo (Span _ lst) =
  inlineListToTexinfo lst

inlineToTexinfo (Emph lst) =
  inCmd "emph" <$> inlineListToTexinfo lst

-- Underline isn't supported, fall back to Emph
inlineToTexinfo (Underline lst) =
  inlineToTexinfo (Emph lst)

inlineToTexinfo (Strong lst) =
  inCmd "strong" <$> inlineListToTexinfo lst

inlineToTexinfo (Strikeout lst) = do
  modify $ \st -> st{ stStrikeout = True }
  contents <- inlineListToTexinfo lst
  return $ text "@textstrikeout{" <> contents <> text "}"

inlineToTexinfo (Superscript lst) = do
  contents <- inlineListToTexinfo lst
  return $ text "@sup{" <> contents <> char '}'

inlineToTexinfo (Subscript lst) = do
  contents <- inlineListToTexinfo lst
  return $ text "@sub{" <> contents <> char '}'

inlineToTexinfo (SmallCaps lst) =
  inCmd "sc" <$> inlineListToTexinfo lst

inlineToTexinfo (Code (_, cls , _) str) | T.pack "variable" `elem` cls  = do
  code <- stringToTexinfo str
  return $ literal $ "@code{@var{" <> code <> "}}"

inlineToTexinfo (Code _ str) = do
  code <- stringToTexinfo str
  return $ literal $ "@code{" <> code <> "}"

inlineToTexinfo (Quoted SingleQuote lst) = do
  contents <- inlineListToTexinfo lst
  return $ char '`' <> contents <> char '\''

inlineToTexinfo (Quoted DoubleQuote lst) = do
  contents <- inlineListToTexinfo lst
  return $ text "``" <> contents <> text "''"

inlineToTexinfo (Cite _ lst) =
  inlineListToTexinfo lst
inlineToTexinfo (Str str) = literal <$> stringToTexinfo str
inlineToTexinfo (Math _ str) = return $ inCmd "math" $ literal str
inlineToTexinfo il@(RawInline f str)
  | f == "latex" || f == "tex" =
                      return $ text "@tex" $$ literal str $$ text "@end tex"
  | f == "texinfo" =  return $ literal str
  | otherwise      =  do
      report $ InlineNotRendered il
      return empty
inlineToTexinfo LineBreak = return $ text "@*" <> cr
inlineToTexinfo SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
      WrapAuto     -> return space
      WrapNone     -> return space
      WrapPreserve -> return cr
inlineToTexinfo Space = return space

inlineToTexinfo (Link _ txt (src, _))
  | Just ('#', ident) <- T.uncons src = do
      headings <- gets stHeadings
      target <- case M.lookup ident headings of
                  Nothing -> literal <$> stringToTexinfo
                                    (T.filter (not . disallowedInNode) src)
                  Just node -> pure $ literal node
      contents <- withContext NodeContext $ inlineListToTexinfo txt
      return $ text "@ref"
        <> braces (target <> if contents == target
                                then mempty
                                else text ",," <> contents)
  | otherwise = case txt of
      [Str x] | escapeURI x == src ->  -- autolink
                  return $ literal $ "@url{" <> x <> "}"
      _ -> do
        contents <- withContext NodeContext $ inlineListToTexinfo txt
        src1 <- stringToTexinfo src
        return $ literal ("@uref{" <> src1 <> ",") <> contents <>
                 char '}'

inlineToTexinfo (Image attr alternate (source, _)) = do
  content <- withContext NodeContext $ inlineListToTexinfo alternate
  opts <- gets stOptions
  let showDim dim = case dimension dim attr of
                      (Just (Pixel a))   -> showInInch opts (Pixel a) <> "in"
                      (Just (Percent _)) -> ""
                      (Just d)           -> tshow d
                      Nothing            -> ""
  return $ literal ("@image{" <> base <> "," <> showDim Width <> "," <> showDim Height <> ",")
           <> content <> text "," <> literal (ext <> "}")
  where
    ext     = T.drop 1 $ T.pack $ takeExtension source'
    base    = T.pack $ dropExtension source'
    source' = if isURI source
              then T.unpack source
              else unEscapeString $ T.unpack source

inlineToTexinfo (Note contents) = do
  contents' <- blockListToTexinfo contents
  return $ text "@footnote" <> braces contents'
