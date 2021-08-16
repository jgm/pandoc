{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Readers.LaTeX.Table
  ( tableEnvironments )
where

import Data.Functor (($>))
import Text.Pandoc.Class
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Readers.LaTeX.Types
import Text.Pandoc.Builder as B
import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Control.Applicative ((<|>), optional, many)
import Control.Monad (when, void)
import Text.Pandoc.Shared (safeRead, trim)
import Text.Pandoc.Logging (LogMessage(SkippedContent))
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Parsing hiding (blankline, many, mathDisplay, mathInline,
                            optional, space, spaces, withRaw, (<|>))

tableEnvironments :: PandocMonad m
                  => LP m Blocks
                  -> LP m Inlines
                  -> M.Map Text (LP m Blocks)
tableEnvironments blocks inline =
  M.fromList
  [ ("longtable",  env "longtable" $
          resetCaption *>
            simpTable blocks inline "longtable" False >>= addTableCaption)
  , ("table",  env "table" $
          skipopts *> resetCaption *> blocks >>= addTableCaption)
  , ("tabular*", env "tabular*" $ simpTable blocks inline "tabular*" True)
  , ("tabularx", env "tabularx" $ simpTable blocks inline "tabularx" True)
  , ("tabular", env "tabular"  $ simpTable blocks inline "tabular" False)
  ]

hline :: PandocMonad m => LP m ()
hline = try $ do
  spaces
  controlSeq "hline" <|>
    (controlSeq "cline" <* braced) <|>
    -- booktabs rules:
    controlSeq "toprule" <|>
    controlSeq "bottomrule" <|>
    controlSeq "midrule" <|>
    controlSeq "endhead" <|>
    controlSeq "endfirsthead"
  spaces
  optional rawopt
  return ()

lbreak :: PandocMonad m => LP m Tok
lbreak = (controlSeq "\\" <|> controlSeq "tabularnewline")
         <* skipopts <* spaces

amp :: PandocMonad m => LP m Tok
amp = symbol '&'

-- Split a Word into individual Symbols (for parseAligns)
splitWordTok :: PandocMonad m => LP m ()
splitWordTok = do
  inp <- getInput
  case inp of
       (Tok spos Word t : rest) ->
         setInput $ map (Tok spos Symbol . T.singleton) (T.unpack t) <> rest
       _ -> return ()

parseAligns :: PandocMonad m => LP m [(Alignment, ColWidth, ([Tok], [Tok]))]
parseAligns = try $ do
  let maybeBar = skipMany
        (try $ sp *> (() <$ symbol '|' <|> () <$ (symbol '@' >> braced)))
  let cAlign = AlignCenter <$ symbol 'c'
  let lAlign = AlignLeft <$ symbol 'l'
  let rAlign = AlignRight <$ symbol 'r'
  let parAlign = AlignLeft <$ symbol 'p'
  -- aligns from tabularx
  let xAlign = AlignLeft <$ symbol 'X'
  let mAlign = AlignLeft <$ symbol 'm'
  let bAlign = AlignLeft <$ symbol 'b'
  let alignChar = splitWordTok *> (  cAlign <|> lAlign <|> rAlign <|> parAlign
                                 <|> xAlign <|> mAlign <|> bAlign )
  let alignPrefix = symbol '>' >> braced
  let alignSuffix = symbol '<' >> braced
  let colWidth = try $ do
        symbol '{'
        ds <- trim . untokenize <$> manyTill anyTok (controlSeq "linewidth")
        spaces
        symbol '}'
        return $ safeRead ds
  let alignSpec = do
        pref <- option [] alignPrefix
        spaces
        al <- alignChar
        width <- colWidth <|> option Nothing (do s <- untokenize <$> braced
                                                 pos <- getPosition
                                                 report $ SkippedContent s pos
                                                 return Nothing)
        spaces
        suff <- option [] alignSuffix
        return (al, width, (pref, suff))
  let starAlign = do -- '*{2}{r}' == 'rr', we just expand like a macro
        symbol '*'
        spaces
        ds <- trim . untokenize <$> braced
        spaces
        spec <- braced
        case safeRead ds of
             Just n  ->
               getInput >>= setInput . (mconcat (replicate n spec) ++)
             Nothing -> Prelude.fail $ "Could not parse " <> T.unpack ds <> " as number"
  bgroup
  spaces
  maybeBar
  aligns' <- many $ try $ spaces >> optional starAlign >>
                            (alignSpec <* maybeBar)
  spaces
  egroup
  spaces
  return $ map toSpec aligns'
  where
    toColWidth (Just w) | w > 0 = ColWidth w
    toColWidth _                = ColWidthDefault
    toSpec (x, y, z) = (x, toColWidth y, z)

-- N.B. this parser returns a Row that may have erroneous empty cells
-- in it. See the note above fixTableHead for details.
parseTableRow :: PandocMonad m
              => LP m Blocks -- ^ block parser
              -> LP m Inlines -- ^ inline parser
              -> Text   -- ^ table environment name
              -> [([Tok], [Tok])] -- ^ pref/suffixes
              -> LP m Row
parseTableRow blocks inline envname prefsufs = do
  notFollowedBy (spaces *> end_ envname)
  -- contexts that can contain & that is not colsep:
  let canContainAmp (Tok _ (CtrlSeq "begin") _) = True
      canContainAmp (Tok _ (CtrlSeq "verb") _)  = True
      canContainAmp (Tok _ (CtrlSeq "Verb") _)  = True
      canContainAmp _       = False
  -- add prefixes and suffixes in token stream:
  let celltoks (pref, suff) = do
        prefpos <- getPosition
        contents <- mconcat <$>
            many ( snd <$> withRaw
                     ((lookAhead (controlSeq "parbox") >>
                       void blocks) -- #5711
                      <|>
                      (lookAhead (satisfyTok canContainAmp) >> void inline)
                      <|>
                      (lookAhead (symbol '$') >> void inline))
                  <|>
                   (do notFollowedBy
                         (() <$ amp <|> () <$ lbreak <|> end_ envname)
                       count 1 anyTok) )

        suffpos <- getPosition
        option [] (count 1 amp)
        return $ map (setpos prefpos) pref ++ contents ++ map (setpos suffpos) suff
  rawcells <- mapM celltoks prefsufs
  cells <- mapM (parseFromToks (parseTableCell blocks)) rawcells
  spaces
  return $ Row nullAttr cells

parseTableCell :: PandocMonad m => LP m Blocks -> LP m Cell
parseTableCell blocks = do
  spaces
  updateState $ \st -> st{ sInTableCell = True }
  cell' <-   multicolumnCell blocks
         <|> multirowCell blocks
         <|> parseSimpleCell
         <|> parseEmptyCell
  updateState $ \st -> st{ sInTableCell = False }
  spaces
  return cell'
  where
    -- The parsing of empty cells is important in LaTeX, especially when dealing
    -- with multirow/multicolumn. See #6603.
    parseEmptyCell = spaces $> emptyCell
    parseSimpleCell = simpleCell <$> (plainify <$> blocks)


cellAlignment :: PandocMonad m => LP m Alignment
cellAlignment = skipMany (symbol '|') *> alignment <* skipMany (symbol '|')
  where
    alignment = do
      c <- untoken <$> singleChar
      return $ case c of
        "l" -> AlignLeft
        "r" -> AlignRight
        "c" -> AlignCenter
        "*" -> AlignDefault
        _   -> AlignDefault

plainify :: Blocks -> Blocks
plainify bs = case toList bs of
                [Para ils] -> plain (fromList ils)
                _          -> bs

multirowCell :: PandocMonad m => LP m Blocks -> LP m Cell
multirowCell blocks = controlSeq "multirow" >> do
  -- Full prototype for \multirow macro is:
  --     \multirow[vpos]{nrows}[bigstruts]{width}[vmove]{text}
  -- However, everything except `nrows` and `text` make
  -- sense in the context of the Pandoc AST
  _ <- optional $ symbol '[' *> cellAlignment <* symbol ']'   -- vertical position
  nrows <- fmap (fromMaybe 1 . safeRead . untokenize) braced
  _ <- optional $ symbol '[' *> manyTill anyTok (symbol ']')  -- bigstrut-related
  _ <- symbol '{' *> manyTill anyTok (symbol '}')             -- Cell width
  _ <- optional $ symbol '[' *> manyTill anyTok (symbol ']')  -- Length used for fine-tuning
  content <- symbol '{' *> (plainify <$> blocks) <* symbol '}'
  return $ cell AlignDefault (RowSpan nrows) (ColSpan 1) content

multicolumnCell :: PandocMonad m => LP m Blocks -> LP m Cell
multicolumnCell blocks = controlSeq "multicolumn" >> do
  span' <- fmap (fromMaybe 1 . safeRead . untokenize) braced
  alignment <- symbol '{' *> cellAlignment <* symbol '}'

  let singleCell = do
        content <- plainify <$> blocks
        return $ cell alignment (RowSpan 1) (ColSpan span') content

  -- Two possible contents: either a \multirow cell, or content.
  -- E.g. \multicol{1}{c}{\multirow{2}{1em}{content}}
  -- Note that a \multirow cell can be nested in a \multicolumn,
  -- but not the other way around. See #6603
  let nestedCell = do
        (Cell _ _ (RowSpan rs) _ bs) <- multirowCell blocks
        return $ cell
                  alignment
                  (RowSpan rs)
                  (ColSpan span')
                  (fromList bs)

  symbol '{' *> (nestedCell <|> singleCell) <* symbol '}'

-- LaTeX tables are stored with empty cells underneath multirow cells
-- denoting the grid spaces taken up by them. More specifically, if a
-- cell spans m rows, then it will overwrite all the cells in the
-- columns it spans for (m-1) rows underneath it, requiring padding
-- cells in these places. These padding cells need to be removed for
-- proper table reading. See #6603.
--
-- These fixTable functions do not otherwise fix up malformed
-- input tables: that is left to the table builder.
fixTableHead :: TableHead -> TableHead
fixTableHead (TableHead attr rows) = TableHead attr rows'
  where
    rows' = fixTableRows rows

fixTableBody :: TableBody -> TableBody
fixTableBody (TableBody attr rhc th tb)
  = TableBody attr rhc th' tb'
  where
    th' = fixTableRows th
    tb' = fixTableRows tb

fixTableRows :: [Row] -> [Row]
fixTableRows = fixTableRows' $ repeat Nothing
  where
    fixTableRows' oldHang (Row attr cells : rs)
      = let (newHang, cells') = fixTableRow oldHang cells
            rs'               = fixTableRows' newHang rs
        in Row attr cells' : rs'
    fixTableRows' _ [] = []

-- The overhang is represented as Just (relative cell dimensions) or
-- Nothing for an empty grid space.
fixTableRow :: [Maybe (ColSpan, RowSpan)] -> [Cell] -> ([Maybe (ColSpan, RowSpan)], [Cell])
fixTableRow oldHang cells
  -- If there's overhang, drop cells until their total width meets the
  -- width of the occupied grid spaces (or we run out)
  | (n, prefHang, restHang) <- splitHang oldHang
  , n > 0
  = let cells' = dropToWidth getCellW n cells
        (restHang', cells'') = fixTableRow restHang cells'
    in (prefHang restHang', cells'')
  -- Otherwise record the overhang of a pending cell and fix the rest
  -- of the row
  | c@(Cell _ _ h w _):cells' <- cells
  = let h' = max 1 h
        w' = max 1 w
        oldHang' = dropToWidth getHangW w' oldHang
        (newHang, cells'') = fixTableRow oldHang' cells'
    in (toHang w' h' <> newHang, c : cells'')
  | otherwise
  = (oldHang, [])
  where
    getCellW (Cell _ _ _ w _) = w
    getHangW = maybe 1 fst
    getCS (ColSpan n) = n

    toHang c r
      | r > 1     = [Just (c, r)]
      | otherwise = replicate (getCS c) Nothing

    -- Take the prefix of the overhang list representing filled grid
    -- spaces. Also return the remainder and the length of this prefix.
    splitHang = splitHang' 0 id

    splitHang' !n l (Just (c, r):xs)
      = splitHang' (n + c) (l . (toHang c (r-1) ++)) xs
    splitHang' n l xs = (n, l, xs)

    -- Drop list items until the total width of the dropped items
    -- exceeds the passed width.
    dropToWidth _     n l | n < 1 = l
    dropToWidth wproj n (c:cs)    = dropToWidth wproj (n - wproj c) cs
    dropToWidth _     _ []        = []

simpTable :: PandocMonad m
          => LP m Blocks
          -> LP m Inlines
          -> Text
          -> Bool
          -> LP m Blocks
simpTable blocks inline envname hasWidthParameter = try $ do
  when hasWidthParameter $ () <$ tokWith inline
  skipopts
  colspecs <- parseAligns
  let (aligns, widths, prefsufs) = unzip3 colspecs
  optional $ controlSeq "caption" *> setCaption inline
  spaces
  optional label
  spaces
  optional lbreak
  spaces
  skipMany hline
  spaces
  header' <- option [] . try . fmap (:[]) $
             parseTableRow blocks inline envname prefsufs <*
               lbreak <* many1 hline
  spaces
  rows <- sepEndBy (parseTableRow blocks inline envname prefsufs)
                    (lbreak <* optional (skipMany hline))
  spaces
  optional $ controlSeq "caption" *> setCaption inline
  spaces
  optional label
  spaces
  optional lbreak
  spaces
  lookAhead $ controlSeq "end" -- make sure we're at end
  let th  = fixTableHead $ TableHead nullAttr header'
  let tbs = [fixTableBody $ TableBody nullAttr 0 [] rows]
  let tf  = TableFoot nullAttr []
  return $ table emptyCaption (zip aligns widths) th tbs tf

addTableCaption :: PandocMonad m => Blocks -> LP m Blocks
addTableCaption = walkM go
  where go (Table attr c spec th tb tf) = do
          st <- getState
          let mblabel = sLastLabel st
          capt <- case (sCaption st, mblabel) of
                   (Just ils, Nothing)  -> return $ caption Nothing (plain ils)
                   (Just ils, Just lab) -> do
                     num <- getNextNumber sLastTableNum
                     setState
                       st{ sLastTableNum = num
                         , sLabels = M.insert lab
                                    [Str (renderDottedNum num)]
                                    (sLabels st) }
                     return $ caption Nothing (plain ils) -- add number??
                   (Nothing, _)  -> return c
          let attr' = case (attr, mblabel) of
                        ((_,classes,kvs), Just ident) ->
                           (ident,classes,kvs)
                        _ -> attr
          return $ addAttrDiv attr' $ Table nullAttr capt spec th tb tf
        go x = return x

-- TODO: For now we add a Div to contain table attributes, since
-- most writers don't do anything yet with attributes on Table.
-- This can be removed when that changes.
addAttrDiv :: Attr -> Block -> Block
addAttrDiv ("",[],[]) b = b
addAttrDiv attr b       = Div attr [b]
