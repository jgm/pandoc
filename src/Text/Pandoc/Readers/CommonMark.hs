{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.CommonMark
   Copyright   : Copyright (C) 2015-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of CommonMark-formatted plain text to 'Pandoc' document.

CommonMark is a strongly specified variant of Markdown: http://commonmark.org.
-}
module Text.Pandoc.Readers.CommonMark (readCommonMark)
where

import Prelude
import CMarkGFM
import Control.Monad.State
import Data.List (groupBy)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Emoji (emojiToInline)
import Text.Pandoc.Options
import Text.Pandoc.Shared (uniqueIdent, taskListItemFromAscii)
import Text.Pandoc.Walk (walkM)

-- | Parse a CommonMark formatted string into a 'Pandoc' structure.
readCommonMark :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readCommonMark opts s = return $
  (if isEnabled Ext_auto_identifiers opts
      then addHeaderIdentifiers opts
      else id) $
  nodeToPandoc opts $ commonmarkToNode opts' exts s
  where opts' = [ optSmart | isEnabled Ext_smart opts ]
        exts = [ extStrikethrough | isEnabled Ext_strikeout opts ] ++
               [ extTable | isEnabled Ext_pipe_tables opts ] ++
               [ extAutolink | isEnabled Ext_autolink_bare_uris opts ]

convertEmojis :: Text -> [Inline] -- TODO text: refactor
convertEmojis = convertEmojis' . T.unpack

convertEmojis' :: String -> [Inline]
convertEmojis' s@(':':xs) =
   case break (==':') xs of
        (ys,':':zs) ->
           case emojiToInline (T.pack ys) of
                Just em -> em : convertEmojis (T.pack zs)
                Nothing -> Str (T.pack $ ':' : ys) : convertEmojis' (':':zs)
        _ -> [Str $ T.pack s]
convertEmojis' s =
  case break (==':') s of
    ("","") -> []
    (_,"") -> [Str $ T.pack s]
    (xs,ys) -> Str (T.pack xs) : convertEmojis' ys

addHeaderIdentifiers :: ReaderOptions -> Pandoc -> Pandoc
addHeaderIdentifiers opts doc = evalState (walkM (addHeaderId opts) doc) mempty

addHeaderId :: ReaderOptions -> Block -> State (Set.Set Text) Block
addHeaderId opts (Header lev (_,classes,kvs) ils) = do
  ids <- get
  let ident = uniqueIdent (readerExtensions opts) ils ids
  modify (Set.insert ident)
  return $ Header lev (ident,classes,kvs) ils
addHeaderId _ x = return x

nodeToPandoc :: ReaderOptions -> Node -> Pandoc
nodeToPandoc opts (Node _ DOCUMENT nodes) =
  Pandoc nullMeta $ foldr (addBlock opts) [] nodes
nodeToPandoc opts n =  -- shouldn't happen
  Pandoc nullMeta $ foldr (addBlock opts) [] [n]

addBlocks :: ReaderOptions -> [Node] -> [Block]
addBlocks opts = foldr (addBlock opts) []

addBlock :: ReaderOptions -> Node -> [Block] -> [Block]
addBlock opts (Node _ PARAGRAPH nodes) =
  (Para (addInlines opts nodes) :)
addBlock _ (Node _ THEMATIC_BREAK _) =
  (HorizontalRule :)
addBlock opts (Node _ BLOCK_QUOTE nodes) =
  (BlockQuote (addBlocks opts nodes) :)
addBlock opts (Node _ (HTML_BLOCK t) _)
  | isEnabled Ext_raw_html opts = (RawBlock (Format "html") t :)
  | otherwise                 = id
-- Note:  the cmark parser will never generate CUSTOM_BLOCK,
-- so we don't need to handle it:
addBlock _ (Node _ (CUSTOM_BLOCK _onEnter _onExit) _nodes) =
  id
addBlock _ (Node _ (CODE_BLOCK info t) _) =
  (CodeBlock ("", take 1 (T.words info), []) t :)
addBlock opts (Node _ (HEADING lev) nodes) =
  (Header lev ("",[],[]) (addInlines opts nodes) :)
addBlock opts (Node _ (LIST listAttrs) nodes) =
  (constructor (map listItem nodes) :)
  where constructor = case listType listAttrs of
                       BULLET_LIST  -> BulletList
                       ORDERED_LIST -> OrderedList
                                         (start, DefaultStyle, delim)
        start = listStart listAttrs
        listItem = taskListItemFromAscii exts . setTightness
                     . addBlocks opts . children
        setTightness = if listTight listAttrs
                           then map paraToPlain
                           else id
        paraToPlain (Para xs) = Plain xs
        paraToPlain x         = x
        delim = case listDelim listAttrs of
                     PERIOD_DELIM -> Period
                     PAREN_DELIM  -> OneParen
        exts = readerExtensions opts
addBlock opts (Node _ (TABLE alignments) nodes) =
  (Table [] aligns widths headers rows :)
  where aligns = map fromTableCellAlignment alignments
        fromTableCellAlignment NoAlignment   = AlignDefault
        fromTableCellAlignment LeftAligned   = AlignLeft
        fromTableCellAlignment RightAligned  = AlignRight
        fromTableCellAlignment CenterAligned = AlignCenter
        widths = replicate numcols 0.0
        numcols = if null rows'
                     then 0
                     else maximum $ map length rows'
        rows' = map toRow $ filter isRow nodes
        (headers, rows) = case rows' of
                               (h:rs) -> (h, rs)
                               []     -> ([], [])
        isRow (Node _ TABLE_ROW _) = True
        isRow _                    = False
        isCell (Node _ TABLE_CELL _) = True
        isCell _                     = False
        toRow (Node _ TABLE_ROW ns) = map toCell $ filter isCell ns
        toRow (Node _ t _) = error $ "toRow encountered non-row " ++ show t
        toCell (Node _ TABLE_CELL []) = []
        toCell (Node _ TABLE_CELL (n:ns))
          | isBlockNode n = addBlocks opts (n:ns)
          | otherwise     = [Plain (addInlines opts (n:ns))]
        toCell (Node _ t _) = error $ "toCell encountered non-cell " ++ show t
addBlock _ (Node _ TABLE_ROW _) = id -- handled in TABLE
addBlock _ (Node _ TABLE_CELL _) = id -- handled in TABLE
addBlock _ _ = id

isBlockNode :: Node -> Bool
isBlockNode (Node _ nodetype _) =
  case nodetype of
       DOCUMENT          -> True
       THEMATIC_BREAK    -> True
       PARAGRAPH         -> True
       BLOCK_QUOTE       -> True
       HTML_BLOCK _      -> True
       CUSTOM_BLOCK _ _  -> True
       CODE_BLOCK _ _    -> True
       HEADING _         -> True
       LIST _            -> True
       ITEM              -> True
       TEXT _            -> False
       SOFTBREAK         -> False
       LINEBREAK         -> False
       HTML_INLINE _     -> False
       CUSTOM_INLINE _ _ -> False
       CODE _            -> False
       EMPH              -> False
       STRONG            -> False
       LINK _ _          -> False
       IMAGE _ _         -> False
       STRIKETHROUGH     -> False
       TABLE _           -> False
       TABLE_ROW         -> False
       TABLE_CELL        -> False

children :: Node -> [Node]
children (Node _ _ ns) = ns

addInlines :: ReaderOptions -> [Node] -> [Inline]
addInlines opts = foldr (addInline opts) []

addInline :: ReaderOptions -> Node -> [Inline] -> [Inline]
addInline opts (Node _ (TEXT t) _) = (foldr ((++) . toinl) [] clumps ++)
  where raw = unpack t
        clumps = groupBy samekind raw
        samekind ' ' ' ' = True
        samekind ' ' _   = False
        samekind _   ' ' = False
        samekind _  _    = True
        toinl (' ':_) = [Space]
        toinl xs      = if isEnabled Ext_emoji opts -- TODO text: refactor
                        then convertEmojis' xs
                        else [Str $ T.pack xs]
addInline _ (Node _ LINEBREAK _) = (LineBreak :)
addInline opts (Node _ SOFTBREAK _)
  | isEnabled Ext_hard_line_breaks opts = (LineBreak :)
  | otherwise                           = (SoftBreak :)
addInline opts (Node _ (HTML_INLINE t) _)
  | isEnabled Ext_raw_html opts = (RawInline (Format "html") t :)
  | otherwise                 = id
-- Note:  the cmark parser will never generate CUSTOM_BLOCK,
-- so we don't need to handle it:
addInline _ (Node _ (CUSTOM_INLINE _onEnter _onExit) _nodes) =
  id
addInline _ (Node _ (CODE t) _) =
  (Code ("",[],[]) t :)
addInline opts (Node _ EMPH nodes) =
  (Emph (addInlines opts nodes) :)
addInline opts (Node _ STRONG nodes) =
  (Strong (addInlines opts nodes) :)
addInline opts (Node _ STRIKETHROUGH nodes) =
  (Strikeout (addInlines opts nodes) :)
addInline opts (Node _ (LINK url title) nodes) =
  (Link nullAttr (addInlines opts nodes) (url, title) :)
addInline opts (Node _ (IMAGE url title) nodes) =
  (Image nullAttr (addInlines opts nodes) (url, title) :)
addInline _ _ = id
