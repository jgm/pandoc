{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{- |
   Module      : Text.Pandoc.Writers.Zim
   Copyright   : Copyright (C) 2014 Mathnerd314 <mathnerd314.gph+pdc@gmail.com>
   License     : GNU GPL, version 2 or above

   Maintainer  : nobody
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Zim markup.

Note that Zim is relatively limited in terms of syntax; for example, there are no official escape characters.
Pandoc will insert Unicode zero-width non-joiners whenever an escape character would be used.
Also, footnotes and tables are not possible in Zim.
If pandoc encounters one of these, it will transliterate it (e.g., tables will be written as CSV) or give an error.

Zim:  <http://www.zim-wiki.org/>
-}
module Text.Pandoc.Writers.Zim ( writeZim ) where
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Char ( isDigit )
import Data.Default
import Data.List ( isPrefixOf, intersperse )
import Text.Pandoc.Definition
import Text.Pandoc.Pretty
import Text.Pandoc.Shared

data ListMarker = Marker [String] ListNumberStyle String
                deriving (Show, Read, Eq, Ord)

data TextStyle = Italic | Bold | Strike | Sub | Sup | SmallC | Pre deriving ( Eq,Ord )

data WriterState = WriterState
    { stOptions   :: [String]
    , stListLevel :: [ListMarker]
    , stTextStyles :: [TextStyle]
    }

instance Default WriterState where
  def = WriterState
          { stOptions = []
          , stListLevel = []
          , stTextStyles = []
          }

type WS = State WriterState

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x = x

-- | Convert Pandoc to Zim.
writeZim :: Pandoc -> String
writeZim document = render Nothing $ evalState (pandocToZim document) def

-- | Return Zim representation of document
pandocToZim :: Pandoc -> WS Doc
pandocToZim (Pandoc meta blocks) = do
  body <- blocksToZim blocks
  title <- inlinesToZim $ docTitle meta
  {-
  todo
  let titleblock = not $ isEmpty title
  -}
  return body

asList :: ListMarker -> WS a -> WS a
asList marker p = do
  origListLevel <- gets stListLevel
  modify $ \st -> st { stListLevel = marker : origListLevel }
  result <- p
  modify $ \st -> st { stListLevel = origListLevel }
  return result

-- | Zim has no official escape characters, but it does support Unicode.
-- | The zero-width non-joiner (U+200C) "breaks cursive connections and
-- | ligatures in rendering". Zim's sequences bear a resemblance to ligatures,
-- | e.g. == automatically starts a header, so it seems reasonable to use it.
-- | It does add an extra character in key navigation, but rendering is fine...
escapeString :: String -> WS String
escapeString (x:y:xs) | x `elem` "'/[{~_*" && y == x
                     || x `elem` "^_"      && y == '{'
                        = (\z -> x:'\x200C':z) <$> escapeString (y:xs)
escapeString xs = return xs

-- | Version for use at the beginning of a line
escapeHead :: String -> WS String
escapeHead (x:y:xs) | x == '=' && y == '='
                   || x == '*' && y `elem` " \t"
                      = (\z -> x:'\x200C':z) <$> escapeString (y:xs)
escapeHead (x:xs) | x == '\t' || isDigit x = (x :) <$> escapeHead xs
escapeHead xs = escapeString xs

-- | Convert list of block elements to Zim.
blocksToZim :: [Block] -> WS Doc
blocksToZim (x@(Para{}):xs@(CodeBlock{}:_)) = do
    x' <- blockToZim x
    xs' <- blocksToZim xs
    return $ x' $$ xs'
blocksToZim (x@(Para _):xs) = do
    x' <- blockToZim x
    xs' <- blocksToZim xs
    return $ x' $+$ xs'
blocksToZim (x:xs) = do
      x' <- blockToZim x
      xs' <- blocksToZim xs
      return $ x' $$ xs'
blocksToZim [] = return empty

-- | Convert block element to Zim.
blockToZim :: Block -> WS Doc
blockToZim (BlockQuote blocks) = do
  contents <- blocksToZim blocks
  return $ cr <> (prefixed "\t" contents) <> cr
blockToZim (CodeBlock _ str) = do
  return $ cr $$ text "'''" $$ text str $$ text "'''" <> cr
blockToZim (Div _ bs) = do
  contents <- blocksToZim bs
  blocksToZim $ map plainToPara bs
  return $ blankline <> contents <> blankline
blockToZim (Header level _ elements) = do
  contents <- inlinesToZim elements
  let level' = (1 `max` (5 `min` level))
  let eqs = text $ replicate (7-level') '='
  return $ eqs <+> contents <+> eqs <> cr
blockToZim HorizontalRule = return $ blankline <> text "----------------" <> blankline -- not yet implemented in Zim (https://bugs.launchpad.net/zim/+bug/514945)
blockToZim Null = return empty
blockToZim (Para elements) = do
  contents <- inlinesToZim elements
  -- escape if para starts with ordered list marker
  let esc = if ("* "::String) `isPrefixOf` (render Nothing contents)
               then text "\x200B" -- zero-width space (hack)
               else empty
  return $ esc <> contents <> cr

blockToZim (Plain elements) = inlinesToZim elements

blockToZim (RawBlock (Format f) s)
  | f `elem` ["zim"] = return $ text s
  | otherwise  = return $ ("[raw " <> text f <> " block: ") $$ "'''" $$ (prefixed "\t" $ text s) $$ "'''" $$ "]"

blockToZim (Table capt _ _ headers rows) = do
  let table = if null headers then rows else headers : rows
      toCSV row = cat . intersperse "," <$> mapM (fmap doubleQuotes . blocksToZim) row
  body <- vcat <$> mapM toCSV table
  cDoc <- inlinesToZim capt
  let caption = if isEmpty cDoc then cDoc else doubleQuotes cDoc
      str = body $$ caption
  return $ cr <> ("[table (CSV): ") $$ prefixed "\t" ("'''" $$ str $$ "'''") $$ "]" <> cr

blockToZim (DefinitionList []) = return empty
blockToZim (DefinitionList xs) = blockToZim . BulletList $ map defListToBulletList xs
  where
    -- | approximate definition list w/ a bullet list
    defListToBulletList :: ([Inline],[[Block]]) -> [Block]
    defListToBulletList (term, defs) = Plain [Strong term, Space, Str "-"] : concat defs
blockToZim (BulletList elements) = do
  contents <- asList (Marker (repeat "*") Example "") $ mapM listItemToZim elements
  return $ cr <> vcat contents <> cr
blockToZim (OrderedList _ []) = return empty
blockToZim (OrderedList (startnum, numstyle, delim) elements) = do
  listLevel <- gets stListLevel
  let inList = not (null listLevel)
      numstyle' = case numstyle of
                UpperAlpha -> UpperAlpha
                LowerAlpha -> LowerAlpha
                DefaultStyle | inList, Marker _ Decimal _ <- head listLevel -> LowerAlpha
                _ -> Decimal -- nothing else supported
      numbers = drop (startnum-1) $ case numstyle' of
                UpperAlpha -> upperAlpha
                LowerAlpha -> lowerAlpha
                Decimal    -> decimal
                _ -> error "See numstyle' in blockToZim"
      decimal = map show ([1..] :: [Integer])
      lowerAlpha = cycle $ map (:[]) (['a'..'z'] ++ ['A'..'Z'])
      upperAlpha = drop 26 lowerAlpha
      delim' = case delim of
                 _ -> "." -- only period is supported
  let marker = Marker numbers numstyle' delim'
  contents <- asList marker $ mapM listItemToZim elements
  return $ cr <> prefixed "\t" (vcat contents) <> {-if inList then empty else-} cr

-- | Returns appropriate bullet list marker for indent level.
listItemToZim :: [Block] -> WS Doc
listItemToZim elements = do
  (Marker prefix sty delim) : rest <- gets stListLevel
  let marker' = text (head prefix) <> text delim <> space -- todo
  contents <- blocksToZim elements
  modify $ \s -> s{ stListLevel = (Marker (tail prefix) sty delim) : rest }
  return $ marker' <> contents <> cr

inlinesToZim :: [Inline] -> WS Doc
inlinesToZim elements = hcat <$> mapM inlineToZim elements

inlineToZim :: Inline    -- ^ Inline to convert
                -> WS Doc
inlineToZim (Cite _ elements) = inlinesToZim elements
inlineToZim (Code _ str) = do
  s <- escapeString str
  return $ "''" <> text s <> "''"
inlineToZim (Emph elements) = do
  contents <- inlinesToZim elements
  return $ "//" <> contents <> "//"
inlineToZim (Image alt (source, tit)) = do
  alt' <- inlinesToZim alt -- todo escape
  let txt = if (null tit)
               then if null alt
                       then ""
                       else "|" <> alt'
               else "|" <> text tit
  let txt = if (null alt) || (alt == [Str ""]) || (alt == [Str source]) -- to prevent autolinks
               then "image"
               else alt'
  return $ "{{" <> text source <> txt <> "}}"

inlineToZim LineBreak = return cr

inlineToZim (Link [Image alt (imgsrc,imgtit)] (src, tit)) = do
  error "Image in link not supported"
inlineToZim (Link elements@txt (url@src, title@tit)) = do
  -- TODO:
  -- external link:  http://google.cod[Google]
  -- internal document references to sections??
  -- my@email.com[email john]
  -- relative:  link:downloads/foo.zip[download foo.zip]
  -- mailto link?
  -- autolinks
  let src' = escapeURI src
  contents <- inlinesToZim txt
  label <- inlinesToZim txt
  let isRelative = ':' `notElem` src
  return $ "[[" <> label <> text src <> "]]"
-- TODO: generate external files for equations and insert as objects/images like the Equation plugin
inlineToZim (Math DisplayMath str) = return $ "\\[" <> text str <> "\\]"
inlineToZim (Math InlineMath str) = return $ "\\(" <> text str <> "\\)"
inlineToZim (Note contents) = do
  contents' <- blocksToZim contents
  return $ "[Note: " <> contents' <> "]"
inlineToZim (RawInline f str)
  | f == "Zim" = return $ text str
  | otherwise         = return empty
inlineToZim (Quoted DoubleQuote elements) = do
  contents <- inlinesToZim elements
  return $ "“" <> contents <> "”" -- could use \", but whatever
inlineToZim (Quoted SingleQuote elements) = do
  contents <- inlinesToZim elements
  return $ "‘" <> contents <> "’"
inlineToZim (SmallCaps elements) = inlinesToZim elements -- not supported
inlineToZim Space = return space
inlineToZim (Span _ elements) = inlinesToZim elements
inlineToZim (Str str) = do
  st <- escapeString str
  return (text st)
inlineToZim (Strikeout elements) = do
  contents <- inlinesToZim elements
  return $ "~~" <> contents <> "~~"
inlineToZim (Strong elements) = do
  contents <- inlinesToZim elements
  return $ "**" <> contents <> "**"
inlineToZim (Subscript elements) = do
  contents <- inlinesToZim elements
  return $ "_{" <> contents <> "}"
inlineToZim (Superscript elements) = do
  contents <- inlinesToZim elements
  return $ "^{" <> contents <> "}"
