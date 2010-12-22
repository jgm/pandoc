{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
Copyright (C) 2010 John MacFarlane <jgm@berkeley.edu>

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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111(-1)307  USA
-}

{- |
   Module      : Text.Pandoc.Pretty
   Copyright   : Copyright (C) 2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A prettyprinting library for the production of text documents,
including wrapped text, indentated blocks, and tables.
-}

module Text.Pandoc.Pretty (
       Doc
     , render
     , cr
     , blankline
     , space
     , text
     , char
     , prefixed
     , flush
     , nest
     , hang
     , nowrap
     , offset
     , height
     , lblock
     , cblock
     , rblock
     , (<>)
     , (<+>)
     , ($$)
     , ($+$)
     , isEmpty
     , empty
     , cat
     , hcat
     , hsep
     , vcat
     , vsep
     , inside
     , braces
     , brackets
     , parens
     , quotes
     , doubleQuotes
     )

where
import Data.DList (DList, fromList, toList, cons, singleton)
import Data.List (intercalate)
import Data.Monoid
import Data.String
import Control.Monad.State
import Data.Char (isSpace)

data Monoid a =>
     RenderState a = RenderState{
         output       :: [a]        -- ^ In reverse order
       , prefix       :: String
       , usePrefix    :: Bool
       , lineLength   :: Maybe Int  -- ^ 'Nothing' means no wrapping
       , column       :: Int
       , newlines     :: Int        -- ^ Number of preceding newlines
       }

type DocState a = State (RenderState a) ()

data D = Text Int String
       | Block Int [String]
       | Prefixed String Doc
       | Flush Doc
       | BreakingSpace
       | CarriageReturn
       | NewLine
       | BlankLine
       deriving (Show)

newtype Doc = Doc { unDoc :: DList D }
              deriving (Monoid)

instance Show Doc where
  show = render Nothing

instance IsString Doc where
  fromString = text

-- | True if the document is empty.
isEmpty :: Doc -> Bool
isEmpty = null . toList . unDoc

-- | The empty document.
empty :: Doc
empty = mempty

-- | @a <> b@ is the result of concatenating @a@ with @b@.
(<>) :: Doc -> Doc -> Doc
(<>) = mappend

-- | Concatenate a list of 'Doc's.
cat :: [Doc] -> Doc
cat = mconcat

-- | Same as 'cat'.
hcat :: [Doc] -> Doc
hcat = mconcat

-- | Concatenate a list of 'Doc's, putting breakable spaces
-- between them.
(<+>) :: Doc -> Doc -> Doc
(<+>) x y = if isEmpty x
               then y
               else if isEmpty y
                    then x
                    else x <> space <> y

-- | Same as 'cat', but putting breakable spaces between the
-- 'Doc's.
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

-- | @a $$ b@ puts @a@ above @b@.
($$) :: Doc -> Doc -> Doc
($$) x y = if isEmpty x
              then y
              else if isEmpty y
                   then x
                   else x <> cr <> y

-- | @a $$ b@ puts @a@ above @b@, with a blank line between.
($+$) :: Doc -> Doc -> Doc
($+$) x y = if isEmpty x
               then y
               else if isEmpty y
                    then x
                    else x <> blankline <> y

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

-- | List version of '$+$'.
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

outp :: (IsString a, Monoid a)
     => Int -> String -> DocState a
outp off s | off <= 0 = do
  st' <- get
  let rawpref = prefix st'
  when (column st' == 0 && usePrefix st' && not (null rawpref)) $ do
    let pref = reverse $ dropWhile isSpace $ reverse rawpref
    modify $ \st -> st{ output = fromString pref : output st
                      , column = column st + length pref }
  when (off < 0) $ do
     modify $ \st -> st { output = fromString s : output st
                        , column = 0
                        , newlines = newlines st + 1 }
outp off s = do
  st' <- get
  let pref = prefix st'
  when (column st' == 0 && usePrefix st' && not (null pref)) $ do
    modify $ \st -> st{ output = fromString pref : output st
                      , column = column st + length pref }
  modify $ \st -> st{ output = fromString s : output st
                    , column = column st + off
                    , newlines = 0 }

-- | Renders a 'Doc'.  @render (Just n)@ will use
-- a line length of @n@ to reflow text on breakable spaces.
-- @render Nothing@ will not reflow text.
render :: (Monoid a, IsString a)
       => Maybe Int -> Doc -> a
render linelen doc = fromString . mconcat . reverse . output $
  execState (renderDoc doc) startingState
   where startingState = RenderState{
                            output = mempty
                          , prefix = ""
                          , usePrefix = True
                          , lineLength = linelen
                          , column = 0
                          , newlines = 2 }

renderDoc :: (IsString a, Monoid a)
          => Doc -> DocState a
renderDoc = renderList . toList . unDoc

renderList :: (IsString a, Monoid a)
           => [D] -> DocState a
renderList [] = return ()
renderList (Text off s : xs) = do
  outp off s
  renderList xs

renderList (Prefixed pref d : xs) = do
  st <- get
  let oldPref = prefix st
  put st{ prefix = prefix st ++ pref }
  renderDoc d
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

renderList (Flush d : xs) = do
  st <- get
  let oldUsePrefix = usePrefix st
  put st{ usePrefix = False }
  renderDoc d
  modify $ \s -> s{ usePrefix = oldUsePrefix }
  renderList xs

renderList (BlankLine : xs) = do
  st <- get
  case output st of
     _ | newlines st > 1 || null xs -> return ()
     _ | column st == 0 -> do
       outp (-1) "\n"
     _         -> do
       outp (-1) "\n"
       outp (-1) "\n"
  renderList xs

renderList (CarriageReturn : xs) = do
  st <- get
  if newlines st > 0 || null xs
     then renderList xs
     else do
       outp (-1) "\n"
       renderList xs

renderList (NewLine : xs) = do
  outp (-1) "\n"
  renderList xs

renderList (BreakingSpace : CarriageReturn : xs) = renderList (CarriageReturn:xs)
renderList (BreakingSpace : NewLine : xs) = renderList (NewLine:xs)
renderList (BreakingSpace : BlankLine : xs) = renderList (BlankLine:xs)
renderList (BreakingSpace : BreakingSpace : xs) = renderList (BreakingSpace:xs)
renderList (BreakingSpace : xs) = do
  let isText (Text _ _)       = True
      isText (Block _ _)      = True
      isText _                = False
  let isBreakingSpace BreakingSpace = True
      isBreakingSpace _             = False
  let xs' = dropWhile isBreakingSpace xs
  let next = takeWhile isText xs'
  st <- get
  let off = sum $ map offsetOf next
  case lineLength st of
        Just l | column st + 1 + off > l -> do
          outp (-1) "\n"
          renderList xs'
        _  -> do
          outp 1 " "
          renderList xs'

renderList (b1@Block{} : b2@Block{} : xs) =
  renderList (mergeBlocks False b1 b2 : xs)

renderList (b1@Block{} : BreakingSpace : b2@Block{} : xs) =
  renderList (mergeBlocks True b1 b2 : xs)

renderList (Block width lns : xs) = do
  st <- get
  let oldPref = prefix st
  case column st - length oldPref of
        n | n > 0 -> modify $ \s -> s{ prefix = oldPref ++ replicate n ' ' }
        _         -> return ()
  renderDoc $ blockToDoc width lns
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

mergeBlocks :: Bool -> D -> D -> D
mergeBlocks addSpace (Block w1 lns1) (Block w2 lns2) =
  Block (w1 + w2 + if addSpace then 1 else 0) $
     zipWith (\l1 l2 -> pad w1 l1 ++ l2) (lns1 ++ empties) (map sp lns2 ++ empties)
    where empties = replicate (abs $ length lns1 - length lns2) ""
          pad n s = s ++ replicate (n - length s) ' '
          sp "" = ""
          sp xs = if addSpace then (' ' : xs) else xs
mergeBlocks _ _ _ = error "mergeBlocks tried on non-Block!"

blockToDoc :: Int -> [String] -> Doc
blockToDoc _ lns = text $ intercalate "\n" lns

offsetOf :: D -> Int
offsetOf (Text o _)       = o
offsetOf (Block w _)      = w
offsetOf BreakingSpace    = 1
offsetOf _                = 0

-- | A literal string.
text :: String -> Doc
text = Doc . toChunks
  where toChunks :: String -> DList D
        toChunks [] = mempty
        toChunks s = case break (=='\n') s of
                          ([], _:ys) -> NewLine `cons` toChunks ys
                          (xs, _:ys) -> Text (length xs) xs `cons`
                                            NewLine `cons` toChunks ys
                          (xs, [])      -> singleton $ Text (length xs) xs

-- | A character.
char :: Char -> Doc
char c = text [c]

-- | A breaking (reflowable) space.
space :: Doc
space = Doc $ singleton BreakingSpace

-- | A carriage return.  Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: Doc
cr = Doc $ singleton CarriageReturn

-- | Inserts a blank line unless one exists already.
-- (@blankline <> blankline@ has the same effect as @blankline@.
-- If you want multiple blank lines, use @text "\\n\\n"@.
blankline :: Doc
blankline = Doc $ singleton BlankLine

-- | Uses the specified string as a prefix for every line of
-- the inside document (except the first, if not at the beginning
-- of the line).
prefixed :: String -> Doc -> Doc
prefixed pref doc = Doc $ singleton $ Prefixed pref doc

-- | Makes a 'Doc' flush against the left margin.
flush :: Doc -> Doc
flush doc = Doc $ singleton $ Flush doc

-- | Indents a 'Doc' by the specified number of spaces.
nest :: Int -> Doc -> Doc
nest ind = prefixed (replicate ind ' ')

-- | A hanging indent. @hang ind start doc@ prints @start@,
-- then @doc@, leaving an indent of @ind@ spaces on every
-- line but the first.
hang :: Int -> Doc -> Doc -> Doc
hang ind start doc = start <> nest ind doc

-- | Makes a 'Doc' non-reflowable.
nowrap :: Doc -> Doc
nowrap doc = Doc $ fromList $ map replaceSpace $ toList $ unDoc doc
  where replaceSpace BreakingSpace = Text 1 " "
        replaceSpace x = x

-- | Returns the width of a 'Doc'.
offset :: Doc -> Int
offset d = case map length . lines . render Nothing $ d of
                []    -> 0
                os    -> maximum os

block :: (String -> String) -> Int -> Doc -> Doc
block filler width = Doc . singleton . Block width .
                      map filler . chop width . render (Just width)

-- | @lblock n d@ is a block of width @n@ characters, with
-- text derived from @d@ and aligned to the left.
lblock :: Int -> Doc -> Doc
lblock = block id

-- | Like 'lblock' but aligned to the right.
rblock :: Int -> Doc -> Doc
rblock w = block (\s -> replicate (w - length s) ' ' ++ s) w

-- | Like 'lblock' but aligned centered.
cblock :: Int -> Doc -> Doc
cblock w = block (\s -> replicate ((w - length s) `div` 2) ' ' ++ s) w

-- | Returns the height of a block or other 'Doc'.
height :: Doc -> Int
height = length . lines . render Nothing

chop :: Int -> String -> [String]
chop _ [] = []
chop n cs = case break (=='\n') cs of
                  (xs, ys)     -> if len <= n
                                     then case ys of
                                             []     -> [xs]
                                             (_:[]) -> [xs, ""]
                                             (_:zs) -> xs : chop n zs
                                     else take n xs : chop n (drop n xs ++ ys)
                                   where len = length xs

-- | Encloses a 'Doc' inside a start and end 'Doc'.
inside :: Doc -> Doc -> Doc -> Doc
inside start end contents =
  start <> contents <> end

-- | Puts a 'Doc' in curly braces.
braces :: Doc -> Doc
braces = inside (char '{') (char '}')

-- | Puts a 'Doc' in square brackets.
brackets :: Doc -> Doc
brackets = inside (char '[') (char ']')

-- | Puts a 'Doc' in parentheses.
parens :: Doc -> Doc
parens = inside (char '(') (char ')')

-- | Wraps a 'Doc' in single quotes.
quotes :: Doc -> Doc
quotes = inside (char '\'') (char '\'')

-- | Wraps a 'Doc' in double quotes.
doubleQuotes :: Doc -> Doc
doubleQuotes = inside (char '"') (char '"')
