{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Sources
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Defines Sources object to be used as input to pandoc parsers and redefines Char
parsers so they get source position information from it.
-}

module Text.Pandoc.Sources
  ( Sources(..)
  , ToSources(..)
  , UpdateSourcePos(..)
  , sourcesToText
  , initialSourceName
  , addToSources
  , ensureFinalNewlines
  , addToInput
  , satisfy
  , oneOf
  , noneOf
  , anyChar
  , char
  , string
  , newline
  , space
  , spaces
  , letter
  , digit
  , hexDigit
  , alphaNum
  )
where
import qualified Text.Parsec as P
import Text.Parsec (Stream(..), ParsecT)
import Text.Parsec.Pos as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isLetter, isAlphaNum, isDigit, isHexDigit)
import Data.String (IsString(..))
import qualified Data.List.NonEmpty as NonEmpty

-- | A list of inputs labeled with source positions.  It is assumed
-- that the 'Text's have @\n@ line endings.
newtype Sources = Sources { unSources :: [(SourcePos, Text)] }
  deriving (Show, Semigroup, Monoid)

instance Monad m => Stream Sources m Char where
  uncons (Sources []) = return Nothing
  uncons (Sources ((pos,t):rest)) =
    case T.uncons t of
      Nothing -> uncons (Sources rest)
      Just (c,t') -> return $ Just (c, Sources ((pos,t'):rest))

instance IsString Sources where
  fromString s = Sources [(P.initialPos "", T.pack (filter (/='\r') s))]

class ToSources a where
  toSources :: a -> Sources

instance ToSources Text where
  toSources t = Sources [(P.initialPos "", T.filter (/='\r') t)]

instance ToSources [(FilePath, Text)] where
  toSources = Sources
            . map (\(fp,t) ->
                    (P.initialPos fp, T.snoc (T.filter (/='\r') t) '\n'))

instance ToSources Sources where
  toSources = id

sourcesToText :: Sources -> Text
sourcesToText (Sources xs) = mconcat $ map snd xs

addToSources :: Monad m => SourcePos -> Text -> ParsecT Sources u m ()
addToSources pos t = do
  curpos <- P.getPosition
  Sources xs <- P.getInput
  let xs' = case xs of
               [] -> []
               ((_,t'):rest) -> (curpos,t'):rest
  P.setInput $ Sources ((pos, T.filter (/='\r') t):xs')

ensureFinalNewlines :: Int -- ^ number of trailing newlines
                    -> Sources
                    -> Sources
ensureFinalNewlines n (Sources xs) =
  case NonEmpty.nonEmpty xs of
    Nothing -> Sources [(initialPos "", T.replicate n "\n")]
    Just lst ->
      case NonEmpty.last lst of
        (spos, t) ->
          case T.length (T.takeWhileEnd (=='\n') t) of
            len | len >= n -> Sources xs
                | otherwise -> Sources (NonEmpty.init lst ++
                                        [(spos,
                                          t <> T.replicate (n - len) "\n")])

class UpdateSourcePos s c where
  updateSourcePos :: SourcePos -> c -> s -> SourcePos

instance UpdateSourcePos Text Char where
   updateSourcePos pos c _ = updatePosChar pos c

instance UpdateSourcePos Sources Char where
   updateSourcePos pos c sources =
     case sources of
       Sources [] -> updatePosChar pos c
       Sources ((_,t):(pos',_):_)
         | T.null t  -> pos'
       Sources _ ->
           case c of
             '\n' -> incSourceLine (setSourceColumn pos 1) 1
             '\t' -> incSourceColumn pos (4 - ((sourceColumn pos - 1) `mod` 4))
             _    -> incSourceColumn pos 1

-- | Get name of first source in 'Sources'.
initialSourceName :: Sources -> FilePath
initialSourceName (Sources []) = ""
initialSourceName (Sources ((pos,_):_)) = sourceName pos

-- | Add some text to the beginning of the input sources.
-- This simplifies code that expands macros.
addToInput :: Monad m => Text -> ParsecT Sources u m ()
addToInput t = do
  Sources xs <- P.getInput
  case xs of
    [] -> P.setInput $ Sources [(initialPos "",t)]
    (pos,t'):rest -> P.setInput $ Sources ((pos, t <> t'):rest)

-- We need to redefine the parsers in Text.Parsec.Char so that they
-- update source positions properly from the Sources stream.

satisfy :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
         => (Char -> Bool) -> ParsecT s u m Char
satisfy f = P.tokenPrim show updateSourcePos matcher
 where
  matcher c = if f c then Just c else Nothing

oneOf :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
      => [Char] -> ParsecT s u m Char
oneOf cs = satisfy (`elem` cs)

noneOf :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
       => [Char] -> ParsecT s u m Char
noneOf cs = satisfy (`notElem` cs)

anyChar :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
        => ParsecT s u m Char
anyChar = satisfy (const True)

char :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
     => Char -> ParsecT s u m Char
char c = satisfy (== c)

string :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
       => [Char] -> ParsecT s u m [Char]
string = mapM char

newline :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
        => ParsecT s u m Char
newline = satisfy (== '\n')

space :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
      => ParsecT s u m Char
space = satisfy isSpace

spaces :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
       => ParsecT s u m ()
spaces = P.skipMany space P.<?> "white space"

letter :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
       => ParsecT s u m Char
letter = satisfy isLetter

alphaNum :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
         => ParsecT s u m Char
alphaNum = satisfy isAlphaNum

digit :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
      => ParsecT s u m Char
digit = satisfy isDigit

hexDigit :: (Monad m, Stream s m Char, UpdateSourcePos s Char)
         => ParsecT s u m Char
hexDigit = satisfy isHexDigit
