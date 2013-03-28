-- This code was copied from the 'haddock' package, modified, and integrated
-- into Pandoc by David Lazar.
{
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Text.Pandoc.Readers.Haddock.Parse (parseString, parseParas) where

import Text.Pandoc.Readers.Haddock.Lex
import Text.Pandoc.Builder
import Data.Generics (everywhere, mkT)
import Data.Char  (isSpace)
import Data.Maybe (fromMaybe)
import Data.List  (stripPrefix)
import Data.Monoid (mempty)
}

%expect 0

%tokentype  { LToken }

%token
    '/'     { (TokSpecial '/',_) }
    '@'     { (TokSpecial '@',_) }
    '['     { (TokDefStart,_) }
    ']'     { (TokDefEnd,_) }
    DQUO    { (TokSpecial '\"',_) }
    URL     { (TokURL $$,_) }
    PIC     { (TokPic $$,_) }
    ANAME   { (TokAName $$,_) }
    '/../'  { (TokEmphasis $$,_) }
    '-'     { (TokBullet,_) }
    '(n)'   { (TokNumber,_) }
    '>..'   { (TokBirdTrack $$,_) }
    PROP    { (TokProperty $$,_) }
    PROMPT  { (TokExamplePrompt $$,_) }
    RESULT  { (TokExampleResult $$,_) }
    EXP     { (TokExampleExpression $$,_) }
    IDENT   { (TokIdent $$,_) }
    PARA    { (TokPara,_) }
    STRING  { (TokString $$,_) }

%monad { Maybe }

%name parseParas doc
%name parseString seq

%%

doc :: { Blocks }
    : apara PARA doc    { $1 <> $3 }
    | PARA doc          { $2 }
    | apara             { $1 }
    | {- empty -}       { mempty }

apara :: { Blocks }
    : ulpara            { bulletList [$1] }
    | olpara            { orderedList [$1] }
    | defpara           { definitionList [$1] }
    | para              { $1 }

ulpara :: { Blocks }
    : '-' para          { $2 }

olpara  :: { Blocks }
    : '(n)' para        { $2 }

defpara :: { (Inlines, [Blocks]) }
    : '[' seq ']' seq   { ($2, [plain $4]) }

para :: { Blocks }
    : seq               { para $1 }
    | codepara          { codeBlock $1 }
    | property          { $1 }
    | examples          { $1 }

codepara :: { String }
    : '>..' codepara    { $1 ++ $2 }
    | '>..'             { $1 }

property :: { Blocks }
    : PROP              { makeProperty $1 }

examples :: { Blocks }
    : example examples  { $1 <> $2 }
    | example           { $1 }

example :: { Blocks }
    : PROMPT EXP result { makeExample $1 $2 (lines $3) }
    | PROMPT EXP        { makeExample $1 $2 [] }

result :: { String }
    : RESULT result     { $1 ++ $2 }
    | RESULT            { $1 }

seq :: { Inlines }
    : elem seq          { $1 <> $2 }
    | elem              { $1 }

elem :: { Inlines }
    : elem1             { $1 }
    | '@' seq1 '@'      { monospace $2 }

seq1 :: { Inlines }
    : PARA seq1         { linebreak <> $2 }
    | elem1 seq1        { $1 <> $2 }
    | elem1             { $1 }

elem1 :: { Inlines }
    : STRING            { str $1 }
    | '/../'            { emph (str $1) }
    | URL               { makeHyperlink $1 }
    | PIC               { image $1 $1 mempty }
    | ANAME             { mempty } -- TODO
    | IDENT             { code $1 }
    | DQUO strings DQUO { code $2 }

strings :: { String }
    : STRING            { $1 }
    | STRING strings    { $1 ++ $2 }

{
happyError :: [LToken] -> Maybe a
happyError toks = Nothing

monospace :: Inlines -> Inlines 
monospace = everywhere (mkT go)
  where
    go (Str s) = Code nullAttr s
    go Space = Code nullAttr " "
    go x = x

-- | Create a `Hyperlink` from given string.
--
-- A hyperlink consists of a URL and an optional label.  The label is separated
-- from the url by one or more whitespace characters.
makeHyperlink :: String -> Inlines
makeHyperlink input = case break isSpace $ strip input of
  (url, "") -> link url url (str url)
  (url, lb) -> link url url (str label)
    where label = dropWhile isSpace lb

makeProperty :: String -> Blocks
makeProperty s = case strip s of
  'p':'r':'o':'p':'>':xs ->
    codeBlockWith ([], ["property"], []) (dropWhile isSpace xs)
  xs ->
    error $ "makeProperty: invalid input " ++ show xs

-- | Create an 'Example', stripping superfluous characters as appropriate
makeExample :: String -> String -> [String] -> Blocks
makeExample prompt expression result =
    para $ codeWith ([], ["expr"], []) (strip expression ++ "\n")
        <> codeWith ([], ["result"], []) (unlines result')
  where
    -- 1. drop trailing whitespace from the prompt, remember the prefix
    (prefix, _) = span isSpace prompt

    -- 2. drop, if possible, the exact same sequence of whitespace
    -- characters from each result line
    --
    -- 3. interpret lines that only contain the string "<BLANKLINE>" as an
    -- empty line
    result' = map (substituteBlankLine . tryStripPrefix prefix) result
      where
        tryStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine line          = line

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
}
