module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Pandoc
import qualified Text.Pandoc.Parsing as P
import Data.Either (fromRight)
import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Char
import Data.Char -- isUpper etc

sampleMW = T.pack "\
\= sample mediawiki doc =\n\
\\n\
\This is a ''sample'' CamelCase document.\n\
\[https://jmtd.net jon's homepage]\n\
\\n\
\: indented reply\n\
\\n\
\[[Category:Foo]]"


-- this exposes bugs in the Mediawiki reader (level 2 does not get captured as a nested DL)
tinyMW = T.pack "level 0\n:level 1\n::level 2"

parsedMW = (fromRight (error "") . runPure . readMediaWiki def) sampleMW

parseMM = fromRight (error "?") . runPure . readMoinMoin def

main = do
  -- what structure do we get from a Mediawiki doc?
  -- (putStrLn . show) parsedMW
  -- putStrLn "##################################"

  -- what happens to definition list in markdown output?
  ---- the definition list just goes away!
  --(handleError . runPure . writeMediaWiki def) parsedMW >>= TIO.putStrLn
  --putStrLn "##################################"

  sampleMM <- TIO.readFile "testmoin.txt"

  (putStrLn . show . parseMM) sampleMM
  putStrLn "\n##################################\n"

  result <- runIO $
    readMoinMoin def sampleMM >>= writeMarkdown def
  mdwn <- handleError result
  TIO.putStrLn mdwn

-- parser tests
camelWord :: Stream s m Char
          => ParsecT s () m String
camelWord = do
  f    <- upper
  rest <- many1 (satisfy (\c -> isAlphaNum c && not (isUpper c)))
  return (f:rest)
