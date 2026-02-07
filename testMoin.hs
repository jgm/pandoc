module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Pandoc
import Text.Pandoc.Parsing
import Data.Either (fromRight)

sampleMW = T.pack "\
\= sample mediawiki doc =\n\
\\n\
\This is a ''sample'' CamelCase document.\n\
\[https://jmtd.net jon's homepage]\n\
\\n\
\: indented reply\n\
\\n\
\[[Category:Foo]]"

sampleMM = T.pack "\
\#format wiki\n\
\#language en\n\
\#pragma supplementation-page on\n\
\\n\
\== Jon Dowland ==\n\
\\n\
\ * [[http://jmtd.net|jmtd.net]]\n\
\ * another bullet\n\
\\n\
\ ''mostly'' meta '''indeed'''. This is __useful__.\n\
\\n\
\what does `monospace` look like? different to ?\n\
\\n\
\what goes ^up^ must come ,,down,,.\n\
\\n\
\what --(happened in)-- there?\n\
\\n\
\----\n\
\\n\
\CategoryHomepage"

-- this exposes bugs in the Mediawiki reader (level 2 does not get captured as a nested DL)
tinyMW = T.pack "level 0\n:level 1\n::level 2"

parsedMW = (fromRight (error "") . runPure . readMediaWiki def) sampleMW

parsedMM = (fromRight (error "?") . runPure . readMoinMoin def) sampleMM

main = do
  -- what structure do we get from a Mediawiki doc?
  -- (putStrLn . show) parsedMW
  -- putStrLn "##################################"

  -- what happens to definition list in markdown output?
  ---- the definition list just goes away!
  --(handleError . runPure . writeMediaWiki def) parsedMW >>= TIO.putStrLn
  --putStrLn "##################################"

  (putStrLn . show) parsedMM
  putStrLn "\n##################################\n"

  result <- runIO $
    readMoinMoin def sampleMM >>= writeMarkdown def
  mdwn <- handleError result
  TIO.putStrLn mdwn
