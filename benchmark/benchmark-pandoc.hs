{-
Copyright (C) 2012-2014 John MacFarlane <jgm@berkeley.edu>

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
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
import Text.Pandoc
import Criterion.Main
import Criterion.Config
import System.Environment (getArgs)
import Data.Monoid

readerBench :: Pandoc
            -> (String, ReaderOptions -> String -> IO Pandoc)
            -> Benchmark
readerBench doc (name, reader) =
  let writer = case lookup name writers of
                     Just (PureStringWriter w) -> w
                     _ -> error $ "Could not find writer for " ++ name
      inp = writer def{ writerWrapText = True } doc
      -- we compute the length to force full evaluation
      getLength (Pandoc (Meta _) d) = length d
  in  bench (name ++ " reader") $ whnfIO $ getLength `fmap`
      (reader def{ readerSmart = True }) inp

writerBench :: Pandoc
            -> (String, WriterOptions -> Pandoc -> String)
            -> Benchmark
writerBench doc (name, writer) = bench (name ++ " writer") $ nf
    (writer def{ writerWrapText = True }) doc

main :: IO ()
main = do
  args <- getArgs
  (conf,_) <- parseArgs defaultConfig{ cfgSamples = Last $ Just 20 }  defaultOptions args
  inp  <- readFile "README"
  inp2 <- readFile "tests/testsuite.txt"
  let opts = def{ readerSmart = True }
  let doc = readMarkdown opts $ inp ++ unlines (drop 3 $ lines inp2)
  let readerBs = map (readerBench doc)
                 $ filter (\(n,_) -> n /="haddock") readers
  let writers' = [(n,w) | (n, PureStringWriter w) <- writers]
  defaultMainWith conf (return ()) $
    map (writerBench doc) writers' ++ readerBs
