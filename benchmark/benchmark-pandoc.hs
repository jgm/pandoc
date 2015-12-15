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
import Criterion.Types (Config(..))
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Text.Pandoc.Error

readerBench :: Pandoc
            -> (String, ReaderOptions -> String -> IO (Either PandocError Pandoc))
            -> Maybe Benchmark
readerBench doc (name, reader) =
  case lookup name writers of
       Just (PureStringWriter writer) ->
         let inp = writer def{ writerWrapText = WrapAuto} doc
         in return $ bench (name ++ " reader") $ nfIO $
                 (fmap handleError <$> reader def{ readerSmart = True }) inp
       _ -> trace ("\nCould not find writer for " ++ name ++ "\n") Nothing

writerBench :: Pandoc
            -> (String, WriterOptions -> Pandoc -> String)
            -> Benchmark
writerBench doc (name, writer) = bench (name ++ " writer") $ nf
    (writer def{ writerWrapText = WrapAuto }) doc

main :: IO ()
main = do
  inp <- readFile "tests/testsuite.txt"
  let opts = def{ readerSmart = True }
  let doc = handleError $ readMarkdown opts inp
  let readers' = [(n,r) | (n, StringReader r) <- readers]
  let readerBs = mapMaybe (readerBench doc)
                 $ filter (\(n,_) -> n /="haddock") readers'
  let writers' = [(n,w) | (n, PureStringWriter w) <- writers]
  let writerBs = map (writerBench doc)
                 $ writers'
  defaultMainWith defaultConfig{ timeLimit = 6.0 }
    (writerBs ++ readerBs)
