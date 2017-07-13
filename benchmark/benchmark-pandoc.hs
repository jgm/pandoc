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
import Text.Pandoc.Class hiding (getCurrentTime, trace)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Criterion.Main
import Criterion.Types (Config(..))
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import System.Environment (getArgs)

readerBench :: Pandoc
            -> (String, ReaderOptions -> Text -> Pandoc)
            -> Maybe Benchmark
readerBench doc (name, reader) =
  case lookup name writers of
       Just (TextWriter writer) ->
         let inp = either (error . show) id $ runPure
                       $ writer def{ writerWrapText = WrapAuto} doc
         in return $ bench (name ++ " reader") $ nf
                 (reader def) inp
       _ -> trace ("\nCould not find writer for " ++ name ++ "\n") Nothing

writerBench :: Pandoc
            -> (String, WriterOptions -> Pandoc -> Text)
            -> Benchmark
writerBench doc (name, writer) = bench (name ++ " writer") $ nf
    (writer def{ writerWrapText = WrapAuto }) doc

main :: IO ()
main = do
  args <- getArgs
  let matchReader (n, TextReader _) =
        case args of
             [] -> True
             [x] -> x == n
             (x:y:_) -> x == n && y == "reader"
      matchReader (_, _) = False
  let matchWriter (n, TextWriter _) =
        case args of
             [] -> True
             [x] -> x == n
             (x:y:_) -> x == n && y == "writer"
      matchWriter (_, _) = False
  let matchedReaders = filter matchReader readers
  let matchedWriters = filter matchWriter writers
  inp <- UTF8.toText <$> B.readFile "test/testsuite.txt"
  lalune <- B.readFile "test/lalune.jpg"
  movie <- B.readFile "test/movie.jpg"
  time <- getCurrentTime
  let setupFakeFiles = modifyPureState $ \st -> st{ stFiles =
                        FileTree $ Map.fromList [
                           ("lalune.jpg", FileInfo time lalune),
                           ("movie.jpg", FileInfo time movie)
                           ]}
  let opts = def
  let doc = either (error . show) id $ runPure $ readMarkdown opts inp
  let readers' = [(n, \o d ->
             either (error . show) id $ runPure $ r o d)
                        | (n, TextReader r) <- matchedReaders]
  let readerBs = mapMaybe (readerBench doc)
                 $ filter (\(n,_) -> n /="haddock") readers'
  let writers' = [(n, \o d ->
                   either (error . show) id $ runPure $ setupFakeFiles >> w o d)
                        | (n, TextWriter w) <- matchedWriters]
  let writerBs = map (writerBench doc)
                 $ writers'
  defaultMainWith defaultConfig{ timeLimit = 6.0 }
    (writerBs ++ readerBs)
