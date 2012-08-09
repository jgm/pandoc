import Text.Pandoc
import Text.Pandoc.Shared (readDataFile, normalize)
import Criterion.Main
import Criterion.Config
import Text.JSON.Generic
import System.Environment (getArgs)
import Data.Monoid

readerBench :: Pandoc
            -> (String, ReaderOptions -> String -> Pandoc)
            -> Benchmark
readerBench doc (name, reader) =
  let writer = case lookup name writers of
                     Just (PureStringWriter w) -> w
                     _ -> error $ "Could not find writer for " ++ name
      inp = writer def{ writerWrapText = True } doc
      -- we compute the length to force full evaluation
      getLength (Pandoc (Meta a b c) d) =
            length a + length b + length c + length d
  in  bench (name ++ " reader") $ whnf (getLength .
         reader def{ readerSmart = True }) inp

writerBench :: Pandoc
            -> (String, WriterOptions -> Pandoc -> String)
            -> Benchmark
writerBench doc (name, writer) = bench (name ++ " writer") $ nf
    (writer def{ writerWrapText = True }) doc

normalizeBench :: Pandoc -> [Benchmark]
normalizeBench doc = [ bench "normalize - with" $ nf (encodeJSON . normalize) doc
                     , bench "normalize - without" $ nf encodeJSON doc
                     ]

main :: IO ()
main = do
  args <- getArgs
  (conf,_) <- parseArgs defaultConfig{ cfgSamples = Last $ Just 20 }  defaultOptions args
  inp <- readDataFile (Just ".") "README"
  inp2 <- readDataFile (Just ".") "tests/testsuite.txt"
  let opts = def{ readerSmart = True }
  let doc = readMarkdown opts $ inp ++ unlines (drop 3 $ lines inp2)
  let readerBs = map (readerBench doc) readers
  let writers' = [(n,w) | (n, PureStringWriter w) <- writers]
  defaultMainWith conf (return ()) $
    map (writerBench doc) writers' ++ readerBs ++ normalizeBench doc

