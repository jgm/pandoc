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

