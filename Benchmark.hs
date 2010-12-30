import Text.Pandoc
import Text.Pandoc.Shared (readDataFile, normalize)
import Criterion.Main
import Data.List (isSuffixOf)
import Text.JSON.Generic

readerBench :: Pandoc
            -> (String, ParserState -> String -> Pandoc)
            -> Benchmark
readerBench doc (name, reader) =
  let writer = case lookup name writers of
                     Just w  -> w
                     Nothing -> error $ "Could not find writer for " ++ name
      inp = writer defaultWriterOptions{ writerWrapText = True
                                       , writerLiterateHaskell =
                                          "+lhs" `isSuffixOf` name } doc
      -- we compute the length to force full evaluation
      getLength (Pandoc (Meta a b c) d) =
            length a + length b + length c + length d
  in  bench (name ++ " reader") $ whnf (getLength .
         reader defaultParserState{ stateSmart = True
                                  , stateStandalone = True
                                  , stateLiterateHaskell =
                                      "+lhs" `isSuffixOf` name }) inp

writerBench :: Pandoc
            -> (String, WriterOptions -> Pandoc -> String)
            -> Benchmark
writerBench doc (name, writer) = bench (name ++ " writer") $ nf
    (writer defaultWriterOptions{
                   writerWrapText = True
                  , writerLiterateHaskell = "+lhs" `isSuffixOf` name }) doc

normalizeBench :: Pandoc -> [Benchmark]
normalizeBench doc = [ bench "normalize - with" $ nf (encodeJSON . normalize) doc
                     , bench "normalize - without" $ nf encodeJSON doc
                     ]

main = do
  inp <- readDataFile (Just ".") "README"
  let ps = defaultParserState{ stateSmart = True }
  let doc = readMarkdown ps inp
  let readerBs = map (readerBench doc) readers
  defaultMain $ map (writerBench doc) writers ++ readerBs ++ normalizeBench doc

