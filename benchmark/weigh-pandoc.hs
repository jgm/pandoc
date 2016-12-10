import Weigh
import Text.Pandoc

main :: IO ()
main = do
  doc <- read <$> readFile "tests/testsuite.native"
  mainWith $ do
    func "Pandoc document" id doc
    mapM_
      (\(n,r) -> weighReader doc n (either (error . show) id . runPure . r def{ readerSmart = True }))
      [("markdown", readMarkdown)
      ,("html", readHtml)
      ,("docbook", readDocBook)
      ,("latex", readLaTeX)
      ,("commonmark", readCommonMark)
      ]
    mapM_
      (\(n,w) -> weighWriter doc n (either (error . show) id . runPure . w def))
      [("markdown", writeMarkdown)
      ,("html", writeHtmlString)
      ,("docbook", writeDocbook)
      ,("latex", writeLaTeX)
      ,("commonmark", writeCommonMark)
      ]

weighWriter :: Pandoc -> String -> (Pandoc -> String) -> Weigh ()
weighWriter doc name writer = func (name ++ " writer") writer doc

weighReader :: Pandoc -> String -> (String -> Pandoc) -> Weigh ()
weighReader doc name reader = do
  case lookup name writers of
       Just (StringWriter writer) ->
         let inp = either (error . show) id $ runPure $ writer def{ writerWrapText = WrapAuto} doc
         in func (name ++ " reader") reader inp
       _ -> return () -- no writer for reader


