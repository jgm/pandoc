{-# LANGUAGE NoImplicitPrelude #-}
import Prelude
import Weigh
import Text.Pandoc
import Data.Text (Text)

main :: IO ()
main = do
  doc <- read <$> readFile "test/testsuite.native"
  mainWith $ do
    func "Pandoc document" id doc
    mapM_
      (\(n,r) -> weighReader doc n (either (error . show) id . runPure . r def{readerExtensions = pandocExtensions}))
      [("markdown", readMarkdown)
      ,("html", readHtml)
      ,("docbook", readDocBook)
      ,("latex", readLaTeX)
      ,("commonmark", readCommonMark)
      ]
    mapM_
      (\(n,w) -> weighWriter doc n (either (error . show) id . runPure . w def))
      [("markdown", writeMarkdown)
      ,("html", writeHtml5String)
      ,("docbook", writeDocbook5)
      ,("latex", writeLaTeX)
      ,("commonmark", writeCommonMark)
      ]

weighWriter :: Pandoc -> String -> (Pandoc -> Text) -> Weigh ()
weighWriter doc name writer = func (name ++ " writer") writer doc

weighReader :: Pandoc -> String -> (Text -> Pandoc) -> Weigh ()
weighReader doc name reader = do
  case lookup name writers of
       Just (TextWriter writer) ->
         let inp = either (error . show) id $ runPure $ writer def{ writerWrapText = WrapAuto} doc
         in func (name ++ " reader") reader inp
       _ -> return () -- no writer for reader


