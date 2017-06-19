{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai.Handler.CGI
import Network.Wai
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe, fromMaybe)
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.URI (queryToQueryText)
import Text.Pandoc
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Readers (getReader, Reader(..))
import Text.Pandoc.Writers (getWriter, Writer(..))
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Shared (tabFilter)
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)

main :: IO ()
main = run app

app :: Application
app req respond = do
  let query = queryToQueryText $ queryString req
  let getParam x = maybe (error $ T.unpack x ++ " paramater not set")
                       return $ lookup x query
  text <- getParam "text" >>= checkLength . fromMaybe T.empty
  fromFormat <- fromMaybe "" <$> getParam "from"
  toFormat <- fromMaybe "" <$> getParam "to"
  let reader = case getReader (T.unpack fromFormat) of
                    Right (TextReader r) -> r readerOpts
                    _ -> error $ "could not find reader for "
                                  ++ T.unpack fromFormat
  let writer = case getWriter (T.unpack toFormat) of
                    Right (TextWriter w) -> w writerOpts
                    _ -> error $ "could not find writer for " ++
                           T.unpack toFormat
  let result = case runPure $ reader (tabFilter 4 text) >>= writer of
                    Right s   -> s
                    Left  err -> error (show err)
  let output = encode $ object [ T.pack "html" .= result
                               , T.pack "name" .=
                                  if fromFormat == "markdown_strict"
                                     then T.pack "pandoc (strict)"
                                     else T.pack "pandoc"
                               , T.pack "version" .= pandocVersion]
  respond $ responseLBS status200 [(hContentType,"text/json; charset=UTF-8")] output

checkLength :: Text -> IO Text
checkLength t =
  if T.length t > 10000
     then error "exceeds length limit of 10,000 characters"
     else return t

writerOpts :: WriterOptions
writerOpts = def { writerReferenceLinks = True,
                   writerEmailObfuscation = NoObfuscation,
                   writerHTMLMathMethod = MathJax (defaultMathJaxURL ++
                       "MathJax.js?config=TeX-AMS_CHTML-full"),
                   writerHighlightStyle = Just pygments }

readerOpts :: ReaderOptions
readerOpts = def
