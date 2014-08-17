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
import Text.Pandoc.Shared (tabFilter)
import Text.Highlighting.Kate (pygments)
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
  reader <- maybe (error $ "could not find reader for " ++ T.unpack fromFormat)              return
             $ lookup fromFormat fromFormats
  let writer = maybe (error $ "could not find writer for " ++ T.unpack toFormat) id
             $ lookup toFormat toFormats
  let result = T.pack $ writer $ reader $ tabFilter 4 $ T.unpack text
  let output = encode $ object [ T.pack "result" .= result
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
                   writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML",
                   writerHighlight = True,
                   writerHighlightStyle = pygments }

readerOpts :: ReaderOptions
readerOpts = def { readerParseRaw = True,
                   readerSmart = True }

fromFormats :: [(Text, String -> Pandoc)]
fromFormats = [
            ("native"       , readNative)
           ,("json"         , Text.Pandoc.readJSON readerOpts)
           ,("markdown"     , readMarkdown readerOpts)
           ,("markdown_strict" , readMarkdown readerOpts{
                    readerExtensions = strictExtensions,
                    readerSmart = False })
           ,("markdown_phpextra" , readMarkdown readerOpts{
                    readerExtensions = phpMarkdownExtraExtensions })
           ,("markdown_github" , readMarkdown readerOpts{
                    readerExtensions = githubMarkdownExtensions })
           ,("markdown_mmd",  readMarkdown readerOpts{
                    readerExtensions = multimarkdownExtensions })
           ,("rst"          , readRST readerOpts)
           ,("mediawiki"    , readMediaWiki readerOpts)
           ,("docbook"      , readDocBook readerOpts)
           ,("opml"         , readOPML readerOpts)
           ,("t2t"          , readTxt2TagsNoMacros readerOpts)
           ,("org"          , readOrg readerOpts)
           ,("textile"      , readTextile readerOpts) -- TODO : textile+lhs
           ,("html"         , readHtml readerOpts)
           ,("latex"        , readLaTeX readerOpts)
           ,("haddock"      , readHaddock readerOpts)
           ]

toFormats :: [(Text, Pandoc -> String)]
toFormats = mapMaybe (\(x,y) ->
                       case y of
                         PureStringWriter w -> Just (T.pack x, w writerOpts{
                             writerExtensions =
                               case x of
                                  "markdown_strict" -> strictExtensions
                                  "markdown_phpextra" -> phpMarkdownExtraExtensions
                                  "markdown_mmd" -> multimarkdownExtensions
                                  "markdown_github" -> githubMarkdownExtensions
                                  _ -> pandocExtensions
                                 })
                         _                  ->
                            case x of
                                 "rtf"     -> Just (T.pack x, writeRTF writerOpts)
                                 _         -> Nothing) writers

