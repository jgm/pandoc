{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Server
    ( app
    , API
    , ServerOpts(..)
    , Params(..)
    , Blob(..)
    , parseServerOptsFromArgs
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Network.Wai
import Servant
import Text.DocTemplates as DocTemplates
import Text.Pandoc
import Text.Pandoc.Writers.Shared (lookupMetaString)
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Highlighting (lookupHighlightingStyle)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Default
import Control.Monad (when, foldM)
import qualified Data.Set as Set
import Skylighting (defaultSyntaxMap)
import qualified Data.Map as M
import Text.Collate.Lang (Lang (..), parseLang)
import System.Console.GetOpt
import System.Environment (getProgName)
import qualified Control.Exception as E
import Text.Pandoc.Shared (safeStrRead, headerShift, filterIpynbOutput,
                           eastAsianLineBreakFilter)
import Text.Pandoc.App ( IpynbOutput (..), Opt(..), defaultOpts )
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.SelfContained (makeSelfContained)
import System.Exit
import GHC.Generics (Generic)

data ServerOpts =
  ServerOpts
    { serverPort    :: Int
    , serverTimeout :: Int }
  deriving (Show)

defaultServerOpts :: ServerOpts
defaultServerOpts = ServerOpts { serverPort = 3030, serverTimeout = 2 }

cliOptions :: [OptDescr (ServerOpts -> IO ServerOpts)]
cliOptions =
  [ Option ['p'] ["port"]
      (ReqArg (\s opts -> case safeStrRead s of
                            Just i -> return opts{ serverPort = i }
                            Nothing ->
                              E.throwIO $ PandocOptionError $ T.pack
                                s <> " is not a number") "NUMBER")
      "port number"
  , Option ['t'] ["timeout"]
      (ReqArg (\s opts -> case safeStrRead s of
                            Just i -> return opts{ serverTimeout = i }
                            Nothing ->
                              E.throwIO $ PandocOptionError $ T.pack
                                s <> " is not a number") "NUMBER")
      "timeout (seconds)"

  , Option ['h'] ["help"]
      (NoArg (\_ -> do
        prg <- getProgName
        let header = "Usage: " <> prg <> " [OPTION...]"
        putStrLn $ usageInfo header cliOptions
        exitWith ExitSuccess))
      "help message"

  , Option ['v'] ["version"]
      (NoArg (\_ -> do
        prg <- getProgName
        putStrLn $ prg <> " " <> T.unpack pandocVersionText
        exitWith ExitSuccess))
      "version info"

  ]

parseServerOptsFromArgs :: [String] -> IO ServerOpts
parseServerOptsFromArgs args = do
  let handleUnknownOpt x = "Unknown option: " <> x
  case getOpt' Permute cliOptions args of
    (os, ns, unrecognizedOpts, es) -> do
      when (not (null es) || not (null unrecognizedOpts)) $
        E.throwIO $ PandocOptionError $ T.pack $
          concat es ++ unlines (map handleUnknownOpt unrecognizedOpts) ++
          ("Try --help for more information.")
      when (not (null ns)) $
        E.throwIO $ PandocOptionError $ T.pack $
                     "Unknown arguments: " <> unwords ns
      foldM (flip ($)) defaultServerOpts os

newtype Blob = Blob BL.ByteString
  deriving (Show, Eq)

instance ToJSON Blob where
  toJSON (Blob bs) = toJSON (encodeBase64 $ BL.toStrict bs)

instance FromJSON Blob where
 parseJSON = withText "Blob" $ \t -> do
   let inp = UTF8.fromText t
   case decodeBase64 inp of
        Right bs -> return $ Blob $ BL.fromStrict bs
        Left _ -> -- treat as regular text
                    return $ Blob $ BL.fromStrict inp

-- This is the data to be supplied by the JSON payload
-- of requests.  Maybe values may be omitted and will be
-- given default values.
data Params = Params
  { options               :: Opt
  , text                  :: Text
  , files                 :: Maybe (M.Map FilePath Blob)
  , citeproc              :: Maybe Bool
  } deriving (Show)

instance Default Params where
  def = Params
    { options = defaultOpts
    , text = mempty
    , files = Nothing
    , citeproc = Nothing
    }

-- Automatically derive code to convert to/from JSON.
instance FromJSON Params where
 parseJSON = withObject "Params" $ \o ->
   Params
     <$> parseJSON (Object o)
     <*> o .: "text"
     <*> o .:? "files"
     <*> o .:? "citeproc"

instance ToJSON Params where
 toJSON params =
   case toJSON (options params) of
     (Object o) -> Object $
       KeyMap.insert "text" (toJSON $ text params)
       . KeyMap.insert "files" (toJSON $ files params)
       . KeyMap.insert "citeproc" (toJSON $ citeproc params)
       $ o
     x -> x

data Message =
  Message
  { verbosity :: Verbosity
  , message   :: Text }
  deriving (Generic, Show)

instance ToJSON Message where
 toEncoding = genericToEncoding defaultOptions

type Base64 = Bool

data Output = Succeeded Text Base64 [Message]
            | Failed Text
  deriving (Generic, Show)

instance ToJSON Output where
  toEncoding (Succeeded o b m) = pairs
    ( "output" .= o  <>
      "base64" .= b  <>
      "messages" .= m )
  toEncoding (Failed errmsg) = pairs
    ( "error" .= errmsg )

-- This is the API.  The "/convert" endpoint takes a request body
-- consisting of a JSON-encoded Params structure and responds to
-- Get requests with either plain text or JSON, depending on the
-- Accept header.
type API =
  ReqBody '[JSON] Params :> Post '[OctetStream] BS.ByteString
  :<|>
  ReqBody '[JSON] Params :> Post '[PlainText] Text
  :<|>
  ReqBody '[JSON] Params :> Post '[JSON] Output
  :<|>
  "batch" :> ReqBody '[JSON] [Params] :> Post '[JSON] [Output]
  :<|>
  "babelmark" :> QueryParam' '[Required] "text" Text :> QueryParam "from" Text :> QueryParam "to" Text :> QueryFlag "standalone" :> Get '[JSON] Value
  :<|>
  "version" :> Get '[PlainText, JSON] Text

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = convertBytes
    :<|> convertText
    :<|> convertJSON
    :<|> mapM convertJSON
    :<|> babelmark  -- for babelmark which expects {"html": "", "version": ""}
    :<|> pure pandocVersionText
 where
  babelmark text' from' to' standalone' = do
    res <- convertText def{
                        text = text',
                        options = defaultOpts{
                          optFrom = from',
                          optTo = to',
                          optStandalone = standalone' }
                      }
    return $ toJSON $ object [ "html" .= res, "version" .= pandocVersion ]

  -- We use runPure for the pandoc conversions, which ensures that
  -- they will do no IO.  This makes the server safe to use.  However,
  -- it will mean that features requiring IO, like RST includes, will not work.
  -- Changing this to
  --    handleErr =<< liftIO (runIO (convert' params))
  -- will allow the IO operations.
  convertText params = handleErr $
    runPure (convert' return (return . encodeBase64 . BL.toStrict) params)

  convertBytes params = handleErr $
    runPure (convert' (return . UTF8.fromText) (return . BL.toStrict) params)

  convertJSON params = handleErrJSON $
    runPure
      (convert'
        (\t -> do
            msgs <- getLog
            return $ Succeeded t False (map toMessage msgs))
        (\bs -> do
            msgs <- getLog
            return $ Succeeded (encodeBase64 (BL.toStrict bs)) True
                               (map toMessage msgs))
        params)

  toMessage m = Message { verbosity = messageVerbosity m
                        , message = showLogMessage m }

  convert' :: (Text -> PandocPure a)
           -> (BL.ByteString -> PandocPure a)
           -> Params -> PandocPure a
  convert' textHandler bsHandler params = do
    curtime <- getCurrentTime
    -- put files params in ersatz file system
    let addFile :: FilePath -> Blob -> FileTree -> FileTree
        addFile fp (Blob lbs) =
          insertInFileTree fp FileInfo{ infoFileMTime = curtime
                                      , infoFileContents = BL.toStrict lbs }
    case files params of
      Nothing -> return ()
      Just fs -> do
        let filetree = M.foldrWithKey addFile mempty fs
        modifyPureState $ \st -> st{ stFiles = filetree }

    let opts = options params
    let readerFormat = fromMaybe "markdown" $ optFrom opts
    let writerFormat = fromMaybe "html" $ optTo opts
    (readerSpec, readerExts) <- getReader readerFormat
    (writerSpec, writerExts) <- getWriter writerFormat

    let isStandalone = optStandalone opts
    let toformat = T.toLower $ T.takeWhile isAlphaNum $ writerFormat
    hlStyle <- traverse (lookupHighlightingStyle . T.unpack)
                  $ optHighlightStyle opts

    mbTemplate <- if isStandalone
                     then case optTemplate opts of
                            Nothing -> Just <$>
                              compileDefaultTemplate toformat
                            Just t  -> Just <$>
                              compileCustomTemplate toformat t
                     else return Nothing

    abbrevs <- Set.fromList . filter (not . T.null) . T.lines . UTF8.toText <$>
                 case optAbbreviations opts of
                      Nothing -> readDataFile "abbreviations"
                      Just f  -> readFileStrict f

    let readeropts = def{ readerExtensions = readerExts
                        , readerStandalone = isStandalone
                        , readerTabStop = optTabStop opts
                        , readerIndentedCodeClasses =
                            optIndentedCodeClasses opts
                        , readerAbbreviations = abbrevs
                        , readerDefaultImageExtension =
                            optDefaultImageExtension opts
                        , readerTrackChanges = optTrackChanges opts
                        , readerStripComments = optStripComments opts
                        }

    let writeropts =
          def{ writerExtensions = writerExts
             , writerTabStop = optTabStop opts
             , writerWrapText = optWrap opts
             , writerColumns = optColumns opts
             , writerTemplate = mbTemplate
             , writerSyntaxMap = defaultSyntaxMap
             , writerVariables = optVariables opts
             , writerTableOfContents = optTableOfContents opts
             , writerIncremental = optIncremental opts
             , writerHTMLMathMethod = optHTMLMathMethod opts
             , writerNumberSections = optNumberSections opts
             , writerNumberOffset = optNumberOffset opts
             , writerSectionDivs = optSectionDivs opts
             , writerReferenceLinks = optReferenceLinks opts
             , writerDpi = optDpi opts
             , writerEmailObfuscation = optEmailObfuscation opts
             , writerIdentifierPrefix = optIdentifierPrefix opts
             , writerCiteMethod = optCiteMethod opts
             , writerHtmlQTags = optHtmlQTags opts
             , writerSlideLevel = optSlideLevel opts
             , writerTopLevelDivision = optTopLevelDivision opts
             , writerListings = optListings opts
             , writerHighlightStyle = hlStyle
             , writerSetextHeaders = optSetextHeaders opts
             , writerEpubSubdirectory = T.pack $ optEpubSubdirectory opts
             , writerEpubMetadata = T.pack <$> optEpubMetadata opts
             , writerEpubFonts = optEpubFonts opts
             , writerEpubChapterLevel = optEpubChapterLevel opts
             , writerTOCDepth = optTOCDepth opts
             , writerReferenceDoc = optReferenceDoc opts
             , writerReferenceLocation = optReferenceLocation opts
             , writerPreferAscii = optAscii opts
             }

    let reader = case readerSpec of
                TextReader r -> r readeropts
                ByteStringReader r -> \t -> do
                  let eitherbs = decodeBase64 $ UTF8.fromText t
                  case eitherbs of
                    Left errt -> throwError $ PandocSomeError errt
                    Right bs -> r readeropts $ BL.fromStrict bs

    let writer d@(Pandoc meta _) = do
          case lookupMetaString "lang" meta of
              ""      -> setTranslations $
                            Lang "en" Nothing (Just "US") [] [] []
              l       -> case parseLang l of
                              Left _   -> report $ InvalidLang l
                              Right l' -> setTranslations l'
          case writerSpec of
                TextWriter w ->
                  w writeropts d >>=
                    (if optEmbedResources opts && htmlFormat (optTo opts)
                        then makeSelfContained
                        else return) >>=
                    textHandler
                ByteStringWriter w ->
                  w writeropts d >>= bsHandler

    let transforms :: Pandoc -> Pandoc
        transforms = (case optShiftHeadingLevelBy opts of
                        0             -> id
                        x             -> headerShift x) .
                   (if extensionEnabled Ext_east_asian_line_breaks
                          readerExts &&
                       not (extensionEnabled Ext_east_asian_line_breaks
                              writerExts &&
                            optWrap opts == WrapPreserve)
                       then eastAsianLineBreakFilter
                       else id) .
                   (case optIpynbOutput opts of
                     IpynbOutputAll  -> id
                     IpynbOutputNone -> filterIpynbOutput Nothing
                     IpynbOutputBest -> filterIpynbOutput (Just $
                       case optTo opts of
                            Just "latex"  -> Format "latex"
                            Just "beamer" -> Format "latex"
                            Nothing       -> Format "html"
                            Just f
                              | htmlFormat (optTo opts) -> Format "html"
                              | otherwise -> Format f))

    let meta =   (case optBibliography opts of
                   [] -> id
                   fs -> setMeta "bibliography" (MetaList
                            (map (MetaString . T.pack) fs))) .
                 maybe id (setMeta "csl" . MetaString . T.pack)
                   (optCSL opts) .
                 maybe id (setMeta "citation-abbreviations" . MetaString .
                              T.pack)
                   (optCitationAbbreviations opts) $
                 optMetadata opts

    let addMetadata m' (Pandoc m bs) = Pandoc (m <> m') bs

    reader (text params) >>=
      return . transforms . addMetadata meta >>=
        (case citeproc params of
          Just True -> processCitations
          _ -> return) >>=
      writer

  htmlFormat :: Maybe Text -> Bool
  htmlFormat Nothing = True
  htmlFormat (Just f) =
    any (`T.isPrefixOf` f)
      ["html","html4","html5","s5","slidy", "slideous","dzslides","revealjs"]

  handleErr (Right t) = return t
  handleErr (Left err) = throwError $
    err500 { errBody = TLE.encodeUtf8 $ TL.fromStrict $ renderError err }

  handleErrJSON (Right o) = return o
  handleErrJSON (Left err) =
    return $ Failed (renderError err)

  compileCustomTemplate toformat t = do
    res <- runWithPartials $ compileTemplate ("custom." <> T.unpack toformat)
               (T.pack t)
    case res of
      Left e -> throwError $ PandocTemplateError (T.pack e)
      Right tpl -> return tpl
