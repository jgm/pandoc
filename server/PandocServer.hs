{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module PandocServer
    ( app
    , Params(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Servant
import Text.DocTemplates as DocTemplates
import Text.Pandoc
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Highlighting (lookupHighlightingStyle)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)
import Data.ByteString.Lazy (fromStrict, toStrict, ByteString)
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Default
import Data.Set (Set)
import Skylighting (defaultSyntaxMap)

newtype Blob = Blob ByteString
  deriving (Show, Eq)

instance ToJSON Blob where
  toJSON (Blob bs) = toJSON (encodeBase64 $ toStrict bs)

instance FromJSON Blob where
 parseJSON = withText "Blob" $ \t -> do
   let inp = UTF8.fromText t
   case decodeBase64 inp of
        Right bs -> return $ Blob $ fromStrict bs
        Left _ -> -- treat as regular text
                    return $ Blob $ fromStrict inp

-- This is the data to be supplied by the JSON payload
-- of requests.  Maybe values may be omitted and will be
-- given default values.
data Params = Params
  { text                  :: Text
  , from                  :: Maybe Text
  , to                    :: Maybe Text
  , wrapText              :: Maybe WrapOption
  , columns               :: Maybe Int
  , standalone            :: Maybe Bool
  , template              :: Maybe Text
  , tabStop               :: Maybe Int
  , indentedCodeClasses   :: Maybe [Text]
  , abbreviations         :: Maybe (Set Text)
  , defaultImageExtension :: Maybe Text
  , trackChanges          :: Maybe TrackChanges
  , stripComments         :: Maybe Bool
  , citeproc              :: Maybe Bool
  , variables             :: Maybe (DocTemplates.Context Text)
  , tableOfContents       :: Maybe Bool
  , incremental           :: Maybe Bool
  , htmlMathMethod        :: Maybe HTMLMathMethod
  , numberSections        :: Maybe Bool
  , numberOffset          :: Maybe [Int]
  , sectionDivs           :: Maybe Bool
  , referenceLinks        :: Maybe Bool
  , dpi                   :: Maybe Int
  , emailObfuscation      :: Maybe ObfuscationMethod
  , identifierPrefix      :: Maybe Text
  , citeMethod            :: Maybe CiteMethod
  , htmlQTags             :: Maybe Bool
  , slideLevel            :: Maybe Int
  , topLevelDivision      :: Maybe TopLevelDivision
  , listings              :: Maybe Bool
  , highlightStyle        :: Maybe Text
  , setextHeaders         :: Maybe Bool
  , epubSubdirectory      :: Maybe Text
  , epubFonts             :: Maybe [FilePath]
  , epubMetadata          :: Maybe Text
  , epubChapterLevel      :: Maybe Int
  , tocDepth              :: Maybe Int
  , referenceDoc          :: Maybe FilePath
  , referenceLocation     :: Maybe ReferenceLocation
  , preferAscii           :: Maybe Bool
  , files                 :: Maybe [(FilePath, Blob)]
  } deriving (Show)

instance Default Params where
  def = Params
    { text = ""
    , from = Nothing
    , to = Nothing
    , wrapText = Nothing
    , columns = Nothing
    , standalone = Nothing
    , template = Nothing
    , tabStop = Nothing
    , indentedCodeClasses = Nothing
    , abbreviations = Nothing
    , defaultImageExtension = Nothing
    , trackChanges = Nothing
    , stripComments = Nothing
    , citeproc = Nothing
    , variables = Nothing
    , tableOfContents = Nothing
    , incremental = Nothing
    , htmlMathMethod = Nothing
    , numberSections = Nothing
    , numberOffset = Nothing
    , sectionDivs = Nothing
    , referenceLinks = Nothing
    , dpi = Nothing
    , emailObfuscation = Nothing
    , identifierPrefix = Nothing
    , citeMethod = Nothing
    , htmlQTags = Nothing
    , slideLevel = Nothing
    , topLevelDivision = Nothing
    , listings = Nothing
    , highlightStyle = Nothing
    , setextHeaders = Nothing
    , epubSubdirectory = Nothing
    , epubMetadata = Nothing
    , epubChapterLevel = Nothing
    , epubFonts = Nothing
    , tocDepth = Nothing
    , referenceDoc = Nothing
    , referenceLocation = Nothing
    , preferAscii = Nothing
    , files = Nothing
    }
    -- TODO:
    -- shiftHeadingLevelBy
    -- metadata
    -- selfContained
    -- embedResources
    -- epubCoverImage
    -- stripEmptyParagraphs
    -- titlePrefix
    -- ipynbOutput
    -- eol
    -- csl
    -- bibliography
    -- citationAbbreviations

-- Automatically derive code to convert to/from JSON.
$(deriveJSON defaultOptions ''Params)

-- This is the API.  The "/convert" endpoint takes a request body
-- consisting of a JSON-encoded Params structure and responds to
-- Get requests with either plain text or JSON, depending on the
-- Accept header.
type API =
  ReqBody '[JSON] Params :> Post '[PlainText, JSON] Text
  :<|>
  "batch" :> ReqBody '[JSON] [Params] :> Post '[JSON] [Text]
  :<|>
  "babelmark" :> QueryParam' '[Required] "text" Text :> QueryParam "from" Text :> QueryParam "to" Text :> QueryFlag "standalone" :> Get '[JSON] Value
  :<|>
  "version" :> Get '[PlainText, JSON] Text

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = convert
    :<|> mapM convert
    :<|> babelmark  -- for babelmark which expects {"html": "", "version": ""}
    :<|> pure pandocVersion
 where
  babelmark text' from' to' standalone' = do
    res <- convert def{ text = text',
                        from = from', to = to',
                        standalone = Just standalone' }
    return $ toJSON $ object [ "html" .= res, "version" .= pandocVersion ]

  -- We use runPure for the pandoc conversions, which ensures that
  -- they will do no IO.  This makes the server safe to use.  However,
  -- it will mean that features requiring IO, like RST includes, will not work.
  -- Changing this to
  --    handleErr =<< liftIO (runIO (convert' params))
  -- will allow the IO operations.
  convert params = handleErr $ runPure (convert' params)

  convert' :: PandocMonad m => Params -> m Text
  convert' params = do
    let readerFormat = fromMaybe "markdown" $ from params
    let writerFormat = fromMaybe "html" $ to params
    (readerSpec, readerExts) <- getReader readerFormat
    (writerSpec, writerExts) <- getWriter writerFormat
    let binaryOutput = case writerSpec of
                         ByteStringWriter{} -> True
                         _ -> False
    let isStandalone = fromMaybe binaryOutput (standalone params)
    let toformat = T.toLower $ T.takeWhile isAlphaNum $ writerFormat
    hlStyle <- traverse (lookupHighlightingStyle . T.unpack)
                  $ highlightStyle params
    mbTemplate <- if isStandalone
                     then case template params of
                            Nothing -> Just <$>
                              compileDefaultTemplate toformat
                            Just t  -> Just <$>
                              compileCustomTemplate toformat t
                     else return Nothing
    let readeropts = def{ readerExtensions = readerExts
                        , readerStandalone = isStandalone
                        , readerTabStop = fromMaybe 4 (tabStop params)
                        , readerIndentedCodeClasses = fromMaybe []
                            (indentedCodeClasses params)
                        , readerAbbreviations =
                            fromMaybe mempty (abbreviations params)
                        , readerDefaultImageExtension =
                            fromMaybe mempty (defaultImageExtension params)
                        , readerTrackChanges =
                            fromMaybe AcceptChanges (trackChanges params)
                        , readerStripComments =
                            fromMaybe False (stripComments params)
                        }
    let writeropts =
          def{ writerExtensions = writerExts
             , writerTabStop = fromMaybe 4 (tabStop params)
             , writerWrapText = fromMaybe WrapAuto (wrapText params)
             , writerColumns = fromMaybe 72 (columns params)
             , writerTemplate = mbTemplate
             , writerSyntaxMap = defaultSyntaxMap
             , writerVariables = fromMaybe mempty (variables params)
             , writerTableOfContents = fromMaybe False (tableOfContents params)
             , writerIncremental = fromMaybe False (incremental params)
             , writerHTMLMathMethod =
                 fromMaybe PlainMath (htmlMathMethod params)
             , writerNumberSections = fromMaybe False (numberSections params)
             , writerNumberOffset = fromMaybe [] (numberOffset params)
             , writerSectionDivs = fromMaybe False (sectionDivs params)
             , writerReferenceLinks = fromMaybe False (referenceLinks params)
             , writerDpi = fromMaybe 96 (dpi params)
             , writerEmailObfuscation =
                 fromMaybe NoObfuscation (emailObfuscation params)
             , writerIdentifierPrefix =
                 fromMaybe mempty (identifierPrefix params)
             , writerCiteMethod = fromMaybe Citeproc (citeMethod params)
             , writerHtmlQTags = fromMaybe False (htmlQTags params)
             , writerSlideLevel = slideLevel params
             , writerTopLevelDivision =
                 fromMaybe TopLevelDefault (topLevelDivision params)
             , writerListings = fromMaybe False (listings params)
             , writerHighlightStyle = hlStyle
             , writerSetextHeaders = fromMaybe False (setextHeaders params)
             , writerEpubSubdirectory =
                 fromMaybe "EPUB" (epubSubdirectory params)
             , writerEpubMetadata = epubMetadata params
             , writerEpubFonts = fromMaybe [] (epubFonts params)
             , writerEpubChapterLevel = fromMaybe 1 (epubChapterLevel params)
             , writerTOCDepth = fromMaybe 3 (tocDepth params)
             , writerReferenceDoc = referenceDoc params
             , writerReferenceLocation =
                 fromMaybe EndOfDocument (referenceLocation params)
             , writerPreferAscii = fromMaybe False (preferAscii params)
             }
    let reader = case readerSpec of
                TextReader r -> r readeropts
                ByteStringReader r -> \t -> do
                  let eitherbs = decodeBase64 $ UTF8.fromText t
                  case eitherbs of
                    Left errt -> throwError $ PandocSomeError errt
                    Right bs -> r readeropts $ fromStrict bs
    let writer = case writerSpec of
                TextWriter w -> w writeropts
                ByteStringWriter w -> fmap (encodeBase64 . toStrict) . w writeropts
    reader (text params) >>=
      (if citeproc params == Just True
          then processCitations
          else return) >>=
      writer

  handleErr (Right t) = return t
  handleErr (Left err) = throwError $
    err500 { errBody = TLE.encodeUtf8 $ TL.fromStrict $ renderError err }

  compileCustomTemplate toformat t = do
    res <- runWithPartials $ compileTemplate ("custom." <> T.unpack toformat) t
    case res of
      Left e -> throwError $ PandocTemplateError (T.pack e)
      Right tpl -> return tpl
