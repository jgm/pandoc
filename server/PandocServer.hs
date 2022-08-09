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
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Base64

-- This is the data to be supplied by the JSON payload
-- of requests.  Maybe values may be omitted and will be
-- given default values.
data Params = Params
  { text           :: Text
  , from           :: Maybe Text
  , to             :: Maybe Text
  , wrapText       :: Maybe WrapOption
  , columns        :: Maybe Int
  , standalone     :: Maybe Bool
  , template       :: Maybe Text
  } deriving (Show)

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
  "babelmark" :> QueryParam "text" Text :> QueryParam "from" Text :> QueryParam "to" Text :> QueryFlag "standalone" :> Get '[JSON] Value
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
    res <- convert Params{ text = fromMaybe mempty text',
                           from = from', to = to',
                           standalone = Just standalone', wrapText = Nothing,
                           columns = Nothing, template = Nothing }
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
    let isStandalone = fromMaybe False (standalone params)
    let toformat = T.toLower $ T.takeWhile isAlphaNum $ writerFormat
    mbTemplate <- if isStandalone
                     then case template params of
                            Nothing -> Just <$>
                              compileDefaultTemplate toformat
                            Just t  -> Just <$>
                              compileCustomTemplate toformat t
                     else return Nothing
    let readeropts = def{ readerExtensions = readerExts
                        , readerStandalone = isStandalone }
    let writeropts = def{ writerExtensions = writerExts
                        , writerWrapText = fromMaybe WrapAuto (wrapText params)
                        , writerColumns = fromMaybe 72 (columns params)
                        , writerTemplate = mbTemplate }
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
    reader (text params) >>= writer

  handleErr (Right t) = return t
  handleErr (Left err) = throwError $
    err500 { errBody = TLE.encodeUtf8 $ TL.fromStrict $ renderError err }

  compileCustomTemplate toformat t = do
    res <- runWithPartials $ compileTemplate ("custom." <> T.unpack toformat) t
    case res of
      Left e -> throwError $ PandocTemplateError (T.pack e)
      Right tpl -> return tpl
