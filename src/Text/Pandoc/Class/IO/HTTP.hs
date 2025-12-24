{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Class.IO.HTTP
Copyright   : Copyright (C) 2025 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

HTTP fetching functionality for pandoc.
-}
module Text.Pandoc.Class.IO.HTTP
  ( openURL
  ) where

import Network.URI (URI(..), parseURI)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Class.PandocMonad (PandocMonad, extractURIData)
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.MIME (MimeType)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad.Except (throwError)
#ifdef PANDOC_HTTP_SUPPORT
import Data.ByteString.Lazy (toChunks)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Data.Default (def)
import Network.Connection (TLSSettings(..))
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import System.X509 (getSystemCertificateStore)
import Network.HTTP.Client
       (httpLbs, Manager, responseBody, responseHeaders,
        Request(port, host, requestHeaders), parseUrlThrow, newManager, HttpException)
import Network.HTTP.Client.Internal (addProxy)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Header ( hContentType )
import Network.Socket (withSocketsDo)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad ( getsCommonState, modifyCommonState, report )
import qualified Data.CaseInsensitive as CI
import System.IO.Error
import Text.Pandoc.Logging (LogMessage (..))
import qualified Control.Exception as E
import qualified Text.Pandoc.UTF8 as UTF8
#endif

#ifdef PANDOC_HTTP_SUPPORT
getManager :: (PandocMonad m, MonadIO m) => m Manager
getManager = do
  mbManager <- getsCommonState stManager
  disableCertificateValidation <- getsCommonState stNoCheckCertificate
  case mbManager of
    Just manager -> pure manager
    Nothing -> do
      manager <- liftIO $ do
        certificateStore <- getSystemCertificateStore
        let tlsSettings = TLSSettings $
               (TLS.defaultParamsClient "localhost.localdomain" "80")
                  { TLS.clientSupported = def{ TLS.supportedCiphers =
                                               TLS.ciphersuite_default
                                             , TLS.supportedExtendedMainSecret =
                                                TLS.AllowEMS }
                  , TLS.clientShared = def
                      { TLS.sharedCAStore = certificateStore
                      , TLS.sharedValidationCache =
                          if disableCertificateValidation
                             then TLS.ValidationCache
                                   (\_ _ _ -> return TLS.ValidationCachePass)
                                   (\_ _ _ -> return ())
                             else def
                      }
                  }
        let tlsManagerSettings = mkManagerSettings tlsSettings  Nothing
        newManager tlsManagerSettings
      modifyCommonState $ \st -> st{ stManager = Just manager }
      pure manager
#endif

openURL :: (PandocMonad m, MonadIO m) => Text -> m (B.ByteString, Maybe MimeType)
openURL u
 | Just (URI{ uriScheme = "data:",
              uriPath = upath }) <- parseURI (T.unpack u)
     = pure $ extractURIData upath
#ifdef PANDOC_HTTP_SUPPORT
 | otherwise = do
     let toReqHeader (n, v) = (CI.mk (UTF8.fromText n), UTF8.fromText v)
     customHeaders <- map toReqHeader <$> getsCommonState stRequestHeaders
     report $ Fetching u
     manager <- getManager
     res <- liftIO $ E.try $ withSocketsDo $ do
       proxy <- tryIOError (getEnv "http_proxy")
       let addProxy' x = case proxy of
                            Left _ -> return x
                            Right pr -> parseUrlThrow pr >>= \r ->
                                return (addProxy (host r) (port r) x)
       req <- parseUrlThrow (T.unpack u) >>= addProxy'
       let req' = req{requestHeaders = customHeaders ++ requestHeaders req}
       resp <- httpLbs req' manager
       return (B.concat $ toChunks $ responseBody resp,
               UTF8.toText `fmap` lookup hContentType (responseHeaders resp))

     case res of
          Right r -> return r
          Left (e :: HttpException)
                  -> throwError $ PandocHttpError u (T.pack (show e))
#else
 | otherwise =
     throwError $ PandocHttpError u "pandoc was compiled without HTTP support"
#endif
