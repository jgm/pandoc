{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.UTF8
   Copyright   : Copyright (C) 2010-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

UTF-8 aware string IO functions that will work with GHC 6.10, 6.12, or 7.
-}
module Text.Pandoc.UTF8 ( readFile
                        , getContents
                        , writeFileWith
                        , writeFile
                        , putStrWith
                        , putStr
                        , putStrLnWith
                        , putStrLn
                        , hPutStrWith
                        , hPutStr
                        , hPutStrLnWith
                        , hPutStrLn
                        , hGetContents
                        , toString
                        , toText
                        , fromString
                        , fromText
                        , toStringLazy
                        , fromTextLazy
                        , toTextLazy
                        , fromStringLazy
                        , encodePath
                        , decodeArg
                        )

where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Prelude hiding (getContents, putStr, putStrLn, readFile, writeFile)
import System.IO hiding (getContents, hGetContents, hPutStr, hPutStrLn, putStr,
                  putStrLn, readFile, writeFile)

readFile :: FilePath -> IO Text
readFile f = do
  h <- openFile (encodePath f) ReadMode
  hGetContents h

getContents :: IO Text
getContents = hGetContents stdin

writeFileWith :: Newline -> FilePath -> Text -> IO ()
writeFileWith eol f s =
  withFile (encodePath f) WriteMode $ \h -> hPutStrWith eol h s

writeFile :: FilePath -> Text -> IO ()
writeFile = writeFileWith nativeNewline

putStrWith :: Newline -> Text -> IO ()
putStrWith eol s = hPutStrWith eol stdout s

putStr :: Text -> IO ()
putStr = putStrWith nativeNewline

putStrLnWith :: Newline -> Text -> IO ()
putStrLnWith eol s = hPutStrLnWith eol stdout s

putStrLn :: Text -> IO ()
putStrLn = putStrLnWith nativeNewline

hPutStrWith :: Newline -> Handle -> Text -> IO ()
hPutStrWith eol h s =
  hSetNewlineMode h (NewlineMode eol eol) >>
  hSetEncoding h utf8 >> TIO.hPutStr h s

hPutStr :: Handle -> Text -> IO ()
hPutStr = hPutStrWith nativeNewline

hPutStrLnWith :: Newline -> Handle -> Text -> IO ()
hPutStrLnWith eol h s =
  hSetNewlineMode h (NewlineMode eol eol) >>
  hSetEncoding h utf8 >> TIO.hPutStrLn h s

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn = hPutStrLnWith nativeNewline

hGetContents :: Handle -> IO Text
hGetContents = fmap toText . B.hGetContents

-- | Convert UTF8-encoded ByteString to Text, also
-- removing '\r' characters.
toText :: B.ByteString -> Text
toText = T.decodeUtf8 . filterCRs . dropBOM
  where dropBOM bs =
         if "\xEF\xBB\xBF" `B.isPrefixOf` bs
            then B.drop 3 bs
            else bs
        filterCRs = B.filter (/='\r')

-- | Convert UTF8-encoded ByteString to String, also
-- removing '\r' characters.
toString :: B.ByteString -> String
toString = T.unpack . toText

-- | Convert UTF8-encoded ByteString to Text, also
-- removing '\r' characters.
toTextLazy :: BL.ByteString -> TL.Text
toTextLazy = TL.decodeUtf8 . filterCRs . dropBOM
  where dropBOM bs =
         if "\xEF\xBB\xBF" `BL.isPrefixOf` bs
            then BL.drop 3 bs
            else bs
        filterCRs = BL.filter (/='\r')

-- | Convert UTF8-encoded ByteString to String, also
-- removing '\r' characters.
toStringLazy :: BL.ByteString -> String
toStringLazy = TL.unpack . toTextLazy

fromText :: Text -> B.ByteString
fromText = T.encodeUtf8

fromTextLazy :: TL.Text -> BL.ByteString
fromTextLazy = TL.encodeUtf8

fromString :: String -> B.ByteString
fromString = fromText . T.pack

fromStringLazy :: String -> BL.ByteString
fromStringLazy = fromTextLazy . TL.pack

encodePath :: FilePath -> FilePath
encodePath = id

decodeArg :: String -> String
decodeArg = id
