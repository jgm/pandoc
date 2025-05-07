{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Pandoc.URI
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.URI ( urlEncode
                       , escapeURI
                       , isURI
                       , schemes
                       , uriPathToPath
                       , pBase64DataURI
                       ) where
import qualified Network.HTTP.Types as HTTP
import Data.ByteString.Base64 (decodeLenient)
import Text.Pandoc.MIME (MimeType)
import qualified Data.ByteString as B
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Char (isSpace, isAscii)
import Network.URI (URI (uriScheme), parseURI, escapeURIString)
import qualified Data.Attoparsec.Text as A
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative (many)

urlEncode :: T.Text -> T.Text
urlEncode = UTF8.toText . HTTP.urlEncode True . UTF8.fromText

-- | Escape whitespace and some punctuation characters in URI.
escapeURI :: T.Text -> T.Text
escapeURI = T.pack . escapeURIString (not . needsEscaping) . T.unpack
  where needsEscaping c = isSpace c || T.any (== c) "<>|\"{}[]^`"

--
-- IANA URIs
--

-- | Schemes from http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes doi, javascript, isbn, pmid.
schemes :: Set.Set T.Text
schemes = Set.fromList
  -- Official IANA schemes
  [ "aaa", "aaas", "about", "acap", "acct", "acr", "adiumxtra", "afp", "afs"
  , "aim", "appdata", "apt", "attachment", "aw", "barion", "beshare", "bitcoin"
  , "blob", "bolo", "browserext", "callto", "cap", "chrome", "chrome-extension"
  , "cid", "coap", "coaps", "com-eventbrite-attendee", "content", "crid", "cvs"
  , "data", "dav", "dict", "dis", "dlna-playcontainer", "dlna-playsingle"
  , "dns", "dntp", "dtn", "dvb", "ed2k", "example", "facetime", "fax", "feed"
  , "feedready", "file", "filesystem", "finger", "fish", "ftp", "geo", "gg"
  , "git", "gizmoproject", "go", "gopher", "graph", "gtalk", "h323", "ham"
  , "hcp", "http", "https", "hxxp", "hxxps", "hydrazone", "iax", "icap", "icon"
  , "im", "imap", "info", "iotdisco", "ipn", "ipp", "ipps", "irc", "irc6"
  , "ircs", "iris", "iris.beep", "iris.lwz", "iris.xpc", "iris.xpcs"
  , "isostore", "itms", "jabber", "jar", "jms", "keyparc", "lastfm", "ldap"
  , "ldaps", "lvlt", "magnet", "mailserver", "mailto", "maps", "market"
  , "message", "mid", "mms", "modem", "mongodb", "moz", "ms-access"
  , "ms-browser-extension", "ms-drive-to", "ms-enrollment", "ms-excel"
  , "ms-gamebarservices", "ms-getoffice", "ms-help", "ms-infopath"
  , "ms-media-stream-id", "ms-officeapp", "ms-project", "ms-powerpoint"
  , "ms-publisher", "ms-search-repair", "ms-secondary-screen-controller"
  , "ms-secondary-screen-setup", "ms-settings", "ms-settings-airplanemode"
  , "ms-settings-bluetooth", "ms-settings-camera", "ms-settings-cellular"
  , "ms-settings-cloudstorage", "ms-settings-connectabledevices"
  , "ms-settings-displays-topology", "ms-settings-emailandaccounts"
  , "ms-settings-language", "ms-settings-location", "ms-settings-lock"
  , "ms-settings-nfctransactions", "ms-settings-notifications"
  , "ms-settings-power", "ms-settings-privacy", "ms-settings-proximity"
  , "ms-settings-screenrotation", "ms-settings-wifi", "ms-settings-workplace"
  , "ms-spd", "ms-sttoverlay", "ms-transit-to", "ms-virtualtouchpad"
  , "ms-visio", "ms-walk-to", "ms-whiteboard", "ms-whiteboard-cmd", "ms-word"
  , "msnim", "msrp", "msrps", "mtqp", "mumble", "mupdate", "mvn", "news", "nfs"
  , "ni", "nih", "nntp", "notes", "ocf", "oid", "onenote", "onenote-cmd"
  , "opaquelocktoken", "pack", "palm", "paparazzi", "pkcs11", "platform", "pop"
  , "pres", "prospero", "proxy", "pwid", "psyc", "qb", "query", "redis"
  , "rediss", "reload", "res", "resource", "rmi", "rsync", "rtmfp", "rtmp"
  , "rtsp", "rtsps", "rtspu", "secondlife", "service", "session", "sftp", "sgn"
  , "shttp", "sieve", "sip", "sips", "skype", "smb", "sms", "smtp", "snews"
  , "snmp", "soap.beep", "soap.beeps", "soldat", "spotify", "ssh", "steam"
  , "stun", "stuns", "submit", "svn", "tag", "teamspeak", "tel", "teliaeid"
  , "telnet", "tftp", "things", "thismessage", "tip", "tn3270", "tool", "turn"
  , "turns", "tv", "udp", "unreal", "urn", "ut2004", "v-event", "vemmi"
  , "ventrilo", "videotex", "vnc", "view-source", "wais", "webcal", "wpid"
  , "ws", "wss", "wtai", "wyciwyg", "xcon", "xcon-userid", "xfire"
  , "xmlrpc.beep", "xmlrpc.beeps", "xmpp", "xri", "ymsgr", "z39.50", "z39.50r"
  , "z39.50s"
  -- Unofficial schemes
  , "doi", "gemini", "isbn", "javascript", "pmid"
  ]

-- | Check if the string is a valid URL with a IANA or frequently used but
-- unofficial scheme (see @schemes@).
isURI :: T.Text -> Bool
isURI t =
  -- If it's a base 64 data: URI, avoid the expensive call to parseURI:
  case A.parseOnly (pBase64DataURI *> A.endOfInput) t of
    Right () -> True
    Left _ ->
      -- we URI-escape non-ASCII characters because otherwise parseURI will choke:
      maybe False hasKnownScheme . parseURI . escapeURIString isAscii . T.unpack $ t
  where
    hasKnownScheme =
      (`Set.member` schemes) . T.toLower . T.filter (/= ':') . T.pack . uriScheme

-- | Converts the path part of a file: URI to a regular path.
-- On windows, @/c:/foo@ should be @c:/foo@.
-- On linux, @/foo@ should be @/foo@.
uriPathToPath :: T.Text -> FilePath
uriPathToPath (T.unpack -> path) =
#ifdef _WINDOWS
  case path of
    '/':ps -> ps
    ps     -> ps
#else
  path
#endif

pBase64DataURI :: A.Parser (B.ByteString, MimeType)
pBase64DataURI = base64uri
 where
  base64uri = do
    A.string "data:"
    mime <- do
      n1 <- restrictedName
      A.char '/'
      n2 <- restrictedName
      mps <- many mediaParam
      pure $ n1 <> "/" <> n2 <> mconcat mps
    A.string ";base64,"
    b64 <- A.takeWhile (A.inClass "A-Za-z0-9+/")
    A.skipWhile (== '=')
    -- this decode should be lazy:
    pure (decodeLenient (encodeUtf8 b64), mime)
  restrictedName = do
    c <- A.satisfy (A.inClass "A-Za-z0-9")
    rest <- A.takeWhile (A.inClass "A-Za-z0-9!#$&^_.+-")
    pure $ T.singleton c <> rest
  mediaParam = do
    A.char ';'
    A.skipWhile isSpace
    k <- restrictedName
    A.char '='
    v <- A.takeWhile (/=';')
    pure $ ";" <> k <> "=" <> v
