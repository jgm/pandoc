{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.MIME
   Copyright   : Copyright (C) 2011-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Mime type lookup.
-}
module Text.Pandoc.MIME (
  MimeType,
  getMimeType,
  getMimeTypeDef,
  getCharset,
  extensionFromMimeType,
  mediaCategory ) where
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Tuple (swap)
import qualified Network.Mime
import System.FilePath

type MimeType = T.Text

-- | Determine mime type appropriate for file path.
getMimeType :: FilePath -> Maybe MimeType
getMimeType fp
  -- ODT
  | fp == "layout-cache" =
        Just "application/binary"
  | "Formula-" `isPrefixOf` fp && "/" `isSuffixOf` fp =
        Just "application/vnd.oasis.opendocument.formula"
  -- generic
  | otherwise = M.lookup (T.toLower $ T.drop 1 $ T.pack $ takeExtension fp) mimeTypes

-- | Determime mime type appropriate for file path, defaulting to
-- “application/octet-stream” if nothing else fits.
getMimeTypeDef :: FilePath -> MimeType
getMimeTypeDef = fromMaybe "application/octet-stream" . getMimeType

extensionFromMimeType :: MimeType -> Maybe T.Text
-- few special cases, where there are multiple options:
extensionFromMimeType "text/plain" = Just "txt"
extensionFromMimeType "video/quicktime" = Just "mov"
extensionFromMimeType "video/mpeg" = Just "mpeg"
extensionFromMimeType "video/dv" = Just "dv"
extensionFromMimeType "image/vnd.djvu" = Just "djvu"
extensionFromMimeType "image/tiff" = Just "tiff"
extensionFromMimeType "image/jpeg" = Just "jpg"
extensionFromMimeType "application/xml" = Just "xml"
extensionFromMimeType "application/ogg" = Just "ogg"
extensionFromMimeType mimetype =
  M.lookup (T.takeWhile (/=';') mimetype) reverseMimeTypes
  -- note:  we just look up the basic mime type, dropping the content-encoding etc.

-- | Determine general media category for file path, e.g.
--
-- prop> mediaCategory "foo.jpg" = Just "image"
mediaCategory :: FilePath -> Maybe T.Text
mediaCategory fp = getMimeType fp >>= listToMaybe . T.splitOn "/"

reverseMimeTypes :: M.Map MimeType T.Text
reverseMimeTypes = M.fromList $ map swap mimeTypesList

mimeTypes :: M.Map T.Text MimeType
mimeTypes = M.fromList mimeTypesList

-- | Get the charset from a mime type, if one is present.
getCharset :: MimeType -> Maybe T.Text
getCharset mt =
  let (_,y) = T.breakOn "charset=" mt
   in if T.null y
         then Nothing
         else Just $ T.toUpper $ T.takeWhile (/= ';') $ T.drop 8 y

-- | Collection of common mime types.
-- Except for first entry, list borrowed from
-- <https://github.com/Happstack/happstack-server/blob/master/src/Happstack/Server/FileServe/BuildingBlocks.hs happstack-server>
mimeTypesList :: [(T.Text, MimeType)]
mimeTypesList = M.toList (M.map T.decodeUtf8 Network.Mime.defaultMimeMap) ++
           [("%","application/x-trash")
           ,("323","text/h323")
           ,("alc","chemical/x-alchemy")
           ,("art","image/x-jg")
           ,("asn","chemical/x-ncbi-asn1")
           ,("aso","chemical/x-ncbi-asn1-binary")
           ,("atomsrv","application/atomserv+xml")
           ,("b","chemical/x-molconn-Z")
           ,("bak","application/x-trash")
           ,("bat","application/x-msdos-program")
           ,("bmp","image/x-ms-bmp")
           ,("boo","text/x-boo")
           ,("book","application/x-maker")
           ,("bsd","chemical/x-crossfire")
           ,("c","text/x-csrc")
           ,("c++","text/x-c++src")
           ,("c3d","chemical/x-chem3d")
           ,("cabal","application/x-cabal")
           ,("cac","chemical/x-cache")
           ,("cache","chemical/x-cache")
           ,("cascii","chemical/x-cactvs-binary")
           ,("cbin","chemical/x-cactvs-binary")
           ,("cbz","application/x-cbz")
           ,("cc","text/x-c++src")
           ,("cdf","application/x-cdf")
           ,("cdr","image/x-coreldraw")
           ,("cdt","image/x-coreldrawtemplate")
           ,("cef","chemical/x-cxf")
           ,("cer","chemical/x-cerius")
           ,("chm","chemical/x-chemdraw")
           ,("chrt","application/x-kchart")
           ,("com","application/x-msdos-program")
           ,("cpa","chemical/x-compass")
           ,("cpp","text/x-c++src")
           ,("cpt","image/x-corelphotopaint")
           ,("crl","application/x-pkcs7-crl")
           ,("csf","chemical/x-cache-csf")
           ,("csm","chemical/x-csml")
           ,("ctab","chemical/x-cactvs-binary")
           ,("ctx","chemical/x-ctx")
           ,("cub","chemical/x-gaussian-cube")
           ,("cxf","chemical/x-cxf")
           ,("cxx","text/x-c++src")
           ,("d","text/x-dsrc")
           ,("dat","chemical/x-mopac-input")
           ,("dif","video/dv")
           ,("diff","text/x-diff")
           ,("dl","video/dl")
           ,("dll","application/x-msdos-program")
           ,("dms","application/x-dms")
           ,("dx","chemical/x-jcamp-dx")
           ,("emb","chemical/x-embl-dl-nucleotide")
           ,("embl","chemical/x-embl-dl-nucleotide")
           ,("emf","image/x-emf")
           ,("ent","chemical/x-ncbi-asn1-ascii")

           -- The type used in mime-types is `application/postscript`,
           -- but code in Text.Pandoc.PDF relies on the type being
           -- `application/eps`. Do not remove without updating that
           -- module.
           ,("eps","application/eps")

           ,("fb","application/x-maker")
           ,("fbdoc","application/x-maker")
           ,("fch","chemical/x-gaussian-checkpoint")
           ,("fchk","chemical/x-gaussian-checkpoint")
           ,("frm","application/x-maker")
           ,("fs","text/plain")
           ,("gal","chemical/x-gaussian-log")
           ,("gam","chemical/x-gamess-input")
           ,("gamin","chemical/x-gamess-input")
           ,("gau","chemical/x-gaussian-input")
           ,("gcd","text/x-pcs-gcd")
           ,("gcf","application/x-graphing-calculator")
           ,("gcg","chemical/x-gcg8-sequence")
           ,("gen","chemical/x-genbank")
           ,("gjc","chemical/x-gaussian-input")
           ,("gjf","chemical/x-gaussian-input")
           ,("gl","video/gl")
           ,("glsl","text/plain")
           ,("gpt","chemical/x-mopac-graph")
           ,("gsm","audio/x-gsm")
           ,("h","text/x-chdr")
           ,("h++","text/x-c++hdr")
           ,("hh","text/x-c++hdr")
           ,("hin","chemical/x-hin")
           ,("hpp","text/x-c++hdr")
           ,("hs","text/x-haskell")
           ,("hta","application/hta")
           ,("hxx","text/x-c++hdr")
           ,("ica","application/x-ica")
           ,("icz","text/calendar")
           ,("iii","application/x-iphone")
           ,("inp","chemical/x-gamess-input")
           ,("ins","application/x-internet-signup")
           ,("isp","application/x-internet-signup")
           ,("ist","chemical/x-isostar")
           ,("istr","chemical/x-isostar")
           ,("jdx","chemical/x-jcamp-dx")
           ,("jfif","image/jpeg")
           ,("jmz","application/x-jmol")
           ,("key","application/pgp-keys")
           ,("kil","application/x-killustrator")
           ,("kin","chemical/x-kinemage")
           ,("lhs","text/x-literate-haskell")
           ,("lsf","video/x-la-asf")
           ,("lsx","video/x-la-asf")
           ,("lyx","application/x-lyx")
           ,("lzh","application/x-lzh")
           ,("lzx","application/x-lzx")
           ,("man","application/x-troff-man")
           ,("mcif","chemical/x-mmcif")
           ,("mcm","chemical/x-macmolecule")
           ,("mdb","application/msaccess")
           ,("me","application/x-troff-me")
           ,("mm","application/x-freemind")
           ,("mmd","chemical/x-macromodel-input")
           ,("mmod","chemical/x-macromodel-input")
           ,("moc","text/x-moc")
           ,("mol","chemical/x-mdl-molfile")
           ,("mol2","chemical/x-mol2")
           ,("moo","chemical/x-mopac-out")
           ,("mop","chemical/x-mopac-input")
           ,("mopcrt","chemical/x-mopac-input")
           ,("mpc","chemical/x-mopac-input")
           ,("mpega","audio/mpeg")
           ,("ms","application/x-troff-ms")
           ,("msi","application/x-msi")
           ,("mvb","chemical/x-mopac-vib")
           ,("nwc","application/x-nwc")
           ,("o","application/x-object")
           ,("old","application/x-trash")
           ,("oza","application/x-oz-application")
           ,("pat","image/x-coreldrawpattern")
           ,("patch","text/x-diff")
           ,("pdb","chemical/x-pdb")
           ,("php","application/x-httpd-php")
           ,("php3","application/x-httpd-php3")
           ,("php3p","application/x-httpd-php3-preprocessed")
           ,("php4","application/x-httpd-php4")
           ,("phps","application/x-httpd-php-source")
           ,("pht","application/x-httpd-php")
           ,("phtml","application/x-httpd-php")
           ,("pk","application/x-tex-pk")
           ,("pls","audio/x-scpls")
           ,("pot","text/plain")
           ,("prt","chemical/x-ncbi-asn1-ascii")
           ,("py","text/x-python")
           ,("pyc","application/x-python-code")
           ,("pyo","application/x-python-code")
           ,("qtl","application/x-quicktimeplayer")
           ,("rd","chemical/x-mdl-rdfile")
           ,("rhtml","application/x-httpd-eruby")
           ,("rm","audio/x-pn-realaudio")
           ,("ros","chemical/x-rosdal")
           ,("rxn","chemical/x-mdl-rxnfile")
           ,("sct","text/scriptlet")
           ,("sd","chemical/x-mdl-sdfile")
           ,("sd2","audio/x-sd2")
           ,("sdf","application/vnd.stardivision.math")
           ,("sds","application/vnd.stardivision.chart")
           ,("sgf","application/x-go-sgf")
           ,("sid","audio/prs.sid")
           ,("sik","application/x-trash")
           ,("spc","chemical/x-galactic-spc")
           ,("sw","chemical/x-swissprot")
           ,("swfl","application/x-shockwave-flash")
           ,("taz","application/x-gtar")
           ,("tgf","chemical/x-mdl-tgf")
           ,("tm","text/texmacs")
           ,("ts","text/texmacs")
           ,("tsp","application/dsptype")
           ,("val","chemical/x-ncbi-asn1-binary")
           ,("vmd","chemical/x-vmd")
           ,("vms","chemical/x-vamas-iso14976")
           ,("vrm","x-world/x-vrml")
           ,("vs","text/plain")
           ,("wk","application/x-123")
           ,("wmf","image/x-wmf")
           ,("wmz","application/x-ms-wmz")
           ,("wp5","application/wordperfect5.1")
           ,("wsc","text/scriptlet")
           ,("wz","application/x-wingz")
           ,("xlb","application/vnd.ms-excel")
           ,("xtel","chemical/x-xtel")
           ,("zmt","chemical/x-mopac-input")
           ]
