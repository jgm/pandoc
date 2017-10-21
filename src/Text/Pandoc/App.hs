{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App (
            convertWithOpts
          , Opt(..)
          , LineEnding(..)
          , Filter(..)
          , defaultOpts
          , parseOptions
          , options
          , applyFilters
          ) where
import Prelude
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.YAML as YAML
import Network.URI (URI (..), parseURI)
import Skylighting (defaultSyntaxMap)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (nativeNewline, stdout)
import qualified System.IO as IO (Newline (..))
import Text.Pandoc
import Text.Pandoc.App.CommandLineOptions (Opt (..), LineEnding (..),
        defaultOpts, engines, parseOptions, options)
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Builder (setMeta, deleteMeta)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), applyFilters)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.SelfContained (makeDataURI, makeSelfContained)
import Text.Pandoc.Shared (eastAsianLineBreakFilter, stripEmptyParagraphs,
         headerShift, isURI, tabFilter, uriPathToPath)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif

pdfIsNoWriterErrorMsg :: String
pdfIsNoWriterErrorMsg =
  "To create a pdf using pandoc, use " ++
  "-t latex|beamer|context|ms|html5" ++
  "\nand specify an output file with " ++
  ".pdf extension (-o filename.pdf)."

pdfWriterAndProg :: Maybe String              -- ^ user-specified writer name
                 -> Maybe String              -- ^ user-specified pdf-engine
                 -> IO (String, Maybe String) -- ^ IO (writerName, maybePdfEngineProg)
pdfWriterAndProg mWriter mEngine = do
  let panErr msg = liftIO $ E.throwIO $ PandocAppError msg
  case go mWriter mEngine of
      Right (writ, prog) -> return (writ, Just prog)
      Left err           -> panErr err
    where
      go Nothing Nothing       = Right ("latex", "pdflatex")
      go (Just writer) Nothing = (writer,) <$> engineForWriter writer
      go Nothing (Just engine) = (,engine) <$> writerForEngine (takeBaseName engine)
      go (Just writer) (Just engine) =
           case find (== (baseWriterName writer, takeBaseName engine)) engines of
                Just _  -> Right (writer, engine)
                Nothing -> Left $ "pdf-engine " ++ engine ++
                           " is not compatible with output format " ++ writer

      writerForEngine eng = case [f | (f,e) <- engines, e == eng] of
                                 fmt : _ -> Right fmt
                                 []      -> Left $
                                   "pdf-engine " ++ eng ++ " not known"

      engineForWriter "pdf" = Left pdfIsNoWriterErrorMsg
      engineForWriter w = case [e |  (f,e) <- engines, f == baseWriterName w] of
                                eng : _ -> Right eng
                                []      -> Left $
                                   "cannot produce pdf output from " ++ w

convertWithOpts :: Opt -> IO ()
convertWithOpts opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
       exitSuccess

  epubMetadata <- case optEpubMetadata opts of
                         Nothing -> return Nothing
                         Just fp -> Just <$> UTF8.readFile fp

  let isPandocCiteproc (JSONFilter f) = takeBaseName f == "pandoc-citeproc"
      isPandocCiteproc _              = False
  -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
  let needsCiteproc = isJust (lookup "bibliography" (optMetadata opts)) &&
                      optCiteMethod opts `notElem` [Natbib, Biblatex] &&
                      all (not . isPandocCiteproc) filters
  let filters' = if needsCiteproc then JSONFilter "pandoc-citeproc" : filters
                                  else filters

  let sources = case optInputFiles opts of
                     []  -> ["-"]
                     xs | optIgnoreArgs opts -> ["-"]
                        | otherwise  -> xs

  datadir <- case optDataDir opts of
                  Nothing   -> E.catch
                                 (Just <$> getAppUserDataDirectory "pandoc")
                                 (\e -> let _ = (e :: E.SomeException)
                                        in  return Nothing)
                  Just _    -> return $ optDataDir opts

  -- assign reader and writer based on options and filenames
  let readerName = case optReader opts of
                     Just f  -> f
                     Nothing -> formatFromFilePaths fallback sources
                       where fallback = if any isURI sources
                                           then "html"
                                           else "markdown"

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"

  (writerName, maybePdfProg) <-
    if pdfOutput
       then pdfWriterAndProg (optWriter opts) (optPdfEngine opts)
       else case optWriter opts of
              Nothing  ->
                return (formatFromFilePaths "html" [outputFile], Nothing)
              Just f   -> return (f, Nothing)

  let format = map toLower $ baseWriterName
                 $ takeFileName writerName  -- in case path to lua script

  -- disabling the custom writer for now
  (writer, writerExts) <-
            if ".lua" `isSuffixOf` format
               then return (TextWriter
                       (\o d -> writeCustom writerName o d)
                               :: Writer PandocIO, mempty)
               else case getWriter (map toLower writerName) of
                         Left e  -> E.throwIO $ PandocAppError $
                           if format == "pdf"
                              then e ++ "\n" ++ pdfIsNoWriterErrorMsg
                              else e
                         Right (w, es) -> return (w :: Writer PandocIO, es)

  -- TODO: we have to get the input and the output into the state for
  -- the sake of the text2tags reader.
  (reader, readerExts) <-
           case getReader readerName of
                Right (r, es) -> return (r :: Reader PandocIO, es)
                Left e   -> E.throwIO $ PandocAppError e'
                  where e' = case readerName of
                                  "pdf" -> e ++
                                     "\nPandoc can convert to PDF, but not from PDF."
                                  "doc" -> e ++
                                     "\nPandoc can convert from DOCX, but not from DOC.\nTry using Word to save your DOC file as DOCX, and convert that with pandoc."
                                  _ -> e

  let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput
  let addStringAsVariable varname s vars = return $ (varname, s) : vars

  let addSyntaxMap existingmap f = do
        res <- parseSyntaxDefinition f
        case res of
              Left errstr -> E.throwIO $ PandocSyntaxMapError errstr
              Right syn   -> return $ addSyntaxDefinition syn existingmap

  syntaxMap <- foldM addSyntaxMap defaultSyntaxMap
                     (optSyntaxDefinitions opts)

  -- We don't want to send output to the terminal if the user
  -- does 'pandoc -t docx input.txt'; though we allow them to
  -- force this with '-o -'.  On posix systems, we detect
  -- when stdout is being piped and allow output to stdout
  -- in that case, but on Windows we can't.
#ifdef _WINDOWS
  let istty = True
#else
  istty <- queryTerminal stdOutput
#endif
  when (not (isTextFormat format) && istty && isNothing ( optOutputFile opts)) $
    E.throwIO $ PandocAppError $
            "Cannot write " ++ format ++ " output to terminal.\n" ++
            "Specify an output file using the -o option, or " ++
            "use '-o -' to force output to stdout."

  let convertTabs = tabFilter (if optPreserveTabs opts ||
                                    readerName == "t2t" ||
                                    readerName == "man"
                                  then 0
                                  else optTabStop opts)

      readSources :: [FilePath] -> PandocIO Text
      readSources srcs = convertTabs . T.intercalate (T.pack "\n") <$>
                            mapM readSource srcs

  let runIO' :: PandocIO a -> IO a
      runIO' f = do
        (res, reports) <- runIOorExplode $ do
                             setTrace (optTrace opts)
                             setVerbosity verbosity
                             x <- f
                             rs <- getLog
                             return (x, rs)
        case optLogFile opts of
             Nothing      -> return ()
             Just logfile -> B.writeFile logfile (encodeLogMessages reports)
        let isWarning msg = messageVerbosity msg == WARNING
        when (optFailIfWarnings opts && any isWarning reports) $
            E.throwIO PandocFailOnWarningError
        return res

  let eol = case optEol opts of
                 CRLF   -> IO.CRLF
                 LF     -> IO.LF
                 Native -> nativeNewline

  -- note: this reverses the list constructed in option parsing,
  -- which in turn was reversed from the command-line order,
  -- so we end up with the correct order in the variable list:
  let withList _ [] vars     = return vars
      withList f (x:xs) vars = f x vars >>= withList f xs

  let addContentsAsVariable varname fp vars = do
             s <- UTF8.toString <$> readFileStrict fp
             return $ (varname, s) : vars

  runIO' $ do
    setUserDataDir datadir
    setInputFiles (optInputFiles opts)
    setOutputFile (optOutputFile opts)

    variables <-
        withList (addStringAsVariable "sourcefile")
                 (reverse $ optInputFiles opts)
                 (("outputfile", fromMaybe "-" (optOutputFile opts))
                  : optVariables opts)
                 -- we reverse this list because, unlike
                 -- the other option lists here, it is
                 -- not reversed when parsed from CLI arguments.
                 -- See withList, above.
        >>=
        withList (addContentsAsVariable "include-before")
                 (optIncludeBeforeBody opts)
        >>=
        withList (addContentsAsVariable "include-after")
                 (optIncludeAfterBody opts)
        >>=
        withList (addContentsAsVariable "header-includes")
                 (optIncludeInHeader opts)
        >>=
        withList (addStringAsVariable "css") (optCss opts)
        >>=
        maybe return (addStringAsVariable "title-prefix")
                     (optTitlePrefix opts)
        >>=
        maybe return (addStringAsVariable "epub-cover-image")
                     (optEpubCoverImage opts)
        >>=
        (\vars ->  if format == "dzslides"
                      then do
                          dztempl <- UTF8.toString <$> readDataFile
                                       ("dzslides" </> "template.html")
                          let dzline = "<!-- {{{{ dzslides core"
                          let dzcore = unlines
                                     $ dropWhile (not . (dzline `isPrefixOf`))
                                     $ lines dztempl
                          return $ ("dzslides-core", dzcore) : vars
                      else return vars)

    abbrevs <- Set.fromList . filter (not . null) . lines <$>
               case optAbbreviations opts of
                    Nothing -> UTF8.toString <$> readDataFile "abbreviations"
                    Just f  -> UTF8.toString <$> readFileStrict f

    templ <- case optTemplate opts of
                    _ | not standalone -> return Nothing
                    Nothing -> Just <$> getDefaultTemplate format
                    Just tp -> do
                      -- strip off extensions
                      let tp' = case takeExtension tp of
                                     "" -> tp <.> format
                                     _  -> tp
                      Just . UTF8.toString <$>
                            ((fst <$> fetchItem tp') `catchError`
                             (\e ->
                                 case e of
                                      PandocResourceNotFound _ ->
                                         readDataFile ("templates" </> tp')
                                      _ -> throwError e))

    metadata <- if format == "jats" &&
                   isNothing (lookup "csl" (optMetadata opts)) &&
                   isNothing (lookup "citation-style" (optMetadata opts))
                   then do
                     jatsCSL <- readDataFile "jats.csl"
                     let jatsEncoded = makeDataURI
                                         ("application/xml", jatsCSL)
                     return $ ("csl", jatsEncoded) : optMetadata opts
                   else return $ optMetadata opts
    metadataFromFile <-
      case optMetadataFile opts of
        Nothing   -> return mempty
        Just file -> readFileLazy file >>= yamlToMeta

    case lookup "lang" (optMetadata opts) of
           Just l  -> case parseBCP47 l of
                           Left _   -> return ()
                           Right l' -> setTranslations l'
           Nothing -> setTranslations $ Lang "en" "" "US" []

    let writerOptions = def {
            writerTemplate         = templ
          , writerVariables        = variables
          , writerTabStop          = optTabStop opts
          , writerTableOfContents  = optTableOfContents opts
          , writerHTMLMathMethod   = optHTMLMathMethod opts
          , writerIncremental      = optIncremental opts
          , writerCiteMethod       = optCiteMethod opts
          , writerNumberSections   = optNumberSections opts
          , writerNumberOffset     = optNumberOffset opts
          , writerSectionDivs      = optSectionDivs opts
          , writerExtensions       = writerExts
          , writerReferenceLinks   = optReferenceLinks opts
          , writerReferenceLocation = optReferenceLocation opts
          , writerDpi              = optDpi opts
          , writerWrapText         = optWrapText opts
          , writerColumns          = optColumns opts
          , writerEmailObfuscation = optEmailObfuscation opts
          , writerIdentifierPrefix = optIdentifierPrefix opts
          , writerHtmlQTags        = optHtmlQTags opts
          , writerTopLevelDivision = optTopLevelDivision opts
          , writerListings         = optListings opts
          , writerSlideLevel       = optSlideLevel opts
          , writerHighlightStyle   = optHighlightStyle opts
          , writerSetextHeaders    = optSetextHeaders opts
          , writerEpubSubdirectory = optEpubSubdirectory opts
          , writerEpubMetadata     = epubMetadata
          , writerEpubFonts        = optEpubFonts opts
          , writerEpubChapterLevel = optEpubChapterLevel opts
          , writerTOCDepth         = optTOCDepth opts
          , writerReferenceDoc     = optReferenceDoc opts
          , writerSyntaxMap        = syntaxMap
          , writerPreferAscii      = optAscii opts
          }

    let readerOpts = def{
            readerStandalone = standalone
          , readerColumns = optColumns opts
          , readerTabStop = optTabStop opts
          , readerIndentedCodeClasses = optIndentedCodeClasses opts
          , readerDefaultImageExtension =
             optDefaultImageExtension opts
          , readerTrackChanges = optTrackChanges opts
          , readerAbbreviations = abbrevs
          , readerExtensions = readerExts
          , readerStripComments = optStripComments opts
          }

    let transforms = (case optBaseHeaderLevel opts of
                          x | x > 1     -> (headerShift (x - 1) :)
                            | otherwise -> id) .
                     (if optStripEmptyParagraphs opts
                         then (stripEmptyParagraphs :)
                         else id) .
                     (if extensionEnabled Ext_east_asian_line_breaks
                            readerExts &&
                         not (extensionEnabled Ext_east_asian_line_breaks
                              writerExts &&
                              writerWrapText writerOptions == WrapPreserve)
                         then (eastAsianLineBreakFilter :)
                         else id) $
                     []

    let sourceToDoc :: [FilePath] -> PandocIO Pandoc
        sourceToDoc sources' =
           case reader of
                TextReader r
                  | optFileScope opts || readerName == "json" ->
                      mconcat <$> mapM (readSource >=> r readerOpts) sources
                  | otherwise ->
                      readSources sources' >>= r readerOpts
                ByteStringReader r ->
                  mconcat <$> mapM (readFile' >=> r readerOpts) sources


    when (readerName == "markdown_github" ||
          writerName == "markdown_github") $
      report $ Deprecated "markdown_github" "Use gfm instead."

    setResourcePath (optResourcePath opts)
    mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

    doc <- sourceToDoc sources >>=
              (   (if isJust (optExtractMedia opts)
                      then fillMediaBag
                      else return)
              >=> return . addNonPresentMetadata metadataFromFile
              >=> return . addMetadata metadata
              >=> applyTransforms transforms
              >=> applyFilters readerOpts filters' [format]
              >=> maybe return extractMedia (optExtractMedia opts)
              )

    case writer of
      ByteStringWriter f -> f writerOptions doc >>= writeFnBinary outputFile
      TextWriter f -> case maybePdfProg of
        Just pdfProg -> do
                res <- makePDF pdfProg (optPdfEngineArgs opts) f
                        writerOptions doc
                case res of
                     Right pdf -> writeFnBinary outputFile pdf
                     Left err' -> liftIO $
                       E.throwIO $ PandocPDFError $
                                     TL.unpack (TE.decodeUtf8With TE.lenientDecode err')

        Nothing -> do
                let htmlFormat = format `elem`
                      ["html","html4","html5","s5","slidy",
                       "slideous","dzslides","revealjs"]
                    addNl = if standalone
                               then id
                               else (<> T.singleton '\n')
                output <- addNl <$> f writerOptions doc
                writerFn eol outputFile =<<
                  if optSelfContained opts && htmlFormat
                     -- TODO not maximally efficient; change type
                     -- of makeSelfContained so it works w/ Text
                     then T.pack <$> makeSelfContained (T.unpack output)
                     else return output

type Transform = Pandoc -> Pandoc

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub2","epub3","epub","pptx"]

addNonPresentMetadata :: Text.Pandoc.Meta -> Pandoc -> Pandoc
addNonPresentMetadata newmeta (Pandoc meta bs) = Pandoc (meta <> newmeta) bs

addMetadata :: [(String, String)] -> Pandoc -> Pandoc
addMetadata kvs pdc = foldr addMeta (removeMetaKeys kvs pdc) kvs

addMeta :: (String, String) -> Pandoc -> Pandoc
addMeta (k, v) (Pandoc meta bs) = Pandoc meta' bs
  where meta' = case lookupMeta k meta of
                      Nothing -> setMeta k v' meta
                      Just (MetaList xs) ->
                                 setMeta k (MetaList (xs ++ [v'])) meta
                      Just x  -> setMeta k (MetaList [x, v']) meta
        v' = readMetaValue v

removeMetaKeys :: [(String,String)] -> Pandoc -> Pandoc
removeMetaKeys kvs pdc = foldr (deleteMeta . fst) pdc kvs

readMetaValue :: String -> MetaValue
readMetaValue s = case YAML.decodeStrict (UTF8.fromString s) of
                       Right [YAML.Scalar (YAML.SStr t)]
                                             -> MetaString $ T.unpack t
                       Right [YAML.Scalar (YAML.SBool b)]
                                             -> MetaBool b
                       _                     -> MetaString s

-- Determine default reader based on source file extensions.
formatFromFilePaths :: String -> [FilePath] -> String
formatFromFilePaths fallback [] = fallback
formatFromFilePaths fallback (x:xs) =
  case formatFromFilePath x of
    Just f     -> f
    Nothing    -> formatFromFilePaths fallback xs

-- Determine format based on file extension
formatFromFilePath :: FilePath -> Maybe String
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc"     -> Just "asciidoc"
    ".asciidoc" -> Just "asciidoc"
    ".context"  -> Just "context"
    ".ctx"      -> Just "context"
    ".db"       -> Just "docbook"
    ".doc"      -> Just "doc"  -- so we get an "unknown reader" error
    ".docx"     -> Just "docx"
    ".dokuwiki" -> Just "dokuwiki"
    ".epub"     -> Just "epub"
    ".fb2"      -> Just "fb2"
    ".htm"      -> Just "html"
    ".html"     -> Just "html"
    ".icml"     -> Just "icml"
    ".json"     -> Just "json"
    ".latex"    -> Just "latex"
    ".lhs"      -> Just "markdown+lhs"
    ".ltx"      -> Just "latex"
    ".markdown" -> Just "markdown"
    ".md"       -> Just "markdown"
    ".ms"       -> Just "ms"
    ".muse"     -> Just "muse"
    ".native"   -> Just "native"
    ".odt"      -> Just "odt"
    ".opml"     -> Just "opml"
    ".org"      -> Just "org"
    ".pdf"      -> Just "pdf"  -- so we get an "unknown reader" error
    ".pptx"     -> Just "pptx"
    ".roff"     -> Just "ms"
    ".rst"      -> Just "rst"
    ".rtf"      -> Just "rtf"
    ".s5"       -> Just "s5"
    ".t2t"      -> Just "t2t"
    ".tei"      -> Just "tei"
    ".tei.xml"  -> Just "tei"
    ".tex"      -> Just "latex"
    ".texi"     -> Just "texinfo"
    ".texinfo"  -> Just "texinfo"
    ".text"     -> Just "markdown"
    ".textile"  -> Just "textile"
    ".txt"      -> Just "markdown"
    ".wiki"     -> Just "mediawiki"
    ".xhtml"    -> Just "html"
    ['.',y]     | y `elem` ['1'..'9'] -> Just "man"
    _           -> Nothing

-- Transformations of a Pandoc document post-parsing:

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

readSource :: FilePath -> PandocIO Text
readSource "-" = liftIO (UTF8.toText <$> BS.getContents)
readSource src = case parseURI src of
                      Just u | uriScheme u `elem` ["http:","https:"] ->
                                 readURI src
                             | uriScheme u == "file:" ->
                                 liftIO $ UTF8.toText <$>
                                    BS.readFile (uriPathToPath $ uriPath u)
                      _       -> liftIO $ UTF8.toText <$>
                                    BS.readFile src

readURI :: FilePath -> PandocIO Text
readURI src = UTF8.toText . fst <$> openURL src

readFile' :: MonadIO m => FilePath -> m B.ByteString
readFile' "-" = liftIO B.getContents
readFile' f   = liftIO $ B.readFile f

writeFnBinary :: MonadIO m => FilePath -> B.ByteString -> m ()
writeFnBinary "-" = liftIO . B.putStr
writeFnBinary f   = liftIO . B.writeFile (UTF8.encodePath f)

writerFn :: MonadIO m => IO.Newline -> FilePath -> Text -> m ()
-- TODO this implementation isn't maximally efficient:
writerFn eol "-" = liftIO . UTF8.putStrWith eol . T.unpack
writerFn eol f   = liftIO . UTF8.writeFileWith eol f . T.unpack


baseWriterName :: String -> String
baseWriterName = takeWhile (\c -> c /= '+' && c /= '-')
