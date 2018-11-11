{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.YAML as YAML
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.Pandoc
import Text.Pandoc.App.Opt (Opt (..), defaultOpts)
import Text.Pandoc.App.CommandLineOptions (parseOptions, options)
import Text.Pandoc.App.InputSettings (optToInputSettings)
import Text.Pandoc.App.OutputSettings (OutputSettings (..), optToOutputSettings)
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Builder (setMeta, deleteMeta)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), applyFilters)
import Text.Pandoc.IO.Input (InputSettings (..), readInput)
import Text.Pandoc.IO.Options (LineEnding (..))
import Text.Pandoc.IO.Output (writeOutput)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.SelfContained (makeDataURI)
import Text.Pandoc.Shared (eastAsianLineBreakFilter, stripEmptyParagraphs,
         headerShift)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif


convertWithOpts :: Opt -> IO ()
convertWithOpts opts = do
  let outputFile' = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout outputFile'
       mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
       exitSuccess

  let isPandocCiteproc (JSONFilter f) = takeBaseName f == "pandoc-citeproc"
      isPandocCiteproc _              = False
  -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
  let needsCiteproc = isJust (lookup "bibliography" (optMetadata opts)) &&
                      optCiteMethod opts `notElem` [Natbib, Biblatex] &&
                      all (not . isPandocCiteproc) filters
  let filters' = if needsCiteproc then JSONFilter "pandoc-citeproc" : filters
                                  else filters

  datadir <- case optDataDir opts of
                  Nothing   -> E.catch
                                 (Just <$> getAppUserDataDirectory "pandoc")
                                 (\e -> let _ = (e :: E.SomeException)
                                        in  return Nothing)
                  Just _    -> return $ optDataDir opts

  let pdfOutput = map toLower (takeExtension outputFile') == ".pdf"

  -- TODO: we have to get the input and the output into the state for
  -- the sake of the text2tags reader.

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

  runIO' $ do
    setUserDataDir datadir
    setInputFiles (optInputFiles opts)
    setOutputFile (optOutputFile opts)

    case lookup "lang" (optMetadata opts) of
           Just l  -> case parseBCP47 l of
                           Left _   -> return ()
                           Right l' -> setTranslations l'
           Nothing -> setTranslations $ Lang "en" "" "US" []

    outputSettings <- optToOutputSettings opts
    let format = outputFormat outputSettings

    let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput

    inputSettings <- optToInputSettings opts standalone
    let readerOpts = inputReaderOptions inputSettings

    -- We don't want to send output to the terminal if the user
    -- does 'pandoc -t docx input.txt'; though we allow them to
    -- force this with '-o -'.
    istty <- stdoutToTTY
    let noOutfile = isNothing (outputFile outputSettings)
    when (not (isTextFormat format) && istty && noOutfile) $
      liftIO . E.throwIO . PandocAppError $
              "Cannot write " ++ format ++ " output to terminal.\n" ++
              "Specify an output file using the -o option, or " ++
              "use '-o -' to force output to stdout."

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

    let writerOptions = outputWriterOptions outputSettings
    let transforms = (case optBaseHeaderLevel opts of
                          x | x > 1     -> (headerShift (x - 1) :)
                            | otherwise -> id) .
                     (if optStripEmptyParagraphs opts
                         then (stripEmptyParagraphs :)
                         else id) .
                     (if extensionEnabled Ext_east_asian_line_breaks
                            (readerExtensions readerOpts) &&
                         not (extensionEnabled Ext_east_asian_line_breaks
                              (writerExtensions writerOptions) &&
                              writerWrapText writerOptions == WrapPreserve)
                         then (eastAsianLineBreakFilter :)
                         else id) $
                     []

    when (inputReaderName inputSettings == "markdown_github" ||
          outputWriterName outputSettings == "markdown_github") $
      report $ Deprecated "markdown_github" "Use gfm instead."

    setResourcePath (optResourcePath opts)
    mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

    doc <- readInput inputSettings >>=
              (   (if isJust (optExtractMedia opts)
                      then fillMediaBag
                      else return)
              >=> return . addNonPresentMetadata metadataFromFile
              >=> return . addMetadata metadata
              >=> applyTransforms transforms
              >=> applyFilters readerOpts filters' [format]
              >=> maybe return extractMedia (optExtractMedia opts)
              )

    writeOutput standalone outputSettings doc

type Transform = Pandoc -> Pandoc

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub2","epub3","epub","pptx"]

-- | Whether stdout is connected to a terminal. On posix systems,
-- we detect when stdout is being piped and allow output to
-- stdout in that case, but on Windows we can't.
stdoutToTTY :: MonadIO m => m Bool
#ifdef _WINDOWS
stdoutToTTY = return True
#else
stdoutToTTY = liftIO $ queryTerminal stdOutput
#endif

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

-- Transformations of a Pandoc document post-parsing:

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms
