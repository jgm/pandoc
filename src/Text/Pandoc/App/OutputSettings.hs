{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App.OutputSettings
  ( OutputSettings (..)
  , optToOutputSettings
  ) where
import Prelude
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Map as M
import Text.DocTemplates (Context(..), ToContext(toVal))
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans
import Data.Char (toLower)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Skylighting (defaultSyntaxMap)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.Pandoc
import Text.Pandoc.App.FormatHeuristics (formatFromFilePaths)
import Text.Pandoc.App.Opt (Opt (..))
import Text.Pandoc.App.CommandLineOptions (engines, lookupHighlightStyle)
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import qualified Text.Pandoc.UTF8 as UTF8

-- | Settings specifying how document output should be produced.
data OutputSettings = OutputSettings
  { outputFormat :: String
  , outputWriter :: Writer PandocIO
  , outputWriterName :: String
  , outputWriterOptions :: WriterOptions
  , outputPdfProgram :: Maybe String
  }

readUtf8File :: PandocMonad m => FilePath -> m String
readUtf8File = fmap UTF8.toString . readFileStrict

-- | Get output settings from command line options.
optToOutputSettings :: Opt -> PandocIO OutputSettings
optToOutputSettings opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)

  when (optDumpArgs opts) . liftIO $ do
    UTF8.hPutStrLn stdout outputFile
    mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
    exitSuccess

  epubMetadata <- case optEpubMetadata opts of
                         Nothing -> return Nothing
                         Just fp -> Just <$> readUtf8File fp

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"
  (writerName, maybePdfProg) <-
    if pdfOutput
       then liftIO $ pdfWriterAndProg (optWriter opts) (optPdfEngine opts)
       else case optWriter opts of
              Nothing
                | outputFile == "-" -> return ("html", Nothing)
                | otherwise ->
                    case formatFromFilePaths [outputFile] of
                           Nothing -> do
                             report $ CouldNotDeduceFormat
                                [takeExtension outputFile] "html"
                             return ("html", Nothing)
                           Just f  -> return (f, Nothing)
              Just f   -> return (f, Nothing)

  let format = if ".lua" `isSuffixOf` writerName
                  then writerName
                  else map toLower $ baseWriterName writerName

  (writer :: Writer PandocIO, writerExts) <-
            if ".lua" `isSuffixOf` format
               then return (TextWriter
                       (\o d -> writeCustom writerName o d)
                               :: Writer PandocIO, mempty)
               else getWriter (map toLower writerName)

  let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput

  let addStringAsVariable varname s vars = return $ (varname, s) : vars

  let addSyntaxMap existingmap f = do
        res <- liftIO (parseSyntaxDefinition f)
        case res of
              Left errstr -> throwError $ PandocSyntaxMapError errstr
              Right syn   -> return $ addSyntaxDefinition syn existingmap

  syntaxMap <- foldM addSyntaxMap defaultSyntaxMap
                     (optSyntaxDefinitions opts)

  hlStyle <- maybe (return Nothing) (fmap Just . lookupHighlightStyle)
                (optHighlightStyle opts)

  -- note: this reverses the list constructed in option parsing,
  -- which in turn was reversed from the command-line order,
  -- so we end up with the correct order in the variable list:
  let withList _ [] vars     = return vars
      withList f (x:xs) vars = f x vars >>= withList f xs

  let addContentsAsVariable varname fp vars = do
        s <- UTF8.toString . fst <$> fetchItem fp
        return $ (varname, s) : vars

  curdir <- liftIO getCurrentDirectory

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
    addStringAsVariable "curdir" curdir
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
    >>= fmap (Context . M.fromList) .
          traverse (\(x,y) -> return (T.pack x, toVal (T.pack y)))

  templStr <- case optTemplate opts of
                  _ | not standalone -> return Nothing
                  Nothing -> Just <$> getDefaultTemplate format
                  Just tp -> do
                    -- strip off extensions
                    let tp' = case takeExtension tp of
                                   "" -> tp <.> format
                                   _  -> tp
                    Just . UTF8.toText <$>
                          ((do surl <- stSourceURL <$> getCommonState
                               -- we don't want to look for templates remotely
                               -- unless the full URL is specified:
                               modifyCommonState $ \st -> st{
                                  stSourceURL = Nothing }
                               (bs, _) <- fetchItem tp'
                               modifyCommonState $ \st -> st{
                                  stSourceURL = surl }
                               return bs)
                           `catchError`
                           (\e ->
                               case e of
                                    PandocResourceNotFound _ ->
                                       readDataFile ("templates" </> tp')
                                    _ -> throwError e))

  let templatePath = fromMaybe "" $ optTemplate opts

  templ <- case templStr of
             Nothing -> return Nothing
             Just ts -> do
               res <- compileTemplate templatePath ts
               case res of
                 Left  e -> throwError $ PandocTemplateError e
                 Right t -> return $ Just t

  case lookup "lang" (optMetadata opts) of
         Just l  -> case parseBCP47 l of
                         Left _   -> return ()
                         Right l' -> setTranslations l'
         Nothing -> setTranslations $ Lang "en" "" "US" []

  let writerOpts = def {
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
        , writerHighlightStyle   = hlStyle
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
  return $ OutputSettings
    { outputFormat = format
    , outputWriter = writer
    , outputWriterName = writerName
    , outputWriterOptions = writerOpts
    , outputPdfProgram = maybePdfProg
    }

baseWriterName :: String -> String
baseWriterName = takeWhile (\c -> c /= '+' && c /= '-')

pdfWriterAndProg :: Maybe String              -- ^ user-specified writer name
                 -> Maybe String              -- ^ user-specified pdf-engine
                 -> IO (String, Maybe String) -- ^ IO (writerName, maybePdfEngineProg)
pdfWriterAndProg mWriter mEngine = do
  let panErr msg = liftIO $ E.throwIO $ PandocAppError msg
  case go mWriter mEngine of
      Right (writ, prog) -> return (writ, Just prog)
      Left "pdf writer"  -> liftIO $ E.throwIO $
                               PandocUnknownWriterError "pdf"
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

      engineForWriter "pdf" = Left "pdf writer"
      engineForWriter w = case [e |  (f,e) <- engines, f == baseWriterName w] of
                                eng : _ -> Right eng
                                []      -> Left $
                                   "cannot produce pdf output from " ++ w

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub2","epub3","epub","pptx"]
