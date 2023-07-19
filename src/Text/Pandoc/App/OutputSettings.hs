{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
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
import qualified Data.Map as M
import qualified Data.Text as T
import Text.DocTemplates (toVal, Context(..), Val(..))
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Trans
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Skylighting (defaultSyntaxMap)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)
import System.Directory (getCurrentDirectory)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.Pandoc.Chunks (PathTemplate(..))
import Text.Pandoc
import Text.Pandoc.App.Opt (Opt (..))
import Text.Pandoc.App.CommandLineOptions (engines)
import Text.Pandoc.Format (FlavoredFormat (..), applyExtensionsDiff,
                           parseFlavoredFormat, formatFromFilePaths)
import Text.Pandoc.Highlighting (lookupHighlightingStyle)
import Text.Pandoc.Scripting (ScriptingEngine (engineLoadCustom),
                              CustomComponents(..))
import qualified Text.Pandoc.UTF8 as UTF8

readUtf8File :: PandocMonad m => FilePath -> m T.Text
readUtf8File fp = readFileStrict fp >>= toTextM fp

-- | Settings specifying how document output should be produced.
data OutputSettings m = OutputSettings
  { outputFormat :: T.Text
  , outputWriter :: Writer m
  , outputWriterOptions :: WriterOptions
  , outputPdfProgram :: Maybe String
  }

-- | Get output settings from command line options.
optToOutputSettings :: (PandocMonad m, MonadIO m)
                    => ScriptingEngine -> Opt -> m (OutputSettings m)
optToOutputSettings scriptingEngine opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)

  when (optDumpArgs opts) . liftIO $ do
    UTF8.hPutStrLn stdout (T.pack outputFile)
    mapM_ (UTF8.hPutStrLn stdout . T.pack) (fromMaybe [] $ optInputFiles opts)
    exitSuccess

  epubMetadata <- traverse readUtf8File $ optEpubMetadata opts

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf" ||
                  optTo opts == Just "pdf"
  let defaultOutput = "html"
  defaultOutputFlavor <- parseFlavoredFormat defaultOutput
  (flvrd@(FlavoredFormat format _extsDiff), maybePdfProg) <-
    if pdfOutput
       then do
         outflavor <- case optTo opts of
                        Just x | x /= "pdf" -> Just <$> parseFlavoredFormat x
                        _ -> pure Nothing
         liftIO $ pdfWriterAndProg outflavor (optPdfEngine opts)
       else case optTo opts of
              Just f -> (, Nothing) <$> parseFlavoredFormat f
              Nothing
               | outputFile == "-" ->
                   return (defaultOutputFlavor, Nothing)
               | otherwise -> case formatFromFilePaths [outputFile] of
                   Nothing -> do
                     report $ CouldNotDeduceFormat
                       [T.pack $ takeExtension outputFile] defaultOutput
                     return (defaultOutputFlavor,Nothing)
                   Just f  -> return (f, Nothing)

  when (format == "asciidoctor") $ do
    report $ Deprecated "asciidoctor" "use asciidoc instead"

  let makeSandboxed pureWriter =
          let files = maybe id (:) (optReferenceDoc opts) .
                      maybe id (:) (optEpubMetadata opts) .
                      maybe id (:) (optEpubCoverImage opts) .
                      maybe id (:) (optCSL opts) .
                      maybe id (:) (optCitationAbbreviations opts) $
                      optEpubFonts opts ++
                      optBibliography opts
           in  case pureWriter of
                 TextWriter w -> TextWriter $ \o d -> sandbox files (w o d)
                 ByteStringWriter w ->
                   ByteStringWriter $ \o d -> sandbox files (w o d)

  let standalone = optStandalone opts || isBinaryFormat format || pdfOutput
  let templateOrThrow = \case
        Left  e -> throwError $ PandocTemplateError (T.pack e)
        Right t -> pure t
  let processCustomTemplate getDefault =
        case optTemplate opts of
          _ | not standalone -> return Nothing
          Nothing -> Just <$> getDefault
          Just tp -> do
            -- strip off extensions
            let tp' = case takeExtension tp of
                        "" -> tp <.> T.unpack format
                        _  -> tp
            getTemplate tp'
              >>= runWithPartials . compileTemplate tp'
              >>= fmap Just . templateOrThrow

  (writer, writerExts, mtemplate) <-
    if "lua" `T.isSuffixOf` format
    then do
      let path = T.unpack format
      components <- engineLoadCustom scriptingEngine path
      w <- case customWriter components of
             Nothing -> throwError $ PandocAppError $
                         format <> " does not contain a custom writer"
             Just w -> return w
      let extsConf = fromMaybe mempty $ customExtensions components
      wexts <- applyExtensionsDiff extsConf flvrd
      templ <- processCustomTemplate $
               case customTemplate components of
                 Nothing -> throwError $ PandocNoTemplateError format
                 Just t -> (runWithDefaultPartials $ compileTemplate path t) >>=
                           templateOrThrow
      return (w, wexts, templ)
    else
      if optSandbox opts
      then do
        tmpl <- processCustomTemplate (compileDefaultTemplate format)
        case runPure (getWriter flvrd) of
             Right (w, wexts) -> return (makeSandboxed w, wexts, tmpl)
             Left e           -> throwError e
      else do
        (w, wexts) <- getWriter flvrd
        tmpl <- processCustomTemplate (compileDefaultTemplate format)
        return (w, wexts, tmpl)


  let addSyntaxMap existingmap f = do
        res <- liftIO (parseSyntaxDefinition f)
        case res of
              Left errstr -> throwError $ PandocSyntaxMapError $ T.pack errstr
              Right syn   -> return $ addSyntaxDefinition syn existingmap

  syntaxMap <- foldM addSyntaxMap defaultSyntaxMap
                     (optSyntaxDefinitions opts)

  hlStyle <- traverse (lookupHighlightingStyle . T.unpack) $
               optHighlightStyle opts

  let setListVariableM _ [] ctx = return ctx
      setListVariableM k vs ctx = do
        let ctxMap = unContext ctx
        return $ Context $
          case M.lookup k ctxMap of
              Just (ListVal xs) -> M.insert k
                                  (ListVal $ xs ++ map toVal vs) ctxMap
              Just v -> M.insert k
                         (ListVal $ v : map toVal vs) ctxMap
              Nothing -> M.insert k (toVal vs) ctxMap

  let getTextContents fp = (fst <$> fetchItem (T.pack fp)) >>= toTextM fp

  let setFilesVariableM k fps ctx = do
        xs <- mapM getTextContents fps
        setListVariableM k xs ctx

  curdir <- liftIO getCurrentDirectory

  variables <-
    return (optVariables opts)
    >>=
    setListVariableM "sourcefile"
      (maybe ["-"] (fmap T.pack) (optInputFiles opts))
    >>=
    setVariableM "outputfile" (T.pack outputFile)
    >>=
    setVariableM "pandoc-version" pandocVersionText
    >>=
    setFilesVariableM "include-before" (optIncludeBeforeBody opts)
    >>=
    setFilesVariableM "include-after" (optIncludeAfterBody opts)
    >>=
    setFilesVariableM "header-includes" (optIncludeInHeader opts)
    >>=
    setListVariableM "css" (map T.pack $ optCss opts)
    >>=
    maybe return (setVariableM "title-prefix") (optTitlePrefix opts)
    >>=
    maybe return (setVariableM "epub-cover-image" . T.pack)
                 (optEpubCoverImage opts)
    >>=
    setVariableM "curdir" (T.pack curdir)
    >>=
    (\vars ->  if format == "dzslides"
                  then do
                      dztempl <-
                        let fp = "dzslides" </> "template.html"
                         in readDataFile fp >>= toTextM fp
                      let dzline = "<!-- {{{{ dzslides core"
                      let dzcore = T.unlines
                                 $ dropWhile (not . (dzline `T.isPrefixOf`))
                                 $ T.lines dztempl
                      setVariableM "dzslides-core" dzcore vars
                  else return vars)

  let writerOpts = def {
          writerTemplate         = mtemplate
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
        , writerWrapText         = optWrap opts
        , writerColumns          = optColumns opts
        , writerEmailObfuscation = optEmailObfuscation opts
        , writerIdentifierPrefix = optIdentifierPrefix opts
        , writerHtmlQTags        = optHtmlQTags opts
        , writerTopLevelDivision = optTopLevelDivision opts
        , writerListings         = optListings opts
        , writerSlideLevel       = optSlideLevel opts
        , writerHighlightStyle   = hlStyle
        , writerSetextHeaders    = optSetextHeaders opts
        , writerListTables       = optListTables opts
        , writerEpubSubdirectory = T.pack $ optEpubSubdirectory opts
        , writerEpubMetadata     = epubMetadata
        , writerEpubFonts        = optEpubFonts opts
        , writerEpubTitlePage    = optEpubTitlePage opts
        , writerSplitLevel       = optSplitLevel opts
        , writerChunkTemplate    = maybe (PathTemplate "%s-%i.html")
                                     PathTemplate
                                     (optChunkTemplate opts)
        , writerTOCDepth         = optTOCDepth opts
        , writerReferenceDoc     = optReferenceDoc opts
        , writerSyntaxMap        = syntaxMap
        , writerPreferAscii      = optAscii opts
        }
  return $ OutputSettings
    { outputFormat = format
    , outputWriter = writer
    , outputWriterOptions = writerOpts
    , outputPdfProgram = maybePdfProg
    }

-- | Set text value in text context unless it is already set.
setVariableM :: Monad m
             => T.Text -> T.Text -> Context T.Text -> m (Context T.Text)
setVariableM key val (Context ctx) = return $ Context $ M.alter go key ctx
  where go Nothing             = Just $ toVal val
        go (Just x)            = Just x

pdfWriterAndProg :: Maybe FlavoredFormat      -- ^ user-specified format
                 -> Maybe String              -- ^ user-specified pdf-engine
                 -> IO (FlavoredFormat, Maybe String) -- ^ format, pdf-engine
pdfWriterAndProg mWriter mEngine =
  case go mWriter mEngine of
      Right (writ, prog) -> return (writ, Just prog)
      Left err           -> liftIO $ E.throwIO $ PandocAppError err
    where
      go Nothing Nothing       = Right
                                 (FlavoredFormat "latex" mempty, "pdflatex")
      go (Just writer) Nothing = (writer,) <$> engineForWriter writer
      go Nothing (Just engine) = (,engine) <$> writerForEngine (takeBaseName engine)
      go (Just writer) (Just engine) | isCustomWriter writer =
           -- custom writers can produce any format, so assume the user knows
           -- what they are doing.
           Right (writer, engine)
      go (Just writer) (Just engine) =
           case find (== (formatName writer, takeBaseName engine)) engines of
                Just _  -> Right (writer, engine)
                Nothing -> Left $ "pdf-engine " <> T.pack engine <>
                           " is not compatible with output format " <>
                           formatName writer

      writerForEngine eng = case [f | (f,e) <- engines, e == eng] of
                                 fmt : _ -> Right (FlavoredFormat fmt mempty)
                                 []      -> Left $
                                   "pdf-engine " <> T.pack eng <> " not known"

      engineForWriter (FlavoredFormat "pdf" _) = Left "pdf writer"
      engineForWriter w = case [e | (f,e) <- engines, f == formatName w] of
                                eng : _ -> Right eng
                                []      -> Left $
                                   "cannot produce pdf output from " <>
                                   formatName w

      isCustomWriter w = ".lua" `T.isSuffixOf` formatName w

isBinaryFormat :: T.Text -> Bool
isBinaryFormat s =
  s `elem` ["odt","docx","epub2","epub3","epub","pptx","pdf","chunkedhtml"]
