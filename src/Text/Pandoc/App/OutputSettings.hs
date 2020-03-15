{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
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
import Text.Pandoc
import Text.Pandoc.App.FormatHeuristics (formatFromFilePaths)
import Text.Pandoc.App.Opt (Opt (..))
import Text.Pandoc.App.CommandLineOptions (engines, lookupHighlightStyle,
                                          setVariable)
import qualified Text.Pandoc.UTF8 as UTF8

readUtf8File :: PandocMonad m => FilePath -> m T.Text
readUtf8File = fmap UTF8.toText . readFileStrict

-- | Settings specifying how document output should be produced.
data OutputSettings = OutputSettings
  { outputFormat :: T.Text
  , outputWriter :: Writer PandocIO
  , outputWriterName :: T.Text
  , outputWriterOptions :: WriterOptions
  , outputPdfProgram :: Maybe String
  }

-- | Get output settings from command line options.
optToOutputSettings :: Opt -> PandocIO OutputSettings
optToOutputSettings opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)

  when (optDumpArgs opts) . liftIO $ do
    UTF8.hPutStrLn stdout outputFile
    mapM_ (UTF8.hPutStrLn stdout) (fromMaybe [] $ optInputFiles opts)
    exitSuccess

  epubMetadata <- traverse readUtf8File $ optEpubMetadata opts

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf" ||
                  optTo opts == Just "pdf"
  (writerName, maybePdfProg) <-
    if pdfOutput
       then liftIO $ pdfWriterAndProg
               (case optTo opts of
                  Just "pdf" -> Nothing
                  x          -> x)
               (optPdfEngine opts)
       else case optTo opts of
              Just f -> return (f, Nothing)
              Nothing
               | outputFile == "-" -> return ("html", Nothing)
               | otherwise ->
                     case formatFromFilePaths [outputFile] of
                           Nothing -> do
                             report $ CouldNotDeduceFormat
                                [T.pack $ takeExtension outputFile] "html"
                             return ("html", Nothing)
                           Just f  -> return (f, Nothing)

  let format = if ".lua" `T.isSuffixOf` writerName
                  then writerName
                  else T.toLower $ baseWriterName writerName

  (writer :: Writer PandocIO, writerExts) <-
            if ".lua" `T.isSuffixOf` format
               then return (TextWriter
                       (\o d -> writeCustom (T.unpack writerName) o d)
                               :: Writer PandocIO, mempty)
               else getWriter (T.toLower writerName)

  let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput

  let addSyntaxMap existingmap f = do
        res <- liftIO (parseSyntaxDefinition f)
        case res of
              Left errstr -> throwError $ PandocSyntaxMapError $ T.pack errstr
              Right syn   -> return $ addSyntaxDefinition syn existingmap

  syntaxMap <- foldM addSyntaxMap defaultSyntaxMap
                     (optSyntaxDefinitions opts)

  hlStyle <- traverse (lookupHighlightStyle . T.unpack) $ optHighlightStyle opts

  let setVariableM k v = return . setVariable k v

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

  let getTextContents fp = UTF8.toText . fst <$> fetchItem (T.pack fp)

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
    maybe return (setVariableM "epub-cover-image")
                 (T.pack <$> optEpubCoverImage opts)
    >>=
    setVariableM "curdir" (T.pack curdir)
    >>=
    (\vars ->  if format == "dzslides"
                  then do
                      dztempl <- UTF8.toText <$> readDataFile
                                   ("dzslides" </> "template.html")
                      let dzline = "<!-- {{{{ dzslides core"
                      let dzcore = T.unlines
                                 $ dropWhile (not . (dzline `T.isPrefixOf`))
                                 $ T.lines dztempl
                      setVariableM "dzslides-core" dzcore vars
                  else return vars)

  templ <- case optTemplate opts of
                  _ | not standalone -> return Nothing
                  Nothing -> Just <$> compileDefaultTemplate format
                  Just tp -> do
                    -- strip off extensions
                    let tp' = case takeExtension tp of
                                   "" -> tp <.> T.unpack format
                                   _  -> tp
                    res <- getTemplate tp' >>= runWithPartials . compileTemplate tp'
                    case res of
                      Left  e -> throwError $ PandocTemplateError $ T.pack e
                      Right t -> return $ Just t

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
        , writerEpubSubdirectory = T.pack $ optEpubSubdirectory opts
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

baseWriterName :: T.Text -> T.Text
baseWriterName = T.takeWhile (\c -> c /= '+' && c /= '-')

pdfWriterAndProg :: Maybe T.Text              -- ^ user-specified writer name
                 -> Maybe String              -- ^ user-specified pdf-engine
                 -> IO (T.Text, Maybe String) -- ^ IO (writerName, maybePdfEngineProg)
pdfWriterAndProg mWriter mEngine =
  case go mWriter mEngine of
      Right (writ, prog) -> return (writ, Just prog)
      Left err           -> liftIO $ E.throwIO $ PandocAppError err
    where
      go Nothing Nothing       = Right ("latex", "pdflatex")
      go (Just writer) Nothing = (writer,) <$> engineForWriter writer
      go Nothing (Just engine) = (,engine) <$> writerForEngine (takeBaseName engine)
      go (Just writer) (Just engine) =
           case find (== (baseWriterName writer, takeBaseName engine)) engines of
                Just _  -> Right (writer, engine)
                Nothing -> Left $ "pdf-engine " <> T.pack engine <>
                           " is not compatible with output format " <> writer

      writerForEngine eng = case [f | (f,e) <- engines, e == eng] of
                                 fmt : _ -> Right fmt
                                 []      -> Left $
                                   "pdf-engine " <> T.pack eng <> " not known"

      engineForWriter "pdf" = Left "pdf writer"
      engineForWriter w = case [e |  (f,e) <- engines, f == baseWriterName w] of
                                eng : _ -> Right eng
                                []      -> Left $
                                   "cannot produce pdf output from " <> w

isTextFormat :: T.Text -> Bool
isTextFormat s =
  s `notElem` ["odt","docx","epub2","epub3","epub","pptx","pdf"]
