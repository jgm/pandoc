{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{- |
   Module      : Text.Pandoc.App.CommandLineOptions
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App.CommandLineOptions (
            parseOptions
          , parseOptionsFromArgs
          , handleOptInfo
          , options
          , OptionSpec(..)
          , engines
          , setVariable
          , versionInfo
          ) where
import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Containers.ListUtils (nubOrd)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), keyOrder,
         defConfig, Indent(..), NumberFormat(..))
import Data.Bifunctor (second)
import Data.Char (toLower)
import Data.List (intercalate, sort)
import qualified Data.List as L
#ifdef _WINDOWS
import Data.List (isPrefixOf)
#endif
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Safe (tailDef)
import Skylighting (Syntax (..), defaultSyntaxMap)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.DocTemplates (Context (..), ToContext (toVal), Val (..))
import Text.Pandoc
import Text.Pandoc.Builder (setMeta)
import Data.Version (showVersion)
import Text.Pandoc.App.Completion (generateCompletion)
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..), IpynbOutput (..),
                            DefaultsState (..), applyDefaults,
                            fullDefaultsPath, OptInfo(..), CompletionShell(..),
                            OptionSpec(..), option, toOptDescr,
                            CompletionKind(..))
import Text.Pandoc.Filter (Filter (..))
import Text.Pandoc.Highlighting (highlightingStyles, lookupHighlightingStyle)
import Text.Pandoc.Scripting (ScriptingEngine (..), customTemplate)
import Text.Pandoc.Shared (safeStrRead)
import qualified Control.Exception as E
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

parseOptions :: [OptionSpec]
             -> Opt -> IO (Either OptInfo Opt)
parseOptions options' defaults = do
  rawArgs <- liftIO getArgs
  prg <- liftIO getProgName
  parseOptionsFromArgs options' defaults prg rawArgs

parseOptionsFromArgs
  :: [OptionSpec]
  -> Opt -> String -> [String] -> IO (Either OptInfo Opt)
parseOptionsFromArgs options' defaults prg rawArgs = do
  let (actions, args, unrecognizedOpts, errors) =
           getOpt' Permute (map toOptDescr options') (preprocessArgs rawArgs)

  let unknownOptionErrors =
       foldr (handleUnrecognizedOption . takeWhile (/= '=')) []
       unrecognizedOpts

  let mbArgs = case args of
                 [] -> Nothing
                 xs -> Just xs

  let adjustOpts opts =
           opts{ optInputFiles =
                   map normalizePath <$> (optInputFiles opts <> mbArgs)
               , optStandalone = -- certain other options imply standalone
                   optStandalone opts ||
                     isJust (optTemplate opts) ||
                     optSelfContained opts ||
                     not (null (optIncludeInHeader opts)) ||
                     not (null (optIncludeBeforeBody opts)) ||
                     not (null (optIncludeAfterBody opts)) }

  if (null errors && null unknownOptionErrors)
     then -- thread option data structure through all supplied option actions
       runExceptT $ adjustOpts <$> (L.foldl' (>>=) (return defaults) actions)
     else return $ Left $ OptError $ PandocOptionError $ T.pack $
             concat errors ++ unlines unknownOptionErrors ++
             ("Try " ++ prg ++ " --help for more information.")

-- | React to an 'OptInfo' by printing the requested information
-- and exiting or (if there was a parsing error) raising an error.
handleOptInfo :: ScriptingEngine -> OptInfo -> IO ()
handleOptInfo engine info = E.handle (handleError . Left) $ do
  case info of
    Completion shell -> do
      datafiles <- getDataFileNames
      script <- generateCompletion shell options
        readersNames writersNames
        (map fst highlightingStyles) pdfEngines datafiles
      UTF8.hPutStrLn stdout script
    ListInputFormats -> mapM_ (UTF8.hPutStrLn stdout) readersNames
    ListOutputFormats -> mapM_ (UTF8.hPutStrLn stdout) writersNames
    ListExtensions mbfmt -> do
      let formatName = fromMaybe "markdown" mbfmt
      let allExts = getAllExtensions formatName
      if formatName `notElem`
          (map fst (readers :: [(Text, Reader PandocPure)]) ++
           map fst (writers :: [(Text, Writer PandocPure)]))
         then E.throwIO $ PandocOptionError $ formatName <>
                " is not a recognized reader or writer format"
         else do
           let defExts = getDefaultExtensions formatName
           let showExt x =
                (if extensionEnabled x defExts
                    then '+'
                    else if extensionEnabled x allExts
                            then '-'
                            else ' ') : drop 4 (show x)
           mapM_ (UTF8.hPutStrLn stdout . T.pack . showExt)
              (extensionsToList allExts)
    ListHighlightLanguages -> do
      let langs = [ T.unpack (T.toLower (sShortname s))
                  | s <- M.elems defaultSyntaxMap
                  , sShortname s `notElem`
                     [T.pack "Alert", T.pack "Alert_indent"]
                  ]
      mapM_ (UTF8.hPutStrLn stdout . T.pack) (sort langs)
    ListHighlightStyles -> do
      mapM_ (UTF8.hPutStrLn stdout . fst) highlightingStyles
    PrintDefaultTemplate mbout fmt -> do
      let write = maybe (UTF8.hPutStr stdout) (UTF8.writeFile) mbout

      templ <- runIO $
               case splitExtension (T.unpack fmt) of
                    (_, "") -> do
                      -- built-in format
                      setUserDataDir Nothing
                      getDefaultTemplate fmt
                    _ -> do
                      -- format looks like a filepath => custom writer
                      components <- engineLoadCustom engine (T.unpack fmt)
                      case customTemplate components of
                        Just t  -> pure t
                        Nothing -> E.throw $ PandocNoTemplateError fmt
      case templ of
           Right t
             | T.null t -> -- e.g. for docx, odt, json:
                 E.throwIO $ PandocCouldNotFindDataFileError $ T.pack
                   ("templates/default." ++ T.unpack fmt)
             | otherwise -> write t
           Left e  -> E.throwIO e
    PrintDefaultDataFile mbout f -> do
      let write = maybe BS.putStr BS.writeFile mbout
      runIOorExplode $ readDefaultDataFile (T.unpack f) >>= liftIO . write
    PrintHighlightStyle mbout styleName -> do
       let write = maybe B.putStr B.writeFile mbout
       sty <- runIOorExplode $ lookupHighlightingStyle (T.unpack styleName)
       write $ encodePretty'
         defConfig{confIndent = Spaces 4
                  ,confCompare = keyOrder
                    (map T.pack
                     ["text-color"
                     ,"background-color"
                     ,"line-number-color"
                     ,"line-number-background-color"
                     ,"bold"
                     ,"italic"
                     ,"underline"
                     ,"text-styles"])
                  ,confNumFormat = Generic
                  ,confTrailingNewline = True} sty
    VersionInfo -> versionInfo [] Nothing ""
    Help -> do
      prg <- getProgName
      mapM_ (UTF8.hPutStrLn stdout . T.stripEnd . T.pack) $
        lines $ usageMessage prg (map toOptDescr options)
    OptError e -> E.throwIO e
  exitSuccess

-- | Supported LaTeX engines; the first item is used as default engine
-- when going through LaTeX.
latexEngines :: [String]
latexEngines  = [ "pdflatex", "lualatex", "xelatex", "latexmk", "tectonic"
                , "pdflatex-dev", "lualatex-dev" ]

-- | Supported HTML PDF engines; the first item is used as default
-- engine when going through HTML.
htmlEngines :: [String]
htmlEngines  = ["weasyprint", "wkhtmltopdf", "pagedjs-cli", "prince"]

engines :: [(Text, String)]
engines = map ("html",) htmlEngines ++
          map ("html5",) htmlEngines ++
          map ("latex",) latexEngines ++
          map ("beamer",) latexEngines ++
          [ ("ms", "groff")
          , ("ms", "pdfroff")
          , ("typst", "typst")
          , ("context", "context")
          ]

pdfEngines :: [String]
pdfEngines = nubOrd $ map snd engines

-- For motivation see #8956.  We want to allow things like `-si` without
-- causing the `i` to be parsed as an optional boolean argument of `-s`.
-- This is for backwards compatibility given the addition of optional
-- boolean arguments in #8879.
preprocessArgs :: [String] -> [String]
preprocessArgs [] = []
preprocessArgs ("--":xs) = "--" : xs -- a bare '--' ends option parsing
-- note that -strue is interpreted as -strue while
-- -stmarkdown is interpreted as -s -tmarkdown
preprocessArgs (('-':c:d:cs):xs)
  | isShortBooleanOpt c
  , case toLower <$> (d:cs) of
      "true" -> True
      "false" -> True
      _ -> False
    = ('-':c:d:cs) : preprocessArgs xs
  | isShortBooleanOpt c
  , isShortOpt d = splitArg (c:d:cs) ++ preprocessArgs xs
preprocessArgs (x:xs) = x : preprocessArgs xs

isShortBooleanOpt :: Char -> Bool
isShortBooleanOpt = (`Set.member` shortBooleanOpts)
 where
  shortBooleanOpts =
     Set.fromList [c | OptionSpec [c] _ (OptArg _ "true|false") _ _ <- options]

isShortOpt :: Char -> Bool
isShortOpt = (`Set.member` shortOpts)
 where
  shortOpts = Set.fromList $ concat [cs | OptionSpec cs _ _ _ _ <- options]

splitArg :: String -> [String]
splitArg (c:d:cs)
  | isShortBooleanOpt c
  , isShortOpt d
  = ['-',c] : splitArg (d:cs)
splitArg (c:cs) = ['-':c:cs]
splitArg [] = []

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptionSpec]
options =
    [ option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optFrom = Just $ T.pack arg })
                  "FORMAT")
                 InputFormats
                 (T.pack "Reader format")

    , option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optTo = Just $ T.pack arg })
                  "FORMAT")
                 OutputFormats
                 (T.pack "Writer format")

    , option "o" ["output"]
                 (ReqArg
                  (\arg opt -> return opt { optOutputFile =
                                             Just (normalizePath arg) })
                  "FILE")
                 Files
                 (T.pack "Output file")

    , option "" ["data-dir"]
                 (ReqArg
                  (\arg opt -> return opt { optDataDir =
                                  Just (normalizePath arg) })
                 "DIRECTORY") -- "Directory containing pandoc data files."
                Files
                (T.pack "Directory for data files")

    , option "M" ["metadata"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optMetadata = addMeta key val $
                                                 optMetadata opt })
                  "KEY[=VALUE]")
                 Files
                 (T.pack "Metadata field KEY=VALUE")

    , option "" ["metadata-file"]
                 (ReqArg
                  (\arg opt -> return opt{ optMetadataFiles =
                      optMetadataFiles opt ++ [normalizePath arg] })
                  "FILE")
                 Files
                 (T.pack "Metadata file")

    , option "d" ["defaults"]
                 (ReqArg
                  (\arg opt -> do
                     res <- liftIO $ runIO $ do
                       let defsState =
                             DefaultsState { curDefaults = Nothing,
                                             inheritanceGraph = [] }
                       fp <- fullDefaultsPath (optDataDir opt) arg
                       evalStateT (applyDefaults opt fp) defsState
                     case res of
                       Left e -> optError e
                       Right x -> return x
                  )
                  "FILE")
                Files
                (T.pack "Defaults file")

    , option "" ["file-scope"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--file-scope" arg
                        return opt { optFileScope = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Parse files before combining")

    , option "" ["sandbox"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--sandbox" arg
                        return opt { optSandbox = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Run pandoc in a sandbox")

     , option "s" ["standalone"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--standalone/-s" arg
                        return opt { optStandalone = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Include header and footer")

    , option "" ["template"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optTemplate = Just (normalizePath arg) })
                  "FILE")
                 Files
                 (T.pack "Custom template file")

    , option "V" ["variable"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optVariables =
                                  setVariable (T.pack key) (T.pack val) $
                                    optVariables opt })
                  "KEY[=VALUE]")
                 Files
                 (T.pack "Template variable KEY=VALUE")

    , option "" ["variable-json"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, json) = splitField arg
                     case eitherDecode (B.fromStrict . UTF8.fromString $ json) of
                       Right (val :: Val Text) ->
                         return opt{ optVariables =
                                      let Context m = optVariables opt
                                       in Context $ M.insert (T.pack key) val m }
                           -- note that this replaces any existing value, which
                           -- is different from what --variable does
                       Left err'  -> optError $ PandocOptionError $
                          "Could not parse '" <> T.pack json <> "' as JSON:\n" <>
                           T.pack err')
                  "KEY[:JSON]")
                 Files
                 (T.pack "Template variable KEY=JSON")

    , option "" ["wrap"]
                 (ReqArg
                  (\arg opt ->
                    case arg of
                      "auto" -> return opt{ optWrap = WrapAuto }
                      "none" -> return opt{ optWrap = WrapNone }
                      "preserve" -> return opt{ optWrap = WrapPreserve }
                      _      -> optError $ PandocOptionError
                                 "--wrap must be auto, none, or preserve")
                 "auto|none|preserve")
                 (Fixed ["auto","none","preserve"])
                 (T.pack "Text wrapping mode")

    , option "" ["ascii"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--ascii" arg
                        return opt { optAscii = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Prefer ASCII output")

    , option "" ["toc", "table-of-contents"]
                (OptArg
                 (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--toc/--table-of-contents" arg
                        return opt { optTableOfContents = boolValue })
                 "true|false")
               OptFlag
               (T.pack "Include table of contents")

    , option "" ["toc-depth"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optTOCDepth = t }
                           _ -> optError $ PandocOptionError
                                "Argument of --toc-depth must be a number 1-6")
                 "NUMBER")
                 Files
                 (T.pack "Number of TOC levels")

    , option "" ["lof", "list-of-figures"]
                (OptArg
                 (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--lof/--list-of-figures" arg
                        return opt { optListOfFigures = boolValue })
                 "true|false")
               OptFlag
               (T.pack "Include list of figures")

    , option "" ["lot", "list-of-tables"]
                (OptArg
                 (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--lot/--list-of-tables" arg
                        return opt { optListOfTables = boolValue })
                 "true|false")
               OptFlag
               (T.pack "Include list of tables")

    , option "N" ["number-sections"]
                  (OptArg
                   (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--number-sections/-N" arg
                        return opt { optNumberSections = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Number section headings")

    , option "" ["number-offset"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead ("[" <> arg <> "]") of
                           Just ns -> return opt { optNumberOffset = ns,
                                                   optNumberSections = True }
                           _      -> optError $ PandocOptionError
                                       "could not parse argument of --number-offset")
                 "NUMBERS")
                 Files
                 (T.pack "Starting number for sections")

    , option "" ["top-level-division"]
                 (ReqArg
                  (\arg opt ->
                      case arg of
                        "section" -> return opt{ optTopLevelDivision =
                                        TopLevelSection }
                        "chapter" -> return opt{ optTopLevelDivision =
                                        TopLevelChapter }
                        "part"    -> return opt{ optTopLevelDivision =
                                        TopLevelPart }
                        "default" -> return opt{ optTopLevelDivision =
                                        TopLevelDefault }
                        _ -> optError $ PandocOptionError $
                                "Argument of --top-level division must be " <>
                                "section,  chapter, part, or default" )
                   "section|chapter|part")
                 (Fixed ["section","chapter","part"])
                 (T.pack "Top-level document division")

    , option "" ["extract-media"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optExtractMedia =
                                  Just (normalizePath arg) })
                  "PATH")
                 Files
                 (T.pack "Directory to extract media into")

    , option "" ["resource-path"]
                (ReqArg
                  (\arg opt -> return opt { optResourcePath =
                                   splitSearchPath arg ++
                                    optResourcePath opt })
                   "SEARCHPATH")
                  Files
                  (T.pack "Search path for resources")

    , option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeInHeader =
                                             optIncludeInHeader opt ++
                                             [normalizePath arg] })
                  "FILE")
                 Files
                 (T.pack "File to include in the header")

    , option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeBeforeBody =
                                            optIncludeBeforeBody opt ++
                                            [normalizePath arg] })
                  "FILE")
                 Files
                 (T.pack "File to include before the body")

    , option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeAfterBody =
                                            optIncludeAfterBody opt ++
                                            [normalizePath arg] })
                  "FILE")
                 Files
                 (T.pack "File to include after the body")

    , option "" ["no-highlight"]
                (NoArg
                 (\opt -> do
                     deprecatedOption "--no-highlight"
                       "Use --syntax-highlighting=none instead."
                     return opt { optSyntaxHighlighting = NoHighlightingString }))
                 OptFlag
                 (T.pack "Disable syntax highlighting")

    , option "" ["highlight-style"]
                (ReqArg
                 (\arg opt -> do
                     deprecatedOption "--highlight-style"
                       "Use --syntax-highlighting instead."
                     return opt{ optSyntaxHighlighting =
                                 T.pack $ normalizePath arg })
                 "STYLE|FILE")
                 HighlightStyles
                 (T.pack "Highlighting style")

    , option "" ["syntax-definition"]
                (ReqArg
                 (\arg opt ->
                   return opt{ optSyntaxDefinitions = normalizePath arg :
                                optSyntaxDefinitions opt })
                 "FILE")
                Files
                (T.pack "Syntax definition XML file")

    , option "" ["syntax-highlighting"]
                (ReqArg
                 (\arg opt -> return opt{ optSyntaxHighlighting =
                                 T.pack $ normalizePath arg })
                 "none|default|idiomatic|<stylename>|<themepath>")
                 (Fixed ["none","default","idiomatic"])
                 (T.pack "Syntax highlighting method")


    , option "" ["dpi"]
                 (ReqArg
                  (\arg opt ->
                    case safeStrRead arg of
                         Just t | t > 0 -> return opt { optDpi = t }
                         _              -> optError $ PandocOptionError
                                        "Argument of --dpi must be a number greater than 0")
                  "NUMBER")
                 Files
                 (T.pack "DPI for imported images")

    , option "" ["eol"]
                 (ReqArg
                  (\arg opt ->
                    case toLower <$> arg of
                      "crlf"   -> return opt { optEol = CRLF }
                      "lf"     -> return opt { optEol = LF }
                      "native" -> return opt { optEol = Native }
                      -- mac-syntax (cr) is not supported in ghc-base.
                      _      -> optError $ PandocOptionError
                                "Argument of --eol must be crlf, lf, or native")
                  "crlf|lf|native")
                 (Fixed ["crlf","lf","native"])
                 (T.pack "End-of-line characters")

    , option "" ["columns"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t > 0 -> return opt { optColumns = t }
                           _              -> optError $ PandocOptionError
                                   "Argument of --columns must be a number greater than 0")
                 "NUMBER")
                 Files
                 (T.pack "Line length in characters")

    , option "p" ["preserve-tabs"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--preserve-tabs/-p" arg
                        return opt { optPreserveTabs = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Preserve tabs")

    , option "" ["tab-stop"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t > 0 -> return opt { optTabStop = t }
                           _              -> optError $ PandocOptionError
                                  "Argument of --tab-stop must be a number greater than 0")
                  "NUMBER")
                 Files
                 (T.pack "Tab stop width")

    , option "" ["pdf-engine"]
                 (ReqArg
                  (\arg opt -> do
                     let b = takeBaseName arg
                     if b `elem` pdfEngines
                        then return opt { optPdfEngine = Just arg }
                        else optError $
                              PandocOptionError $ T.pack $
                              "Argument of --pdf-engine must be one of\n"
                               ++ concatMap (\e -> "\t" <> e <> "\n") pdfEngines)
                  "PROGRAM")
                 Engines
                 (T.pack "Program used to produce PDF")

    , option "" ["pdf-engine-opt"]
                 (ReqArg
                  (\arg opt -> do
                      let oldArgs = optPdfEngineOpts opt
                      return opt { optPdfEngineOpts = oldArgs ++ [arg]})
                  "STRING")
                 Files
                 (T.pack "Flag to pass to the PDF engine")

    , option "" ["reference-doc"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optReferenceDoc = Just $ normalizePath arg })
                  "FILE")
                 Files
                 (T.pack "Custom reference doc")

    , option "" ["self-contained"]
                 (OptArg
                  (\arg opt -> do
                        deprecatedOption "--self-contained"
                          "Use --embed-resources --standalone instead."
                        boolValue <- readBoolFromOptArg "--self-contained" arg
                        return opt { optSelfContained = boolValue })
                    "true|false")
                 OptFlag
                 (T.pack "Embed resources (deprecated)")

    , option "" ["embed-resources"] -- maybe True (\argStr -> argStr == "true") arg
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--embed-resources" arg
                        return opt { optEmbedResources =  boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Embed referenced resources")

    , option "" ["link-images"] -- maybe True (\argStr -> argStr == "true") arg
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--link-images" arg
                        return opt { optLinkImages =  boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Link images in ODT rather than embedding")

    , option "" ["request-header"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optRequestHeaders =
                       (T.pack key, T.pack val) : optRequestHeaders opt })
                  "NAME=VALUE")
                 Files
                 (T.pack "HTTP header NAME=VALUE")

    , option "" ["no-check-certificate"]
                (OptArg
                 (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--no-check-certificate" arg
                        return opt { optNoCheckCertificate = boolValue })
                 "true|false")
                OptFlag
                (T.pack "Disable certificate validation")

    , option "" ["abbreviations"]
                (ReqArg
                 (\arg opt -> return opt { optAbbreviations =
                                            Just $ normalizePath arg })
                "FILE")
                Files
                (T.pack "File with abbreviations")

    , option "" ["typst-input"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optTypstInputs = (T.pack key, T.pack val) : optTypstInputs opt })
                  "KEY=VALUE")
                 Files
                 (T.pack "Typst variable KEY=VALUE")

    , option "" ["indented-code-classes"]
                  (ReqArg
                   (\arg opt -> return opt { optIndentedCodeClasses = T.words $
                                             T.map (\c -> if c == ',' then ' ' else c) $
                                             T.pack arg })
                   "STRING")
                  Files
                  (T.pack "Classes for indented code blocks")

    , option "" ["default-image-extension"]
                 (ReqArg
                  (\arg opt -> return opt { optDefaultImageExtension = T.pack arg })
                   "extension")
                  Files
                  (T.pack "Default extension for images")

    , option "F" ["filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                      optFilters opt ++ [JSONFilter (normalizePath arg)] })
                  "PROGRAM")
                 Files
                 (T.pack "External JSON filter")

    , option "L" ["lua-filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                      optFilters opt ++ [LuaFilter (normalizePath arg)] })
                  "SCRIPTPATH")
                 Files
                 (T.pack "Lua filter script")

    , option "" ["shift-heading-level-by"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t ->
                               return opt{ optShiftHeadingLevelBy = t }
                           _              -> optError $ PandocOptionError
                                               "Argument of --shift-heading-level-by must be an integer")
                  "NUMBER")
                 Files
                 (T.pack "Shift heading level by N")

    , option "" ["base-header-level"]
                 (ReqArg
                  (\arg opt -> do
                      deprecatedOption "--base-header-level"
                        "Use --shift-heading-level-by instead."
                      case safeStrRead arg of
                           Just t | t > 0 && t < 6 ->
                               return opt{ optShiftHeadingLevelBy = t - 1 }
                           _              -> optError $ PandocOptionError
                                               "Argument of --base-header-level must be 1-5")
                  "NUMBER")
                 Files
                 (T.pack "Base header level (deprecated)")

    , option "" ["track-changes"]
                 (ReqArg
                  (\arg opt -> do
                     action <- case arg of
                            "accept" -> return AcceptChanges
                            "reject" -> return RejectChanges
                            "all"    -> return AllChanges
                            _        -> optError $ PandocOptionError $ T.pack
                               "Argument of --track-changes must be accept, reject, or all"
                     return opt { optTrackChanges = action })
                  "accept|reject|all")
                 (Fixed ["accept","reject","all"])
                 (T.pack "Handling of Word track-changes")

    , option "" ["strip-comments"]
                (OptArg
                 (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--strip-comments" arg
                        return opt { optStripComments = boolValue })
                 "true|false")
               OptFlag
               (T.pack "Strip HTML comments")

    , option "" ["reference-links"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--reference-links" arg
                        return opt { optReferenceLinks = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Use reference links in HTML")

    , option "" ["reference-location"]
                 (ReqArg
                  (\arg opt -> do
                     action <- case arg of
                            "block"    -> return EndOfBlock
                            "section"  -> return EndOfSection
                            "document" -> return EndOfDocument
                            _        -> optError $ PandocOptionError $ T.pack
                               "Argument of --reference-location must be block, section, or document"
                     return opt { optReferenceLocation = action })
                  "block|section|document")
                 (Fixed ["block","section","document"])
                 (T.pack "Location of references")

    , option "" ["figure-caption-position"]
                 (ReqArg
                  (\arg opt -> do
                     pos <- case arg of
                            "above"  -> return CaptionAbove
                            "below"  -> return CaptionBelow
                            _        -> optError $ PandocOptionError $ T.pack
                               "Argument of --figure-caption-position must be above or below"
                     return opt { optFigureCaptionPosition = pos })
                  "above|below")
                 (Fixed ["above","below"])
                 (T.pack "Figure caption position")

    , option "" ["table-caption-position"]
                 (ReqArg
                  (\arg opt -> do
                     pos <- case arg of
                            "above"  -> return CaptionAbove
                            "below"  -> return CaptionBelow
                            _        -> optError $ PandocOptionError $ T.pack
                               "Argument of --table-caption-position must be above or below"
                     return opt { optTableCaptionPosition = pos })
                  "above|below")
                 (Fixed ["above","below"])
                 (T.pack "Table caption position")

    , option "" ["markdown-headings"]
                  (ReqArg
                    (\arg opt -> do
                      headingFormat <- case arg of
                        "setext" -> pure True
                        "atx" -> pure False
                        _ -> optError $ PandocOptionError $ T.pack
                          "Argument of --markdown-headings must be setext or atx"
                      pure opt { optSetextHeaders = headingFormat }
                    )
                  "setext|atx")
                  (Fixed ["setext","atx"])
                  (T.pack "Markdown heading style")

    , option "" ["list-tables"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--list-tables" arg
                        return opt { optListTables = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Use list tables for RST")

    , option "" ["listings"]
                 (OptArg
                  (\arg opt -> do
                      deprecatedOption "--listings"
                        "Use --syntax-highlighting=idiomatic instead."
                      boolValue <- readBoolFromOptArg "--listings" arg
                      return $
                        if boolValue
                        then opt { optSyntaxHighlighting =
                                   IdiomaticHighlightingString }
                        else opt)
                  "true|false")
                 OptFlag
                 (T.pack "Use listings package (deprecated)")

    , option "i" ["incremental"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--incremental/-i" arg
                        return opt { optIncremental = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Make list items display incrementally")

    , option "" ["slide-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t >= 0 && t <= 6 ->
                                    return opt { optSlideLevel = Just t }
                           _      -> optError $ PandocOptionError
                                    "Argument of --slide-level must be a number between 0 and 6")
                 "NUMBER")
                 Files
                 (T.pack "Header level used for slides")

    , option "" ["section-divs"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--section-divs" arg
                        return opt { optSectionDivs = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Wrap sections in div tags")

    , option "" ["html-q-tags"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--html-q-tags" arg
                        return opt { optHtmlQTags = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Use q tags for quotes in HTML")

    , option "" ["email-obfuscation"]
                 (ReqArg
                  (\arg opt -> do
                     method <- case arg of
                            "references" -> return ReferenceObfuscation
                            "javascript" -> return JavascriptObfuscation
                            "none"       -> return NoObfuscation
                            _            -> optError $ PandocOptionError $ T.pack
                               "Argument of --email-obfuscation must be references, javascript, or none"
                     return opt { optEmailObfuscation = method })
                  "none|javascript|references")
                 (Fixed ["references","javascript","none"])
                 (T.pack "Email obfuscation method")

     , option "" ["id-prefix"]
                  (ReqArg
                   (\arg opt -> return opt { optIdentifierPrefix = T.pack arg })
                   "STRING")
                  Files
                  (T.pack "Prefix for auto identifiers")

    , option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt ->
                    return opt {
                       optVariables =
                         setVariable "title-prefix" (T.pack arg) $
                           optVariables opt,
                       optStandalone = True })
                  "STRING")
                 Files
                 (T.pack "Window title prefix")

    , option "c" ["css"]
                 (ReqArg
                  (\arg opt -> return opt{ optCss = optCss opt ++ [arg] })
                  -- add new link to end, so it is included in proper order
                  "URL")
                 Files
                 (T.pack "CSS style sheet")

    , option "" ["epub-subdirectory"]
             (ReqArg
                  (\arg opt ->
                     return opt { optEpubSubdirectory = arg })
                  "DIRNAME")
                 Files
                 (T.pack "EPUB content subdirectory")

    , option "" ["epub-cover-image"]
                 (ReqArg
                  (\arg opt ->
                     return opt { optVariables =
                       setVariable "epub-cover-image"
                         (T.pack $ normalizePath arg) $
                         optVariables opt })
                  "FILE")
                 Files
                 (T.pack "EPUB cover image")

    , option "" ["epub-title-page"]
                 (OptArg
                  (\arg opt -> do
                     boolValue <- readBoolFromOptArg "--epub-title-page" arg
                     return opt{ optEpubTitlePage = boolValue })
                 "true|false")
                 Files
                 (T.pack "URL or file for EPUB title page")

    , option "" ["epub-metadata"]
                 (ReqArg
                  (\arg opt -> return opt { optEpubMetadata = Just $
                                             normalizePath arg })
                  "FILE")
                 Files
                 (T.pack "EPUB metadata file")

    , option "" ["epub-embed-font"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optEpubFonts = normalizePath arg :
                                                optEpubFonts opt })
                  "FILE")
                 Files
                 (T.pack "Font file to embed in EPUB")

    , option "" ["split-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optSplitLevel = t }
                           _      -> optError $ PandocOptionError
                                    "Argument of --split-level must be a number between 1 and 6")
                 "NUMBER")
                 Files
                 (T.pack "Split level for chunked HTML or EPUB")

    , option "" ["chunk-template"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optChunkTemplate = Just (T.pack arg) })
                 "PATHTEMPLATE")
                 Files
                 (T.pack "Template for chunked HTML paths")

    , option "" ["epub-chapter-level"]
                 (ReqArg
                  (\arg opt -> do
                      deprecatedOption "--epub-chapter-level"
                                       "Use --split-level instead."
                      case safeStrRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optSplitLevel = t }
                           _      -> optError $ PandocOptionError
                                    "Argument of --epub-chapter-level must be a number between 1 and 6")
                 "NUMBER")
                 Files
                 (T.pack "Split level (deprecated)")

    , option "" ["ipynb-output"]
                 (ReqArg
                  (\arg opt ->
                    case arg of
                      "all" -> return opt{ optIpynbOutput = IpynbOutputAll }
                      "best" -> return opt{ optIpynbOutput = IpynbOutputBest }
                      "none" -> return opt{ optIpynbOutput = IpynbOutputNone }
                      _ -> optError $ PandocOptionError
                             "Argument of --ipynb-output must be all, none, or best")
                 "all|none|best")
                 (Fixed ["all","none","best"])
                 (T.pack "Handling of ipynb output cells")

    , option "C" ["citeproc"]
                 (NoArg
                  (\opt -> return opt { optFilters =
                      optFilters opt ++ [CiteprocFilter] }))
                 OptFlag
                 (T.pack "Process citations")

    , option "" ["bibliography"]
                 (ReqArg
                  (\arg opt -> return opt{ optBibliography =
                                            optBibliography opt ++
                                              [normalizePath arg] })
                   "FILE")
                 Files
                 (T.pack "Bibliography file")

     , option "" ["csl"]
                 (ReqArg
                  (\arg opt -> do
                    return opt{ optCSL = Just (normalizePath arg) })
                   "FILE")
                 Files
                 (T.pack "CSL style file")

     , option "" ["citation-abbreviations"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata =
                                  addMeta "citation-abbreviations"
                                    (normalizePath arg) $ optMetadata opt })
                   "FILE")
                 Files
                 (T.pack "Citation abbreviations file")

    , option "" ["natbib"]
                 (NoArg
                  (\opt -> return opt { optCiteMethod = Natbib }))
                 OptFlag
                 (T.pack "Use natbib citations in LaTeX")

    , option "" ["biblatex"]
                 (NoArg
                  (\opt -> return opt { optCiteMethod = Biblatex }))
                 OptFlag
                 (T.pack "Use biblatex citations in LaTeX")

    , option "" ["mathml"]
                 (NoArg
                  (\opt ->
                      return opt { optHTMLMathMethod = MathML }))
                 OptFlag
                 (T.pack "Use MathML for HTML math")

    , option "" ["webtex"]
                 (OptArg
                  (\arg opt -> do
                      let url' = maybe defaultWebTeXURL T.pack arg
                      return opt { optHTMLMathMethod = WebTeX url' })
                  "URL")
                 OptFlag
                 (T.pack "Use WebTeX for HTML math")

    , option "" ["mathjax"]
                 (OptArg
                  (\arg opt -> do
                      let url' = maybe defaultMathJaxURL T.pack arg
                      return opt { optHTMLMathMethod = MathJax url'})
                  "URL")
                 OptFlag
                 (T.pack "Use MathJax for HTML math")

    , option "" ["katex"]
                 (OptArg
                  (\arg opt ->
                      return opt
                        { optHTMLMathMethod = KaTeX $
                           maybe defaultKaTeXURL T.pack arg })
                  "URL")
                  OptFlag
                  (T.pack "Use KaTeX for HTML math")

    , option "" ["gladtex"]
                 (NoArg
                  (\opt ->
                      return opt { optHTMLMathMethod = GladTeX }))
                 OptFlag
                 (T.pack "Use gladTeX for HTML math")

    , option "" ["trace"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--trace" arg
                        return opt { optTrace = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Turn on diagnostic tracing")

    , option "" ["dump-args"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--dump-args" arg
                        return opt { optDumpArgs = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Print output filename and arguments")

    , option "" ["ignore-args"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--ignore-args" arg
                        return opt { optIgnoreArgs = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Ignore command-line arguments")

    , option "" ["verbose"]
                 (NoArg
                  (\opt -> return opt { optVerbosity = INFO }))
                 OptFlag
                 (T.pack "Verbose diagnostic output")

    , option "" ["quiet"]
                 (NoArg
                  (\opt -> return opt { optVerbosity = ERROR }))
                 OptFlag
                 (T.pack "Suppress warning messages")

    , option "" ["fail-if-warnings"]
                 (OptArg
                  (\arg opt -> do
                        boolValue <- readBoolFromOptArg "--fail-if-warnings" arg
                        return opt { optFailIfWarnings = boolValue })
                  "true|false")
                 OptFlag
                 (T.pack "Exit with error status if there were warnings")

    , option "" ["log"]
                 (ReqArg
                  (\arg opt -> return opt{ optLogFile = Just $
                                            normalizePath arg })
                "FILE")
                Files
                (T.pack "Log messages in JSON format to this file")

    , option "" ["completion"]
                 (ReqArg
                  (\arg _opt -> optInfo $ parseCompletionShell arg)
                  "SHELL")
                 OptFlag
                 (T.pack "Shell for which to print the completion script")

    , option "" ["bash-completion"]
                 (NoArg (\_ -> do
                    deprecatedOption "--bash-completion"
                       "Use --completion=bash instead."
                    optInfo $ Completion Bash))
                 OptFlag
                 (T.pack "Print bash completion script (deprecated)")

    , option "" ["list-input-formats"]
                 (NoArg (\_ -> optInfo ListInputFormats))
                 OptFlag
                 (T.pack "List supported input formats")

    , option "" ["list-output-formats"]
                 (NoArg (\_ -> optInfo ListOutputFormats))
                 OptFlag
                 (T.pack "List supported output formats")

    , option "" ["list-extensions"]
                 (OptArg (\arg _ -> optInfo $ ListExtensions $ T.pack <$> arg)
                 "FORMAT")
                 OptFlag
                 (T.pack "List supported extensions")

    , option "" ["list-highlight-languages"]
                 (NoArg (\_ -> optInfo ListHighlightLanguages))
                 OptFlag
                 (T.pack "List highlighting languages")

    , option "" ["list-highlight-styles"]
                 (NoArg (\_ -> optInfo ListHighlightStyles))
                 OptFlag
                 (T.pack "List highlighting styles")

    , option "D" ["print-default-template"]
                 (ReqArg
                  (\arg opts -> optInfo $
                    PrintDefaultTemplate (optOutputFile opts) (T.pack arg))
                 "FORMAT")
                 OutputFormats
                 (T.pack "Format to print template for")

    , option "" ["print-default-data-file"]
                 (ReqArg
                  (\arg opts -> optInfo $
                    PrintDefaultDataFile (optOutputFile opts) (T.pack arg))
                 "FILE")
                  DataFiles
                  (T.pack "Data file to print")

    , option "" ["print-highlight-style"]
                 (ReqArg
                  (\arg opts ->
                    optInfo $ PrintHighlightStyle (optOutputFile opts)
                               (T.pack arg))
                  "STYLE|FILE")
                 HighlightStyles
                 (T.pack "Highlighting style")

    , option "v" ["version"]
                 (NoArg (\_ -> optInfo VersionInfo))
                 OptFlag
                 (T.pack "Print version")

    , option "h" ["help"]
                 (NoArg (\_ -> optInfo Help))
                 OptFlag
                 (T.pack "Show help")
    ]

optError :: PandocError -> ExceptT OptInfo IO a
optError = throwError . OptError

optInfo :: OptInfo -> ExceptT OptInfo IO a
optInfo = throwError

parseCompletionShell :: String -> OptInfo
parseCompletionShell "bash" = Completion Bash
parseCompletionShell "zsh"  = Completion Zsh
parseCompletionShell "fish" = Completion Fish
parseCompletionShell s =
  OptError $ PandocOptionError $
    "Unknown completion shell '" <> T.pack s <>
    "'.  Expected one of: bash, zsh, fish."

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> ExceptT OptInfo IO Opt)] -> String
usageMessage programName = usageInfo (programName ++ " [OPTIONS] [FILES]")

copyrightMessage :: String
copyrightMessage = intercalate "\n" [
 "Copyright (C) 2006-2025 John MacFarlane. Web:  https://pandoc.org",
 "This is free software; see the source for copying conditions. There is no",
 "warranty, not even for merchantability or fitness for a particular purpose." ]

handleUnrecognizedOption :: String -> [String] -> [String]
handleUnrecognizedOption "--smart" =
  (("--smart/-S has been removed.  Use +smart or -smart extension instead.\n" ++
    "For example: pandoc -f markdown+smart -t markdown-smart.") :)
handleUnrecognizedOption "--normalize" =
  ("--normalize has been removed.  Normalization is now automatic." :)
handleUnrecognizedOption "-S" = handleUnrecognizedOption "--smart"
handleUnrecognizedOption "--old-dashes" =
  ("--old-dashes has been removed.  Use +old_dashes extension instead." :)
handleUnrecognizedOption "--no-wrap" =
  ("--no-wrap has been removed.  Use --wrap=none instead." :)
handleUnrecognizedOption "--latex-engine" =
  ("--latex-engine has been removed.  Use --pdf-engine instead." :)
handleUnrecognizedOption "--latex-engine-opt" =
  ("--latex-engine-opt has been removed.  Use --pdf-engine-opt instead." :)
handleUnrecognizedOption "--chapters" =
  ("--chapters has been removed. Use --top-level-division=chapter instead." :)
handleUnrecognizedOption "--reference-docx" =
  ("--reference-docx has been removed. Use --reference-doc instead." :)
handleUnrecognizedOption "--reference-odt" =
  ("--reference-odt has been removed. Use --reference-doc instead." :)
handleUnrecognizedOption "--parse-raw" =
  ("--parse-raw/-R has been removed. Use +raw_html or +raw_tex extension.\n" :)
handleUnrecognizedOption "--epub-stylesheet" =
  ("--epub-stylesheet has been removed. Use --css instead.\n" :)
handleUnrecognizedOption "-R" = handleUnrecognizedOption "--parse-raw"
handleUnrecognizedOption x =
  (("Unknown option " ++ x ++ ".") :)

readersNames :: [Text]
readersNames = sort (map fst (readers :: [(Text, Reader PandocIO)]))

writersNames :: [Text]
writersNames = sort
  ("pdf" : map fst (writers :: [(Text, Writer PandocIO)]))

splitField :: String -> (String, String)
splitField = second (tailDef "true") . break (\c -> c == ':' || c == '=')

deprecatedOption :: String -> String -> ExceptT OptInfo IO ()
deprecatedOption o msg = do
  res <- liftIO $ runIO (report $ Deprecated (T.pack o) (T.pack msg))
  case res of
       Right () -> return ()
       Left e   -> optError e

-- | Set text value in text context.  Create list if it has a value already,
-- or add to a list value.
setVariable :: Text -> Text -> Context Text -> Context Text
setVariable key val (Context ctx) = Context $ M.alter go key ctx
  where go Nothing             = Just $ toVal val
        go (Just (ListVal xs)) = Just $ ListVal $ xs ++ [toVal val]
        go (Just x)            = Just $ ListVal [x, toVal val]

addMeta :: String -> String -> Meta -> Meta
addMeta k v meta =
  case lookupMeta k' meta of
       Nothing -> setMeta k' v' meta
       Just (MetaList xs) ->
                  setMeta k' (MetaList (xs ++ [v'])) meta
       Just x  -> setMeta k' (MetaList [x, v']) meta
 where
  v' = readMetaValue v
  k' = T.pack k

readMetaValue :: String -> MetaValue
readMetaValue s
  | s == "true"  = MetaBool True
  | s == "True"  = MetaBool True
  | s == "TRUE"  = MetaBool True
  | s == "false" = MetaBool False
  | s == "False" = MetaBool False
  | s == "FALSE" = MetaBool False
  | otherwise    = MetaString $ T.pack s

readBoolFromOptArg ::  Text -> Maybe String -> ExceptT OptInfo IO Bool
readBoolFromOptArg opt = maybe (return True) readBoolFromArg
    where readBoolFromArg arg = case toLower <$> arg of
            "true"  -> return True
            "false" -> return False
            _       -> optError $ PandocOptionError $
                        "Argument of " <> opt <> " must be either true or false"

-- On Windows with ghc 8.6+, we need to rewrite paths
-- beginning with \\ to \\?\UNC\. -- See #5127.
normalizePath :: FilePath -> FilePath
#ifdef _WINDOWS
normalizePath fp =
  if "\\\\" `isPrefixOf` fp && not ("\\\\?\\" `isPrefixOf` fp)
    then "\\\\?\\UNC\\" ++ drop 2 fp
    else fp
#else
normalizePath = id
#endif

-- | Print version information with customizable features and scripting engine
versionInfo :: [String] -> Maybe String -> String -> IO ()
versionInfo features mbScriptingEngineName suffix = do
  defaultDatadir <- defaultUserDataDir
  let featuresLine = if null features
                       then []
                       else ["Features: " ++ unwords features]
  let scriptingLine = case mbScriptingEngineName of
                        Nothing -> []
                        Just name -> ["Scripting engine: " ++ name]
  UTF8.putStr $ T.unlines $ map T.pack $
    ["pandoc " ++ showVersion pandocVersion ++ suffix] ++
    featuresLine ++
    scriptingLine ++
    ["User data directory: " ++ defaultDatadir,
     copyrightMessage]
  exitSuccess
