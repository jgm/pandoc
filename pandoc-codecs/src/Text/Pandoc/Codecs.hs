{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Text.Pandoc.Codecs (jsonSchemaForOpt) where

import Autodocodec
import Autodocodec.Schema
import Data.Aeson (FromJSON, ToJSON(toJSON), encode)
import Data.Text (Text)
import Text.DocTemplates (Context(..))
import Text.Pandoc.App
  ( Filter (..)
  , IpynbOutput (..)
  , LineEnding (..)
  , Opt (..)
  , defaultOpts
  )
import Text.Pandoc.Definition (Meta (..))
import Text.Pandoc.Logging (Verbosity (..))
import Text.Pandoc.Options
  ( CaptionPosition (..)
  , CiteMethod (..)
  , HTMLMathMethod (..)
  , ObfuscationMethod (..)
  , ReferenceLocation (..)
  , TopLevelDivision (..)
  , TrackChanges (..)
  , WrapOption (..)
  )
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBC8

instance HasCodec CaptionPosition where
  codec = stringConstCodec
          [ (CaptionAbove, "above")
          , (CaptionBelow, "below")
          ]

instance HasCodec CiteMethod where
  codec = stringConstCodec
          [ (Citeproc, "citeproc")
          , (Natbib, "natbib")
          , (Biblatex, "biblatex")
          ]

instance HasCodec (Context Text) where
  codec = codecViaAeson "Context"

instance HasCodec Filter where
  codec = codecViaAeson "Filter"

instance HasCodec IpynbOutput where
  codec = codecViaAeson "IpynbOutput"
  -- codec = stringConstCodec
  --         [ (IpynbOutputAll,  "all")
  --         , (IpynbOutputNone, "none")
  --         , (IpynbOutputBest, "best")
  --         ]

instance HasCodec LineEnding where
  codec = codecViaAeson "LineEnding"
  -- codec = stringConstCodec
  --         [ (LF    , "lf")
  --         , (CRLF  , "crlf")
  --         , (Native, "native")
  --         ]

instance HasCodec HTMLMathMethod where
  codec = codecViaAeson "HTMLMathMethod"

instance HasCodec Meta where
  codec = codecViaAeson "Meta"

instance HasCodec ObfuscationMethod where
  codec = stringConstCodec
          [ (NoObfuscation, "none")
          , (ReferenceObfuscation, "references")
          , (JavascriptObfuscation, "javascript")
          ]

instance HasCodec ReferenceLocation where
  codec = stringConstCodec
          [ (EndOfBlock, "end-of-block")
          , (EndOfSection, "end-of-section")
          , (EndOfDocument, "end-of-document")
          ]

instance HasCodec TopLevelDivision where
  codec = stringConstCodec
          [ (TopLevelPart, "top-level-part")
          , (TopLevelChapter, "top-level-chapter")
          , (TopLevelSection, "top-level-section")
          , (TopLevelDefault, "top-level-default")
          ]

instance HasCodec TrackChanges where
  codec = stringConstCodec
          [ (AcceptChanges, "accept")
          , (RejectChanges, "reject")
          , (AllChanges,    "all")
          ]

instance HasCodec Verbosity where
  codec = stringConstCodec
          [ (ERROR, "ERROR")
          , (WARNING, "WARNING")
          , (INFO, "INFO")
          ]

instance HasCodec WrapOption where
  codec = stringConstCodec
          [ (WrapAuto, "wrap-auto")
          , (WrapNone, "wrap-none")
          , (WrapPreserve, "wrap-preserve")
          ]

instance HasCodec Opt where
  codec = object "Opt" $
    Opt
      <$> optionalFieldWithDefault
            "tab-stop"
            (optTabStop defaultOpts)
            "Number of spaces per tab"
            .= optTabStop
      <*> optionalFieldWithDefault "preserve-tabs"
            (optPreserveTabs defaultOpts)
            "Preserve tabs instead of converting to spaces"
          .= optPreserveTabs
      <*> optionalFieldWithDefault "standalone"
            (optStandalone defaultOpts)
            "Include header, footer"
          .= optStandalone
      <*> optionalFieldWithDefault "from"
            (optFrom defaultOpts)
            "Reader format"
          .= optFrom
      <*> optionalFieldWithDefault "to"
            (optTo defaultOpts)
            "Writer format"
          .= optTo
      <*> optionalFieldWithDefault "table-of-contents"
            (optTableOfContents defaultOpts)
            "Include table of contents"
          .= optTableOfContents
      <*> optionalFieldWithDefault "list-of-figures"
            (optListOfFigures defaultOpts)
            "Include list of figures"
          .= optListOfFigures
      <*> optionalFieldWithDefault "list-of-tables"
            (optListOfTables defaultOpts)
            "Include list of tables"
          .= optListOfTables
      <*> optionalFieldWithDefault "shift-heading-level-by"
            (optShiftHeadingLevelBy defaultOpts)
            "Shift heading level by"
          .= optShiftHeadingLevelBy
      <*> optionalFieldWithDefault "template"
            (optTemplate defaultOpts)
            "Custom template"
          .= optTemplate
      <*> optionalFieldWithDefault "variables"
            (optVariables defaultOpts)
            "Template variables to set"
          .= optVariables
      <*> optionalFieldWithDefault "metadata"
            (optMetadata defaultOpts)
            "Metadata fields to set"
          .= optMetadata
      <*> optionalFieldWithDefault "metadata-files"
            (optMetadataFiles defaultOpts)
            "Name of YAML metadata files"
          .= optMetadataFiles
      <*> optionalFieldWithDefault "output-file"
            (optOutputFile defaultOpts)
            "Name of output file"
          .= optOutputFile
      <*> optionalFieldWithDefault "input-files"
            (optInputFiles defaultOpts)
            "Names of input files"
          .= optInputFiles
      <*> optionalFieldWithDefault "number-sections"
            (optNumberSections defaultOpts)
            "Number sections in LaTeX"
          .= optNumberSections
      <*> optionalFieldWithDefault "number-offset"
            (optNumberOffset defaultOpts)
            "Starting number for sections"
          .= optNumberOffset
      <*> optionalFieldWithDefault "section-divs"
            (optSectionDivs defaultOpts)
            "Put sections in div tags in HTML"
          .= optSectionDivs
      <*> optionalFieldWithDefault "incremental"
            (optIncremental defaultOpts)
            "Use incremental lists in Slidy/Slideous/S5"
          .= optIncremental
      <*> optionalFieldWithDefault "self-contained"
            (optSelfContained defaultOpts)
            "Make HTML accessible offline (deprecated)"
          .= optSelfContained
      <*> optionalFieldWithDefault "embed-resources"
            (optEmbedResources defaultOpts)
            "Make HTML accessible offline"
          .= optEmbedResources
      <*> optionalFieldWithDefault "link-images"
            (optLinkImages defaultOpts)
            "Link ODT images rather than embedding"
          .= optLinkImages
      <*> optionalFieldWithDefault "html-q-tags"
            (optHtmlQTags defaultOpts)
            "Use <q> tags in HTML"
          .= optHtmlQTags
      <*> optionalFieldWithDefault "highlight-style"
            (optHighlightStyle defaultOpts)
            "Style to use for highlighted code"
          .= optHighlightStyle
      <*> optionalFieldWithDefault "syntax-definitions"
            (optSyntaxDefinitions defaultOpts)
            "xml syntax defs to load"
          .= optSyntaxDefinitions
      <*> optionalFieldWithDefault "top-level-division"
            (optTopLevelDivision defaultOpts)
            "Type of the top-level divisions"
          .= optTopLevelDivision
      <*> optionalFieldWithDefault "html-math-method"
            (optHTMLMathMethod defaultOpts)
            "Method to print HTML math"
          .= optHTMLMathMethod
      <*> optionalFieldWithDefault "abbreviations"
            (optAbbreviations defaultOpts)
            "Path to abbrevs file"
          .= optAbbreviations
      <*> optionalFieldWithDefault "reference-doc"
            (optReferenceDoc defaultOpts)
            "Path of reference doc"
          .= optReferenceDoc
      <*> optionalFieldWithDefault "split-level"
            (optSplitLevel defaultOpts)
            "Header level at which to split documents in epub and chunkedhtml"
          .= optSplitLevel
      <*> optionalFieldWithDefault "chunk-template"
            (optChunkTemplate defaultOpts)
            "Template to use for chunk filenames"
          .= optChunkTemplate
      <*> optionalFieldWithDefault "epub-subdirectory"
            (optEpubSubdirectory defaultOpts)
            "EPUB subdir in OCF container"
          .= optEpubSubdirectory
      <*> optionalFieldWithDefault "epub-metadata"
            (optEpubMetadata defaultOpts)
            "EPUB metadata"
          .= optEpubMetadata
      <*> optionalFieldWithDefault "epub-fonts"
            (optEpubFonts defaultOpts)
            "EPUB fonts to embed"
          .= optEpubFonts
      <*> optionalFieldWithDefault "epub-cover-image"
            (optEpubCoverImage defaultOpts)
            "Cover image for epub"
          .= optEpubCoverImage
      <*> optionalFieldWithDefault "epub-title-page"
            (optEpubTitlePage defaultOpts)
            "INclude title page in EPUB"
          .= optEpubTitlePage
      <*> optionalFieldWithDefault "toc-depth"
            (optTOCDepth defaultOpts)
            "Number of levels to include in TOC"
          .= optTOCDepth
      <*> optionalFieldWithDefault "dump-args"
            (optDumpArgs defaultOpts)
            "Output command-line arguments"
          .= optDumpArgs
      <*> optionalFieldWithDefault "ignore-args"
            (optIgnoreArgs defaultOpts)
            "Ignore command-line arguments"
          .= optIgnoreArgs
      <*> optionalFieldWithDefault "verbosity"
            (optVerbosity defaultOpts)
            "Verbosity of diagnostic output"
          .= optVerbosity
      <*> optionalFieldWithDefault "trace"
            (optTrace defaultOpts)
            "Enable tracing"
          .= optTrace
      <*> optionalFieldWithDefault "log-file"
            (optLogFile defaultOpts)
            "File to write JSON log output"
          .= optLogFile
      <*> optionalFieldWithDefault "fail-if-warnings"
            (optFailIfWarnings defaultOpts)
            "Fail on warnings"
          .= optFailIfWarnings
      <*> optionalFieldWithDefault "reference-links"
            (optReferenceLinks defaultOpts)
            "Use reference links in writing markdown, rst"
          .= optReferenceLinks
      <*> optionalFieldWithDefault "reference-location"
            (optReferenceLocation defaultOpts)
            "location for footnotes and link references in markdown output"
          .= optReferenceLocation
      <*> optionalFieldWithDefault "figure-caption-position"
            (optFigureCaptionPosition defaultOpts)
            "position for figure caption"
          .= optFigureCaptionPosition
      <*> optionalFieldWithDefault "tablecaptionposition"
            (optTableCaptionPosition defaultOpts)
            "position for table caption"
          .= optTableCaptionPosition
      <*> optionalFieldWithDefault "dpi"
            (optDpi defaultOpts)
            "Dpi"
          .= optDpi
      <*> optionalFieldWithDefault "wrap"
            (optWrap defaultOpts)
            "Options for wrapping text"
          .= optWrap
      <*> optionalFieldWithDefault "columns"
            (optColumns defaultOpts)
            "Line length in characters"
          .= optColumns
      <*> optionalFieldWithDefault "filters"
            (optFilters defaultOpts)
            "Filters to apply"
          .= optFilters
      <*> optionalFieldWithDefault "email-obfuscation"
            (optEmailObfuscation defaultOpts)
            "Mail obfuscation method to apply"
          .= optEmailObfuscation
      <*> optionalFieldWithDefault "identifier-prefix"
            (optIdentifierPrefix defaultOpts)
            "prefix for element identifiers"
          .= optIdentifierPrefix
      <*> optionalFieldWithDefault "indented-code-classes"
            (optIndentedCodeClasses defaultOpts)
            "Default classes for indented code blocks"
          .= optIndentedCodeClasses
      <*> optionalFieldWithDefault "data-dir"
            (optDataDir defaultOpts)
            "Pandoc's data directory"
          .= optDataDir
      <*> optionalFieldWithDefault "cite-method"
            (optCiteMethod defaultOpts)
            "Method to output cites"
          .= optCiteMethod
      <*> optionalFieldWithDefault "listings"
            (optListings defaultOpts)
            "Use listings package for code blocks"
          .= optListings
      <*> optionalFieldWithDefault "pdf-engine"
            (optPdfEngine defaultOpts)
            "Program to use for latex/html -> pdf"
          .= optPdfEngine
      <*> optionalFieldWithDefault "pdf-engine-opts"
            (optPdfEngineOpts defaultOpts)
            "Flags to pass to the engine"
          .= optPdfEngineOpts
      <*> optionalFieldWithDefault "slide-level"
            (optSlideLevel defaultOpts)
            "Header level that creates slides"
          .= optSlideLevel
      <*> optionalFieldWithDefault "setext-headers"
            (optSetextHeaders defaultOpts)
            "Use atx headers for markdown level 1-2"
          .= optSetextHeaders
      <*> optionalFieldWithDefault "list-tables"
            (optListTables defaultOpts)
            "Use list tables for RST"
          .= optListTables
      <*> optionalFieldWithDefault "ascii"
            (optAscii defaultOpts)
            "Prefer ascii output"
          .= optAscii
      <*> optionalFieldWithDefault "default-image-extension"
            (optDefaultImageExtension defaultOpts)
            "Default image extension"
          .= optDefaultImageExtension
      <*> optionalFieldWithDefault "extract-media"
            (optExtractMedia defaultOpts)
            "Path to extract embedded media"
          .= optExtractMedia
      <*> optionalFieldWithDefault "track-changes"
            (optTrackChanges defaultOpts)
            "Accept or reject MS Word track-changes."
          .= optTrackChanges
      <*> optionalFieldWithDefault "file-scope"
            (optFileScope defaultOpts)
            "Parse input files before combining"
          .= optFileScope
      <*> optionalFieldWithDefault "title-prefix"
            (optTitlePrefix defaultOpts)
            "Prefix for title"
          .= optTitlePrefix
      <*> optionalFieldWithDefault "css"
            (optCss defaultOpts)
            "CSS files to link to"
          .= optCss
      <*> optionalFieldWithDefault "ipynb-output"
            (optIpynbOutput defaultOpts)
            "How to treat ipynb output blocks"
          .= optIpynbOutput
      <*> optionalFieldWithDefault "include-before-body"
            (optIncludeBeforeBody defaultOpts)
            "Files to include before"
          .= optIncludeBeforeBody
      <*> optionalFieldWithDefault "include-after-body"
            (optIncludeAfterBody defaultOpts)
            "Files to include after body"
          .= optIncludeAfterBody
      <*> optionalFieldWithDefault "include-in-header"
            (optIncludeInHeader defaultOpts)
            "Files to include in header"
          .= optIncludeInHeader
      <*> optionalFieldWithDefault "resource-path"
            (optResourcePath defaultOpts)
            "Path to search for images etc"
          .= optResourcePath
      <*> optionalFieldWithDefaultWith "request-headers"
            (codecViaAeson "request headers")
            (optRequestHeaders defaultOpts)
            "Headers for HTTP requests"
          .= optRequestHeaders
      <*> optionalFieldWithDefault "no-check-certificate"
            (optNoCheckCertificate defaultOpts)
            "Disable certificate validation"
          .= optNoCheckCertificate
      <*> optionalFieldWithDefault "eol"
            (optEol defaultOpts)
            "Style of line-endings to use"
          .= optEol
      <*> optionalFieldWithDefault "strip-comments"
            (optStripComments defaultOpts)
            "Skip HTML comments"
          .= optStripComments
      <*> optionalFieldWithDefault "csl"
            (optCSL defaultOpts)
            "CSL stylesheet"
          .= optCSL
      <*> optionalFieldWithDefault "bibliography"
            (optBibliography defaultOpts)
            "Bibliography files"
          .= optBibliography
      <*> optionalFieldWithDefault "citation-abbreviations"
            (optCitationAbbreviations defaultOpts)
            "Citation abbreviations"
          .= optCitationAbbreviations
      <*> optionalFieldWithDefault "sandbox"
            (optSandbox defaultOpts)
            "Whether to allow access to the file system and network"
          .= optSandbox

jsonSchemaForOpt :: LBC8.ByteString
jsonSchemaForOpt = encode . toJSON $ jsonSchemaViaCodec @Opt
