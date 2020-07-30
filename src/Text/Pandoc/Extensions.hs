{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Extensions
   Copyright   : Copyright (C) 2012-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures and functions for representing markup extensions.
-}
module Text.Pandoc.Extensions ( Extension(..)
                              , Extensions
                              , emptyExtensions
                              , extensionsFromList
                              , parseFormatSpec
                              , extensionEnabled
                              , enableExtension
                              , disableExtension
                              , getDefaultExtensions
                              , getAllExtensions
                              , pandocExtensions
                              , plainExtensions
                              , strictExtensions
                              , phpMarkdownExtraExtensions
                              , githubMarkdownExtensions
                              , multimarkdownExtensions )
where
import Data.Bits (clearBit, setBit, testBit, (.|.))
import Data.Data (Data)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Safe (readMay)
import Text.Parsec
import Data.Aeson.TH (deriveJSON, defaultOptions)

newtype Extensions = Extensions Integer
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Semigroup Extensions where
  (Extensions a) <> (Extensions b) = Extensions (a .|. b)
instance Monoid Extensions where
  mempty = Extensions 0
  mappend = (<>)

extensionsFromList :: [Extension] -> Extensions
extensionsFromList = foldr enableExtension emptyExtensions

emptyExtensions :: Extensions
emptyExtensions = Extensions 0

extensionEnabled :: Extension -> Extensions -> Bool
extensionEnabled x (Extensions exts) = testBit exts (fromEnum x)

enableExtension :: Extension -> Extensions -> Extensions
enableExtension x (Extensions exts) = Extensions (setBit exts (fromEnum x))

disableExtension :: Extension -> Extensions -> Extensions
disableExtension x (Extensions exts) = Extensions (clearBit exts (fromEnum x))

-- | Individually selectable syntax extensions.
data Extension =
      Ext_abbreviations       -- ^ PHP markdown extra abbreviation definitions
    | Ext_all_symbols_escapable  -- ^ Make all non-alphanumerics escapable
    | Ext_amuse -- ^ Enable Text::Amuse extensions to Emacs Muse markup
    | Ext_angle_brackets_escapable  -- ^ Make < and > escapable
    | Ext_ascii_identifiers   -- ^ ascii-only identifiers for headers;
                              -- presupposes Ext_auto_identifiers
    | Ext_auto_identifiers    -- ^ Automatic identifiers for headers
    | Ext_autolink_bare_uris  -- ^ Make all absolute URIs into links
    | Ext_backtick_code_blocks    -- ^ GitHub style ``` code blocks
    | Ext_blank_before_blockquote -- ^ Require blank line before a blockquote
    | Ext_blank_before_header     -- ^ Require blank line before a header
    | Ext_bracketed_spans         -- ^ Bracketed spans with attributes
    | Ext_citations           -- ^ Pandoc/citeproc citations
    | Ext_compact_definition_lists  -- ^ Definition lists without space between items,
                                    --   and disallow laziness
    | Ext_definition_lists    -- ^ Definition lists as in pandoc, mmd, php
    | Ext_east_asian_line_breaks  -- ^ Newlines in paragraphs are ignored between
                                  --   East Asian wide characters. Note: this extension
                                  --   does not affect readers/writers directly; it causes
                                  --   the eastAsianLineBreakFilter to be applied after
                                  --   parsing, in Text.Pandoc.App.convertWithOpts.
    | Ext_emoji               -- ^ Support emoji like :smile:
    | Ext_empty_paragraphs -- ^ Allow empty paragraphs
    | Ext_epub_html_exts      -- ^ Recognise the EPUB extended version of HTML
    | Ext_escaped_line_breaks     -- ^ Treat a backslash at EOL as linebreak
    | Ext_example_lists       -- ^ Markdown-style numbered examples
    | Ext_fancy_lists         -- ^ Enable fancy list numbers and delimiters
    | Ext_fenced_code_attributes  -- ^ Allow attributes on fenced code blocks
    | Ext_fenced_code_blocks  -- ^ Parse fenced code blocks
    | Ext_fenced_divs             -- ^ Allow fenced div syntax :::
    | Ext_footnotes           -- ^ Pandoc\/PHP\/MMD style footnotes
    | Ext_four_space_rule     -- ^ Require 4-space indent for list contents
    | Ext_gfm_auto_identifiers  -- ^ Use GitHub's method for generating
                              -- header identifiers; presupposes
                              -- Ext_auto_identifiers
    | Ext_grid_tables         -- ^ Grid tables (pandoc, reST)
    | Ext_hard_line_breaks    -- ^ All newlines become hard line breaks
    | Ext_header_attributes   -- ^ Explicit header attributes {#id .class k=v}
    | Ext_ignore_line_breaks  -- ^ Newlines in paragraphs are ignored
    | Ext_implicit_figures    -- ^ A paragraph with just an image is a figure
    | Ext_implicit_header_references -- ^ Implicit reference links for headers
    | Ext_inline_code_attributes  -- ^ Allow attributes on inline code
    | Ext_inline_notes        -- ^ Pandoc-style inline notes
    | Ext_intraword_underscores  -- ^ Treat underscore inside word as literal
    | Ext_latex_macros        -- ^ Parse LaTeX macro definitions (for math only)
    | Ext_line_blocks         -- ^ RST style line blocks
    | Ext_link_attributes         -- ^ link and image attributes
    | Ext_lists_without_preceding_blankline -- ^ Allow lists without preceding blank
    | Ext_literate_haskell    -- ^ Enable literate Haskell conventions
    | Ext_markdown_attribute      -- ^ Interpret text inside HTML as markdown iff
                                  --   container has attribute 'markdown'
    | Ext_markdown_in_html_blocks -- ^ Interpret as markdown inside HTML blocks
    | Ext_mmd_header_identifiers -- ^ Multimarkdown style header identifiers [myid]
    | Ext_mmd_link_attributes     -- ^ MMD style reference link attributes
    | Ext_mmd_title_block     -- ^ Multimarkdown metadata block
    | Ext_multiline_tables    -- ^ Pandoc-style multiline tables
    | Ext_native_divs             -- ^ Use Div blocks for contents of <div> tags
    | Ext_native_spans            -- ^ Use Span inlines for contents of <span>
    | Ext_native_numbering    -- ^ Use output format's native numbering for figures and tables
    | Ext_ntb                 -- ^ ConTeXt Natural Tables
    | Ext_old_dashes          -- ^ -- = em, - before number = en
    | Ext_pandoc_title_block  -- ^ Pandoc title block
    | Ext_pipe_tables         -- ^ Pipe tables (as in PHP markdown extra)
    | Ext_raw_attribute           -- ^ Allow explicit raw blocks/inlines
    | Ext_raw_html            -- ^ Allow raw HTML
    | Ext_raw_tex             -- ^ Allow raw TeX (other than math)
    | Ext_raw_markdown        -- ^ Parse markdown in ipynb as raw markdown
    | Ext_shortcut_reference_links -- ^ Shortcut reference links
    | Ext_simple_tables       -- ^ Pandoc-style simple tables
    | Ext_smart               -- ^ "Smart" quotes, apostrophes, ellipses, dashes
    | Ext_space_in_atx_header -- ^ Require space between # and header text
    | Ext_spaced_reference_links -- ^ Allow space between two parts of ref link
    | Ext_startnum            -- ^ Make start number of ordered list significant
    | Ext_strikeout           -- ^ Strikeout using ~~this~~ syntax
    | Ext_subscript           -- ^ Subscript using ~this~ syntax
    | Ext_superscript         -- ^ Superscript using ^this^ syntax
    | Ext_styles              -- ^ Read styles that pandoc doesn't know
    | Ext_task_lists          -- ^ Parse certain list items as task list items
    | Ext_table_captions      -- ^ Pandoc-style table captions
    | Ext_tex_math_dollars    -- ^ TeX math between $..$ or $$..$$
    | Ext_tex_math_double_backslash  -- ^ TeX math btw \\(..\\) \\[..\\]
    | Ext_tex_math_single_backslash  -- ^ TeX math btw \(..\) \[..\]
    | Ext_yaml_metadata_block -- ^ YAML metadata block
    | Ext_gutenberg           -- ^ Use Project Gutenberg conventions for plain
    | Ext_attributes          -- ^ Generic attribute syntax
    deriving (Show, Read, Enum, Eq, Ord, Bounded, Data, Typeable, Generic)

-- | Extensions to be used with pandoc-flavored markdown.
pandocExtensions :: Extensions
pandocExtensions = extensionsFromList
  [ Ext_footnotes
  , Ext_inline_notes
  , Ext_pandoc_title_block
  , Ext_yaml_metadata_block
  , Ext_table_captions
  , Ext_implicit_figures
  , Ext_simple_tables
  , Ext_multiline_tables
  , Ext_grid_tables
  , Ext_pipe_tables
  , Ext_citations
  , Ext_raw_tex
  , Ext_raw_html
  , Ext_tex_math_dollars
  , Ext_latex_macros
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_backtick_code_blocks
  , Ext_inline_code_attributes
  , Ext_raw_attribute
  , Ext_markdown_in_html_blocks
  , Ext_native_divs
  , Ext_fenced_divs
  , Ext_native_spans
  , Ext_bracketed_spans
  , Ext_escaped_line_breaks
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_all_symbols_escapable
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_space_in_atx_header
  , Ext_strikeout
  , Ext_superscript
  , Ext_subscript
  , Ext_task_lists
  , Ext_auto_identifiers
  , Ext_header_attributes
  , Ext_link_attributes
  , Ext_implicit_header_references
  , Ext_line_blocks
  , Ext_shortcut_reference_links
  , Ext_smart
  ]

-- | Extensions to be used with plain text output.
plainExtensions :: Extensions
plainExtensions = extensionsFromList
  [ Ext_table_captions
  , Ext_implicit_figures
  , Ext_simple_tables
  , Ext_multiline_tables
  , Ext_grid_tables
  , Ext_latex_macros
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_strikeout
  ]

-- | Extensions to be used with PHP Markdown Extra.
phpMarkdownExtraExtensions :: Extensions
phpMarkdownExtraExtensions = extensionsFromList
  [ Ext_footnotes
  , Ext_pipe_tables
  , Ext_raw_html
  , Ext_markdown_attribute
  , Ext_fenced_code_blocks
  , Ext_definition_lists
  , Ext_intraword_underscores
  , Ext_header_attributes
  , Ext_link_attributes
  , Ext_abbreviations
  , Ext_shortcut_reference_links
  , Ext_spaced_reference_links
  ]

-- | Extensions to be used with github-flavored markdown.
githubMarkdownExtensions :: Extensions
githubMarkdownExtensions = extensionsFromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_native_divs
  , Ext_auto_identifiers
  , Ext_gfm_auto_identifiers
  , Ext_autolink_bare_uris
  , Ext_strikeout
  , Ext_task_lists
  , Ext_emoji
  , Ext_fenced_code_blocks
  , Ext_backtick_code_blocks
  ]

-- | Extensions to be used with multimarkdown.
multimarkdownExtensions :: Extensions
multimarkdownExtensions = extensionsFromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_markdown_attribute
  , Ext_mmd_link_attributes
  -- , Ext_raw_tex
  -- Note: MMD's raw TeX syntax requires raw TeX to be
  -- enclosed in HTML comment
  , Ext_tex_math_double_backslash
  , Ext_tex_math_dollars
  , Ext_intraword_underscores
  , Ext_mmd_title_block
  , Ext_footnotes
  , Ext_definition_lists
  , Ext_all_symbols_escapable
  , Ext_implicit_header_references
  , Ext_shortcut_reference_links
  , Ext_auto_identifiers
  , Ext_mmd_header_identifiers
  , Ext_implicit_figures
  -- Note: MMD's syntax for superscripts and subscripts
  -- is a bit more permissive than pandoc's, allowing
  -- e^2 and a~1 instead of e^2^ and a~1~, so even with
  -- these options we don't have full support for MMD
  -- superscripts and subscripts, but there's no reason
  -- not to include these:
  , Ext_superscript
  , Ext_subscript
  , Ext_backtick_code_blocks
  , Ext_spaced_reference_links
  -- So far only in dev version of mmd:
  , Ext_raw_attribute
  ]

-- | Language extensions to be used with strict markdown.
strictExtensions :: Extensions
strictExtensions = extensionsFromList
  [ Ext_raw_html
  , Ext_shortcut_reference_links
  , Ext_spaced_reference_links
  ]

-- | Default extensions from format-describing string.
getDefaultExtensions :: T.Text -> Extensions
getDefaultExtensions "markdown_strict"   = strictExtensions
getDefaultExtensions "markdown_phpextra" = phpMarkdownExtraExtensions
getDefaultExtensions "markdown_mmd"      = multimarkdownExtensions
getDefaultExtensions "markdown_github"   = githubMarkdownExtensions <>
  extensionsFromList
    [ Ext_all_symbols_escapable
    , Ext_backtick_code_blocks
    , Ext_fenced_code_blocks
    , Ext_space_in_atx_header
    , Ext_intraword_underscores
    , Ext_lists_without_preceding_blankline
    , Ext_shortcut_reference_links
    ]
getDefaultExtensions "markdown"          = pandocExtensions
getDefaultExtensions "ipynb"             =
  extensionsFromList
    [ Ext_all_symbols_escapable
    , Ext_pipe_tables
    , Ext_raw_html
    , Ext_fenced_code_blocks
    , Ext_auto_identifiers
    , Ext_gfm_auto_identifiers
    , Ext_backtick_code_blocks
    , Ext_autolink_bare_uris
    , Ext_space_in_atx_header
    , Ext_intraword_underscores
    , Ext_strikeout
    , Ext_task_lists
    , Ext_lists_without_preceding_blankline
    , Ext_shortcut_reference_links
    , Ext_tex_math_dollars
    ]
getDefaultExtensions "muse"            = extensionsFromList
                                           [Ext_amuse,
                                            Ext_auto_identifiers]
getDefaultExtensions "plain"           = plainExtensions
getDefaultExtensions "gfm"             = githubMarkdownExtensions
getDefaultExtensions "commonmark"      = extensionsFromList
                                          [Ext_raw_html]
getDefaultExtensions "commonmark_x"    = extensionsFromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_auto_identifiers
  , Ext_strikeout
  , Ext_task_lists
  , Ext_emoji
  , Ext_pipe_tables
  , Ext_raw_html
  , Ext_raw_tex            -- only supported in writer (for math)
  , Ext_smart
  , Ext_tex_math_dollars
  , Ext_superscript
  , Ext_subscript
  , Ext_definition_lists
  , Ext_footnotes
  , Ext_fancy_lists
  , Ext_fenced_divs
  , Ext_bracketed_spans
  , Ext_raw_attribute
  , Ext_implicit_header_references
  , Ext_attributes
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_backtick_code_blocks
  ]
getDefaultExtensions "org"             = extensionsFromList
                                          [Ext_citations,
                                           Ext_auto_identifiers]
getDefaultExtensions "html"            = extensionsFromList
                                          [Ext_auto_identifiers,
                                           Ext_native_divs,
                                           Ext_line_blocks,
                                           Ext_native_spans]
getDefaultExtensions "html4"           = getDefaultExtensions "html"
getDefaultExtensions "html5"           = getDefaultExtensions "html"
getDefaultExtensions "epub"            = extensionsFromList
                                          [Ext_raw_html,
                                           Ext_native_divs,
                                           Ext_native_spans,
                                           Ext_epub_html_exts]
getDefaultExtensions "epub2"           = getDefaultExtensions "epub"
getDefaultExtensions "epub3"           = getDefaultExtensions "epub"
getDefaultExtensions "latex"           = extensionsFromList
                                          [Ext_smart,
                                           Ext_latex_macros,
                                           Ext_auto_identifiers]
getDefaultExtensions "beamer"          = extensionsFromList
                                          [Ext_smart,
                                           Ext_latex_macros,
                                           Ext_auto_identifiers]
getDefaultExtensions "context"         = extensionsFromList
                                          [Ext_smart,
                                           Ext_auto_identifiers]
getDefaultExtensions "textile"         = extensionsFromList
                                          [Ext_old_dashes,
                                           Ext_smart,
                                           Ext_raw_html,
                                           Ext_auto_identifiers]
getDefaultExtensions "opml"            = pandocExtensions -- affects notes
getDefaultExtensions _                 = extensionsFromList
                                          [Ext_auto_identifiers]


-- | Get all valid extensions for a format. This is used
-- mainly in checking format specifications for validity.
getAllExtensions :: T.Text -> Extensions
getAllExtensions f = universalExtensions <> getAll f
 where
  autoIdExtensions           = extensionsFromList
    [ Ext_auto_identifiers
    , Ext_gfm_auto_identifiers
    , Ext_ascii_identifiers
    ]
  universalExtensions        = extensionsFromList
    [ Ext_east_asian_line_breaks ]
  allMarkdownExtensions =
    pandocExtensions <> autoIdExtensions <>
      extensionsFromList
       [ Ext_old_dashes
       , Ext_angle_brackets_escapable
       , Ext_lists_without_preceding_blankline
       , Ext_four_space_rule
       , Ext_spaced_reference_links
       , Ext_hard_line_breaks
       , Ext_ignore_line_breaks
       , Ext_east_asian_line_breaks
       , Ext_emoji
       , Ext_tex_math_single_backslash
       , Ext_tex_math_double_backslash
       , Ext_markdown_attribute
       , Ext_mmd_title_block
       , Ext_abbreviations
       , Ext_autolink_bare_uris
       , Ext_mmd_link_attributes
       , Ext_mmd_header_identifiers
       , Ext_compact_definition_lists
       , Ext_gutenberg
       , Ext_smart
       , Ext_literate_haskell
       ]
  getAll "markdown_strict"   = allMarkdownExtensions
  getAll "markdown_phpextra" = allMarkdownExtensions
  getAll "markdown_mmd"      = allMarkdownExtensions
  getAll "markdown_github"   = allMarkdownExtensions
  getAll "markdown"          = allMarkdownExtensions
  getAll "ipynb"             = allMarkdownExtensions <> extensionsFromList
    [ Ext_raw_markdown ]
  getAll "docx"            = extensionsFromList
    [ Ext_empty_paragraphs
    , Ext_styles
    ]
  getAll "opendocument"    = extensionsFromList
    [ Ext_empty_paragraphs
    , Ext_native_numbering
    ]
  getAll "odt"             = getAll "opendocument" <> autoIdExtensions
  getAll "muse"            = autoIdExtensions <>
    extensionsFromList
    [ Ext_amuse ]
  getAll "asciidoc"        = autoIdExtensions
  getAll "plain"           = allMarkdownExtensions
  getAll "gfm"             = getAll "commonmark"
  getAll "commonmark"      =
    autoIdExtensions <>
    extensionsFromList
    [ Ext_pipe_tables
    , Ext_autolink_bare_uris
    , Ext_strikeout
    , Ext_task_lists
    , Ext_emoji
    , Ext_raw_html
    , Ext_raw_tex            -- only supported in writer (for math)
    , Ext_implicit_figures
    , Ext_hard_line_breaks
    , Ext_smart
    , Ext_tex_math_dollars
    , Ext_superscript
    , Ext_subscript
    , Ext_definition_lists
    , Ext_footnotes
    , Ext_fancy_lists
    , Ext_fenced_divs
    , Ext_bracketed_spans
    , Ext_raw_attribute
    , Ext_implicit_header_references
    , Ext_attributes
    , Ext_fenced_code_blocks
    , Ext_fenced_code_attributes
    , Ext_backtick_code_blocks
    ]
  getAll "commonmark_x"    = getAll "commonmark"
  getAll "org"             = autoIdExtensions <>
    extensionsFromList
    [ Ext_citations
    , Ext_smart
    ]
  getAll "html"            = autoIdExtensions <>
    extensionsFromList
    [ Ext_native_divs
    , Ext_line_blocks
    , Ext_native_spans
    , Ext_empty_paragraphs
    , Ext_raw_html
    , Ext_raw_tex
    , Ext_task_lists
    , Ext_tex_math_dollars
    , Ext_tex_math_single_backslash
    , Ext_tex_math_double_backslash
    , Ext_literate_haskell
    , Ext_epub_html_exts
    , Ext_smart
    ]
  getAll "html4"           = getAll "html"
  getAll "html5"           = getAll "html"
  getAll "epub"            = getAll "html"
  getAll "epub2"           = getAll "epub"
  getAll "epub3"           = getAll "epub"
  getAll "latex"           = autoIdExtensions <>
    extensionsFromList
    [ Ext_smart
    , Ext_latex_macros
    , Ext_raw_tex
    , Ext_task_lists
    , Ext_literate_haskell
    ]
  getAll "beamer"          = getAll "latex"
  getAll "context"         = autoIdExtensions <>
    extensionsFromList
    [ Ext_smart
    , Ext_raw_tex
    , Ext_ntb
    ]
  getAll "textile"         = autoIdExtensions <>
    extensionsFromList
    [ Ext_old_dashes
    , Ext_smart
    , Ext_raw_tex
    ]
  getAll "opml"            = allMarkdownExtensions -- affects notes
  getAll "twiki"           = autoIdExtensions <>
    extensionsFromList
    [ Ext_smart ]
  getAll "vimwiki"         = autoIdExtensions
  getAll "dokuwiki"        = autoIdExtensions
  getAll "tikiwiki"        = autoIdExtensions
  getAll "rst"             = autoIdExtensions <>
    extensionsFromList
    [ Ext_smart
    , Ext_literate_haskell
    ]
  getAll "mediawiki"       = autoIdExtensions <>
    extensionsFromList
    [ Ext_smart ]
  getAll _                 = mempty


-- | Parse a format-specifying string into a markup format,
-- a set of extensions to enable, and a set of extensions to disable.
parseFormatSpec :: T.Text
                -> Either ParseError (T.Text, [Extension], [Extension])
parseFormatSpec = parse formatSpec ""
  where formatSpec = do
          name <- formatName
          (extsToEnable, extsToDisable) <- foldl (flip ($)) ([],[]) <$>
                                             many extMod
          return (T.pack name, reverse extsToEnable, reverse extsToDisable)
        formatName = many1 $ noneOf "-+"
        extMod = do
          polarity <- oneOf "-+"
          name <- many $ noneOf "-+"
          ext <- case readMay ("Ext_" ++ name) of
                       Just n  -> return n
                       Nothing
                         | name == "lhs" -> return Ext_literate_haskell
                         | otherwise -> Prelude.fail $
                                          "Unknown extension: " ++ name
          return $ \(extsToEnable, extsToDisable) ->
                    case polarity of
                        '+' -> (ext : extsToEnable, extsToDisable)
                        _   -> (extsToEnable, ext : extsToDisable)

$(deriveJSON defaultOptions ''Extension)
$(deriveJSON defaultOptions ''Extensions)
