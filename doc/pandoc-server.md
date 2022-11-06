---
title: pandoc-server
section: 1
date: August 15, 2022
---

# SYNOPSIS

`pandoc-server` [*options*]

# DESCRIPTION

`pandoc-server` is a web server that can perform pandoc
conversions.  It can be used either as a running server
or as a CGI program.

To use `pandoc-server` as a CGI program, rename it (or symlink
it) as `pandoc-server.cgi`. (Note: if you symlink it, you may
need to adjust your webserver's configuration in order to allow
it to follow symlinks for the CGI script.)

All pandoc functions are run in the PandocPure monad, which
ensures that they can do no I/O operations on the server.
This should provide a high degree of security. This security
does, however, impose certain limitations:

- PDFs cannot be produced.

- Filters are not supported.

- Resources cannot be fetched via HTTP.

- Any images, include files, or other resources needed for
  the document conversion must be explicitly included in
  the request, via the `files` field (see below under API).

# OPTIONS

`--port NUM`
:    HTTP port on which to run the server.  Default: 3030.

`--timeout SECONDS`
:    Timeout in seconds, after which a conversion is killed. Default: 2.

     When `pandoc-server` is run as a CGI program, this option
     can be set via the `PANDOC_SERVER_TIMEOUT` environment variable.

`--help`
:    Print this help.

`--version`
:    Print version.

# API

## Root endpoint

The root (`/`) endpoint accepts only POST requests.

### Response

It returns a converted document in one of the following
formats (in order of preference), depending on the `Accept` header:

- `application/octet-stream`
- `text/plain`
- `application/json`

If the result is a binary format (e.g., `epub` or `docx`)
and the content is returned as plain text or JSON, the
binary will be base64 encoded.

If a JSON response is given, it will have one of the
following formats. If the conversion is not successful:

```
{ "error": string with the error message }
```

If the conversion is successful:

```
{ "output": string with textual or base64-encoded binary output,
  "base64": boolean (true means the "output" is base64-encoded),
  "messages": array of message objects (see below) }
```

Each element of the "messages" array will have the format

```
{ "message": string,
  "verbosity": string (either "WARNING" or "INFO") }
```

### Request

The body of the POST request should be a JSON object,
with the following fields.  Only the `text` field is
required; all of the others can be omitted for default
values.  When there are several string alternatives,
the first one given is the default.

`text` (string)

:   The document to be converted.  Note:
    if the `from` format is binary (e.g., `epub` or `docx`), then
    `text` should be a base64 encoding of the document.

`from` (string, default `"markdown"`)

:   The input format, possibly with extensions, just as it is
    specified on the pandoc command line.

`to` (string, default `"html"`)

:   The output format, possibly with extensions, just as it is
    specified on the pandoc command line.

`shift-heading-level-by` (integer, default 0)

:   Increase or decrease the level of all headings.

`indented-code-classes` (array of strings)

:   List of classes to be applied to indented Markdown code blocks.

`default-image-extension` (string)

:   Extension to be applied to image sources that lack extensions
    (e.g. `".jpg"`).

`metadata` (JSON map)

:   String-valued metadata.

`tab-stop` (integer, default 4)

:   Tab stop (spaces per tab).

`track-changes` (`"accept"|"reject"|"all"`)

:   Specifies what to do with insertions, deletions, and
    comments produced by the MS Word "Track Changes" feature. Only
    affects docx input.

`abbreviations` (file path)

:   List of strings to be regarded as abbreviations when
    parsing Markdown. See `--abbreviations` in `pandoc(1)` for
    details.

`standalone` (boolean, default false)

:   If true, causes a standalone document to be produced, using
    the default template or the custom template specified using
    `template`.  If false, a fragment will be produced.

`template` (string)

:   String contents of a document template (see Templates in
    `pandoc(1)` for the format).

`variables` (JSON map)

:   Variables to be interpolated in the template. (See Templates
    in `pandoc(1)`.)

`dpi` (integer, default 96)

:   Dots-per-inch to use for conversions between pixels and
    other measurements (for image sizes).

`wrap` (`"auto"|"preserve"|"none"`)

:   Text wrapping option: either `"auto"` (automatic
    hard-wrapping to fit within a column width), `"preserve"`
    (insert newlines where they are present in the source),
    or `"none"` (don't insert any unnecessary newlines at all).

`columns` (integer, default 72)

:   Column width (affects text wrapping and calculation of
    table column widths in plain text formats)

`table-of-contents` (boolean, default false)

:   Include a table of contents (in supported formats).

`toc-depth` (integer, default 3)

:   Depth of sections to include in the table of contents.

`strip-comments` (boolean, default false)

:   Causes HTML comments to be stripped in Markdown or Textile
    source, instead of being passed through to the output format.

`highlight-style` (string, leave unset for no highlighting)

:   Specify the style to use for syntax highlighting of code.
    Standard styles are `"pygments"` (the default), `"kate"`,
    `"monochrome"`, `"breezeDark"`, `"espresso"`, `"zenburn"`,
    `"haddock"`, and `"tango"`. Alternatively, the path of
    a `.theme` with a KDE syntax theme may be used (in this
    case, the relevant file contents must also be included
    in `files`, see below).

`embed-resources`

:   Embed images, scripts, styles and other resources in an HTML
    document using `data` URIs.  Note that this will not work
    unless the contents of all external resources are included
    under `files`.

`html-q-tags` (boolean, default false)

:   Use `<q>` elements in HTML instead of literal quotation marks.

`ascii` (boolean, default false)

:   Use entities and escapes when possible to avoid non-ASCII
    characters in the output.

`reference-links` (boolean, default false)

:   Create reference links rather than inline links in Markdown output.

`reference-location` (`"document"|"section"|"block"`)

:   Determines whether link references and footnotes are placed
    at the end of the document, the end of the section, or the
    end of the block (e.g. paragraph), in
    certain formats. (See `pandoc(1)` under `--reference-location`.)

`setext-headers` (boolean, default false)

:   Use Setext (underlined) headings instead of ATX (`#`-prefixed)
    in Markdown output.

`top-level-division` (`"default"|"part"|"chapter"|"section"`)

:   Determines how top-level headings are interpreted in
    LaTeX, ConTeXt, DocBook, and TEI.  The `"default"` value
    tries to choose the best interpretation based on heuristics.

`number-sections` (boolean, default false)

:   Automatically number sections (in supported formats).


`number-offset` (array of integers)

:   Offsets to be added to each component of the section number.
    For example, `[1]` will cause the first section to be
    numbered "2" and the first subsection "2.1"; `[0,1]` will
    cause the first section to be numbered "1" and the first
    subsection "1.2."

`html-math-method` (`"plain"|"webtex"|"gladtex"|"mathml"|"mathjax"|"katex"`)

:   Determines how math is represented in HTML.

`listings` (boolean, default false)

:   Use the `listings` package to format code in LaTeX output.

`incremental` (boolean, default false)

:   If true, lists appear incrementally by default in slide shows.

`slide-level` (integer)

:   Heading level that deterimes slide divisions in slide shows.
    The default is to pick the highest heading level under which
    there is body text.

`section-divs` (boolean, default false)

:   Arrange the document into a hierarchy of nested sections
    based on the headings.

`email-obfuscation` (`"none"|"references"|"javascript"`)

:   Determines how email addresses are obfuscated in HTML.

`identifier-prefix` (string)

:   Prefix to be added to all automatically-generated identifiers.

`title-prefix` (string)

:   Prefix to be added to the title in the HTML header.

`reference-doc` (file path)

:   Reference doc to use in creating `docx` or `odt` or `pptx`.
    See `pandoc(1)` under `--reference-doc` for details.
    The contents of the file must be included under `files`.

`epub-cover-image` (file path)

:   Cover image for EPUB.
    The contents of the file must be included under `files`.

`epub-metadata` (file path)

:   Path of file containing Dublin core XML elements to be used for
    EPUB metadata.  The contents of the file must be included
    under `files`.

`epub-chapter-level` (integer, default 1)

:   Heading level at which chapter splitting occurs in EPUBs.

`epub-subdirectory` (string, default "EPUB")

:   Name of content subdirectory in the EPUB container.

`epub-fonts` (array of file paths)

:   Fonts to include in the EPUB. The fonts themselves must be
    included in `files` (see below).

`ipynb-output` (`"best"|"all"|"none"`)

:   Determines how ipynb output cells are treated. `all` means
    that all of the data formats included in the original are
    preserved.  `none` means that the contents of data cells
    are omitted.  `best` causes pandoc to try to pick the
    richest data block in each output cell that is compatible
    with the output format.

`citeproc` (boolean, default false)

:   Causes citations to be processed using citeproc.  See
    Citations in `pandoc(1)` for details.

`bibliography` (array of file paths)

:   Files containing bibliographic data. The contents of the
    files must be included in `files`.

`csl` (file path)

:   CSL style file. The contents of the file must be included
    in `files`.

`cite-method` (`"citeproc"|"natbib"|"biblatex"`)

:   Determines how citations are formatted in LaTeX output.

`files` (JSON mapping of file paths to base64-encoded strings)

:   Any files needed for the conversion, including images
    referred to in the document source, should be included here.
    Binary data must be base64-encoded.  Textual data may be
    left as it is, unless it is *also* valid base 64 data,
    in which case it will be interpreted that way.

## `/batch` endpoint

The `/batch` endpoint behaves like the root endpoint,
except for these two points:

- It accepts a JSON array, each element of which is a JSON
  object like the one expected by the root endpoint.
- It returns a JSON array of JSON results.

This endpoint can be used to convert a sequence of small
snippets in one request.

## `/version` endpoint

The `/version` endpoint accepts a GET request and returns
the pandoc version as a plain or JSON-encoded string,
depending on Accept headers.

## `/babelmark` endpoint

The `/babelmark` endpoint accepts a GET request with
the following query parameters:

- `text` (required string)
- `from` (optional string, default is `"markdown"`)
- `to` (optional string, default is `"html"`)
- `standalone` (optional boolean, default is `false`)

It returns a JSON object with fields `html` and `version`.
This endpoint is designed to support the
[Babelmark]()https://babelmark.github.io website.

# AUTHORS

Copyright 2022 John MacFarlane (jgm@berkeley.edu). Released
under the [GPL], version 2 or greater.  This software carries no
warranty of any kind.  (See COPYRIGHT for full copyright and
warranty notices.)

[GPL]: https://www.gnu.org/copyleft/gpl.html "GNU General Public License"

