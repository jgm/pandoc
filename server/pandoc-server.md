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
or as a CGI program.  To use `pandoc-server` as a CGI
program, rename it as `pandoc-server.cgi`.

# OPTIONS

`--port NUM`
:    HTTP port on which to run the server.  Default: 3030.

`--timeout SECONDS`
:    Timeout in seconds, after which a conversion is killed. Default: 2.

`--help`
:    Print this help.

`--version`
:    Print version.

# API

## Root endpoint

The root (`/`) endpoint accepts only POST requests.
It returns a converted document in one of the following
formats, depending on Accept headers:

- `text/plain`
- `application/json`
- `application/octet-stream`

If the result is a binary format (e.g., `epub` or `docx`)
and the content is returned as plain text or JSON, the
binary will be base64 encoded.

The body of the POST request should be a JSON object,
with the following fields.  Only the `text` field is
required; all of the others can be omitted for default
values.

`text` (string)

:   the document to be converted.  Note:
    if the `from` format is binary (e.g., `epub` or `docx`), then
    `text` should be a base64 encoding of the document.

`from` (string, default `"markdown"`)

:   the input format, possibly with extensions, just as it is
    specified on the pandoc command line.

`to` (string, default `"html"`)

:   the output format, possibly with extensions, just as it is
    specified on the pandoc command line.


``` TODO
wrapText              :: Maybe WrapOption
columns               :: Maybe Int
standalone            :: Maybe Bool
template              :: Maybe Text
tabStop               :: Maybe Int
indentedCodeClasses   :: Maybe [Text]
abbreviations         :: Maybe (Set Text)
defaultImageExtension :: Maybe Text
trackChanges          :: Maybe TrackChanges
stripComments         :: Maybe Bool
citeproc              :: Maybe Bool
variables             :: Maybe (DocTemplates.Context Text)
tableOfContents       :: Maybe Bool
incremental           :: Maybe Bool
htmlMathMethod        :: Maybe HTMLMathMethod
numberSections        :: Maybe Bool
numberOffset          :: Maybe [Int]
sectionDivs           :: Maybe Bool
referenceLinks        :: Maybe Bool
dpi                   :: Maybe Int
emailObfuscation      :: Maybe ObfuscationMethod
identifierPrefix      :: Maybe Text
citeMethod            :: Maybe CiteMethod
htmlQTags             :: Maybe Bool
slideLevel            :: Maybe Int
topLevelDivision      :: Maybe TopLevelDivision
listings              :: Maybe Bool
highlightStyle        :: Maybe Text
setextHeaders         :: Maybe Bool
epubSubdirectory      :: Maybe Text
epubFonts             :: Maybe [FilePath]
epubMetadata          :: Maybe Text
epubChapterLevel      :: Maybe Int
tocDepth              :: Maybe Int
referenceDoc          :: Maybe FilePath
referenceLocation     :: Maybe ReferenceLocation
preferAscii           :: Maybe Bool
files                 :: Maybe [(FilePath, Blob)]
```

## `/batch` endpoint

The `/batch` endpoint behaves like the root endpoint,
except for these two points:

- It accepts a JSON array, each element of which is a JSON
  object like the one expected by the root endpoint.
- It returns a JSON array of results.  (It will not return
  plain text or octet-stream, like the root endpoint.)

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

