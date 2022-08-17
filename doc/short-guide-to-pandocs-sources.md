---
title: Short guide to pandoc's sources
subtitle: Laying a path for code wanderers
author: Albert Krewinkel
date: 2021-06-07
---

Pandoc, the universal document converter, can serve as a nice intro
into functional programming with Haskell. For many contributors,
including the author of this guide, pandoc was their first real
exposure to this language. Despite its impressive size of more than
60.000 lines of Haskell code (excluding the test suite), pandoc is
still very approachable due to its modular architecture. It can
serve as an interesting subject for learning.

This guide exists to navigate the large amount of sources, to
lay-out a path that can be followed for learning, and to explain the
underlying concepts.

A basic understanding of Haskell and of pandoc's functionality is
assumed.

# Getting the code

Pandoc has a publicly accessible git repository on GitHub:
<https://github.com/jgm/pandoc>. To get a local copy of the source:

    git clone https://github.com/jgm/pandoc

The source for the main pandoc program is `app/pandoc.hs`. The
source for the pandoc library is in `src/`, the source for the tests
is in `test/`, and the source for the benchmarks is in `benchmark/`.

Core type definitions are in the separate [*pandoc-types* repo].
Get it with

    git clone https://github.com/jgm/pandoc-types

The organization of library and test sources is identical to the
main repo.

[*pandoc-types* repo]: https://github.com/jgm/pandoc-types

# Document representation

The way documents are represented in pandoc is part of its success.
Every document is read into one central data structure, the
so-called *abstract syntax tree* (AST).

The AST is defined in module `Text.Pandoc.Definition` in package
[*pandoc-types*].

It is not necessary to understand the AST in detail, just check-out
the following points:

 * The [`Pandoc`][def-Pandoc] type serves as the central structure.

 * A document has metadata and a list of "block" elements.

 * There are various types of [blocks][def-Block]; some contain raw
   text, others contain "Inline" elements.

 * [Inlines][def-Inline] are "running text", with many different
   types. The most important constructors are `Str` (a word),
   `Space` (a space char), `Emph` (emphasized text), and `Strong`
   (strongly emphasized text). It's worth checking their
   definitions.

 * Element attributes are captured as [`Attr`][def-Attr], which is a
   triple of the element identifier, its classes, and the key-value
   pairs.^[For plans to change this see [jgm/pandoc-types#88].]

[*pandoc-types*]: https://hackage.haskell.org/package/pandoc-types
[jgm/pandoc-types#88]: https://github.com/jgm/pandoc-types/issues/88
[def-Pandoc]: https://hackage.haskell.org/package/pandoc-types/docs/src/Text.Pandoc.Definition.html#Pandoc
[def-Block]: https://hackage.haskell.org/package/pandoc-types/docs/src/Text.Pandoc.Definition.html#Block
[def-Inline]: https://hackage.haskell.org/package/pandoc-types/docs/src/Text.Pandoc.Definition.html#Inline
[def-Attr]: https://hackage.haskell.org/package/pandoc-types/docs/src/Text.Pandoc.Definition.html#Attr

# Basic architecture

Take a look at pandoc's source files. The code is below the `src`
directory, in the `Text.Pandoc` module. The basic flow is:

 1. Document is parsed into the internal representation by a
    *reader*;

 2. the document AST is modified (optional);

 3. then the internal representation is converted into the target
    format by a *writer*.

The [*readers*] can be found in `Text.Pandoc.Readers`, while the
[*writers*] are submodules of `Text.Pandoc.Writers`. The document
modification step is powerful and used in different ways, e.g., in
[*filters*].

These parts are the "muscles" of pandoc, which do the heavy lifting.
Everything else can be thought of as the bones and fibers to which
these parts are attached and which make them usable.

# Writers

Writers are usually simpler than readers and therefore easier to
grasp.

Broadly speaking, there are three kind of writers:

 1. Text writers: these are used for lightweight markup languages
    and generate plain text output. Examples: Markdown, Org,
    reStructuredText.
 2. XML writers, which convert the AST into structured XML.
    Examples: HTML, JATS.
 3. Binary writers, which are like XML writers, but combine the
    output with other data and zip it into a single file. Examples:
    docx, epub.

 Most writers follow a common pattern and have three main functions:
 docTo*Format*, blockTo*Format* and inlineTo*Format*. Each converts
 the `Pandoc`, `Block`, and `Inline` elements, respectively. The
 *XWiki* and *TEI* writers are comparatively simple and suitable
 samples when taking a first look.

 Most writers are self-contained in that most of the conversion code
 is within a single module. However, newer writers often use a
 different setup: those are built around modules from an external
 package. The details of how to serialize the document are not in
 the writer module itself, but in an external module. The writer
 only has to convert pandoc's AST into the document representation
 used by the module. Good examples: commonmark, jira.

## DocLayout

All writers build on the `doclayout` package. It can be thought of
as a pretty printer with extra features suitable for lightweight
markup languages. E.g., multiple blank lines are collapsed into a
single blank line, unless multiple blank lines are specifically
requested.  This simplifies the code significantly.

See the repo at https://github.com/jgm/doclayout, and the [hackage
documentation](https://hackage.haskell.org/package/doclayout)

# Readers

The same distinction that applies to writers also applies to
readers. Readers for XML formats use XML parsing libraries, while
plain text formats are parsed with [parsec].

## Builders

The plain type constructors from the [`Text.Pandoc.Definition`]
module can be difficult to use, which is why the module
[`Text.Pandoc.Builder`] exists. It offers functions to conveniently
build and combine AST elements.

The most interesting and important types in `Builder` are
[`Blocks`][def-Blocks] and [`Inlines`][def-Inlines]. All type
constructors use simple lists for sequences of AST elements.
Building lists can be awkward and often comes with bad performance
characteristics, esp. when appending. The `Blocks` and `Inlines`
types are better suited for these operations and are therefore used
extensively in builder functions.

The builder functions are named with the convention that the suffix
`With` is added if the first argument is an `Attr`; there is usually
another function without that suffix, creating an element with no
attributes.

[def-Blocks]: https://hackage.haskell.org/package/pandoc-types/docs/src/Text.Pandoc.Builder.html#Blocks
[def-Inlines]: https://hackage.haskell.org/package/pandoc-types/docs/src/Text.Pandoc.Builder.html#Inlines
[parsec]: https://hackage.haskell.org/package/parsec

# PandocMonad

Looking at the readers and writers, one will notice that they all
operate within the `PandocMonad` type class. This class gives access
to options, file operations, and other shared information. The
typeclass has two main implementations: one operates in IO, so on
the "real world", while the other provides a pure functional
interface, suitable to "mock" an environment for testing.

# Document modifications

One of the big advantages of a central document structure is that it
allows document modifications via a unified interface. This section
describes the multiple ways in which the document can be altered.

## Walkable

Document traversal happens through the `Walkable` class in module
`Text.Pandoc.Walk` ([*pandoc-types* package]).

## Transformations

Transformations are simple modifications controllable through
command-line options.

## Filters

Filters allow to use Lua or any external language to perform
document transformations.


[`Text.Pandoc.Builder`]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Builder.html
[`Text.Pandoc.Definition`]: https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html

# Module overview

The library is structured as follows:

  - `Text.Pandoc` is a top-level module that exports what is needed
    by most users of the library.  Any patches that add new readers
    or writers will need to make changes here, too.
  - `Text.Pandoc.Definition` (in `pandoc-types`) defines the types
    used for representing a pandoc document.
  - `Text.Pandoc.Builder` (in `pandoc-types`) provides functions for
    building pandoc documents programmatically.
  - `Text.Pandoc.Generics` (in `pandoc-types`) provides functions allowing
    you to promote functions that operate on parts of pandoc documents
    to functions that operate on whole pandoc documents, walking the
    tree automatically.
  - `Text.Pandoc.Readers.*` are the readers, and `Text.Pandoc.Writers.*`
    are the writers.
  - `Text.Pandoc.Citeproc.*` contain the code for citation handling,
    including an interface to the [citeproc] library.
  - `Text.Pandoc.Data` is used to embed data files when the `embed_data_files`
    cabal flag is used.
  - `Text.Pandoc.Emoji` is a thin wrapper around [emojis].
  - `Text.Pandoc.Highlighting` contains the interface to the
    skylighting library, which is used for code syntax highlighting.
  - `Text.Pandoc.ImageSize` is a utility module containing functions for
    calculating image sizes from the contents of image files.
  - `Text.Pandoc.MIME` contains functions for associating MIME types
    with extensions.
  - `Text.Pandoc.Lua.*` implement Lua filters.
  - `Text.Pandoc.Options` defines reader and writer options.
  - `Text.Pandoc.PDF` contains functions for producing PDFs.
  - `Text.Pandoc.Parsing` contains parsing functions used in multiple readers.
    the needs of pandoc.
  - `Text.Pandoc.SelfContained` contains functions for making an HTML
    file "self-contained," by importing remotely linked images, CSS,
    and JavaScript and turning them into `data:` URLs.
  - `Text.Pandoc.Shared` is a grab-bag of shared utility functions.
  - `Text.Pandoc.Writers.Shared` contains utilities used in writers only.
  - `Text.Pandoc.Slides` contains functions for splitting a markdown document
    into slides, using the conventions described in the MANUAL.
  - `Text.Pandoc.Templates` defines pandoc's templating system.
  - `Text.Pandoc.UTF8` contains functions for converting text to and from
    UTF8 bytestrings (strict and lazy).
  - `Text.Pandoc.Asciify` contains functions to derive ascii versions of
    identifiers that use accented characters.
  - `Text.Pandoc.UUID` contains functions for generating UUIDs.
  - `Text.Pandoc.XML` contains functions for formatting XML.


<!--
# Templating
## DocTemplates
-->
