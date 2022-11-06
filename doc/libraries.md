---
title: The pandoc family of libraries
author: John MacFarlane
---

The following Haskell libraries have been developed to support
pandoc:


[skylighting-core] and [skylighting]
:   Syntax highlighting engine supporting over 140 languages.

[citeproc]
:   Citation processing using CSL stylesheets.

[texmath]
:   Conversion of math between tex, Word equation, MathML, and GNU eqn.

[unicode-collation]
:   Proper Unicode collation (sorting).

[doclayout]
:   Combinators for laying out a textual document, with support
    for line wrapping, tabular layout, and more.

[doctemplates]
:   Supports pandoc's templates.

[commonmark], [commonmark-extensions], and [commonmark-pandoc]
:   Efficient, standards-compliant parser for commonmark and extensions.

[ipynb]
:   Representation of Jupyter notebooks and conversion to and
    from JSON.

[zip-archive]
:   A pure zip file creator and extractor, used by pandoc for
    docx, ODT, and EPUB.

[rfc5051]
:   Simple unicode collation (used for citation sorting).

[emojis]
:   Conversion between emoji characters and aliases.

[jira-wiki-markup]
:   Support for parsing Jira wiki syntax.

[gridtables]
:   Support for parsing grid style textual tables.

[hslua-objectorientation], [hslua-packaging]
:   Bindings, wrappers, and helper functions to access Haskell data
    types from Lua via an object-oriented interface.

[hslua-module-path], [-system], [-text], and [-version]
:   Lua modules that expose functionality of basic Haskell
    libraries to Lua.

[hslua-aeson]
:   Converter from aeson data types to Lua objects.

[hslua-cli]
:   Command-line interface mimicking the default `lua` executable.

[skylighting]: https://hackage.haskell.org/package/skylighting
[skylighting-core]: https://hackage.haskell.org/package/skylighting-core
[citeproc]: https://hackage.haskell.org/package/citeproc
[texmath]: https://hackage.haskell.org/package/texmath
[doclayout]: https://hackage.haskell.org/package/doclayout
[doctemplates]: https://hackage.haskell.org/package/doctemplates
[commonmark]: https://hackage.haskell.org/package/commonmark
[commonmark-extensions]: https://hackage.haskell.org/package/commonmark-extensions
[commonmark-pandoc]: https://hackage.haskell.org/package/commonmark-pandoc
[ipynb]: https://hackage.haskell.org/package/ipynb
[zip-archive]: https://hackage.haskell.org/package/zip-archive
[rfc5051]: https://hackage.haskell.org/package/rfc5051
[emojis]: https://hackage.haskell.org/package/emojis
[jira-wiki-markup]: https://hackage.haskell.org/package/jira-wiki-markup
[unicode-collation]: https://hackage.haskell.org/package/unicode-collation
[gridtables]: https://hackage.haskell.org/package/gridtables
[hslua-objectorientation]: https://hackage.haskell.org/package/hslua-objectorientation
[hslua-packaging]: https://hackage.haskell.org/package/hslua-packaging
[hslua-aeson]: https://hackage.haskell.org/package/hslua-aeson
[hslua-cli]: https://hackage.haskell.org/package/hslua-cli
[hslua-module-path]: https://hackage.haskell.org/package/hslua-module-path
[-system]: https://hackage.haskell.org/package/hslua-module-system
[-text]: https://hackage.haskell.org/package/hslua-module-text
[-version]: https://hackage.haskell.org/package/hslua-module-version
