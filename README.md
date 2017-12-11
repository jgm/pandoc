Pandoc
======

[![github release](https://img.shields.io/github/release/jgm/pandoc.svg?label=current+release)](https://github.com/jgm/pandoc/releases)
[![hackage release](https://img.shields.io/hackage/v/pandoc.svg?label=hackage)](http://hackage.haskell.org/package/pandoc)
[![homebrew](https://img.shields.io/homebrew/v/pandoc.svg)](http://brewformulas.org/Pandoc)
[![stackage LTS package](http://stackage.org/package/pandoc/badge/lts)](http://stackage.org/lts/package/pandoc)
[![travis build status](https://img.shields.io/travis/jgm/pandoc/master.svg?label=travis+build)](https://travis-ci.org/jgm/pandoc)
[![appveyor build status](https://ci.appveyor.com/api/projects/status/nvqs4ct090igjiqc?svg=true)](https://ci.appveyor.com/project/jgm/pandoc)
[![license](https://img.shields.io/badge/license-GPLv2+-lightgray.svg)](https://www.gnu.org/licenses/gpl.html)
[![pandoc-discuss on google groups](https://img.shields.io/badge/pandoc-discuss-red.svg?style=social)](https://groups.google.com/forum/#!forum/pandoc-discuss)


The universal markup converter
------------------------------

Pandoc is a [Haskell] library for converting from one markup format to
another, and a command-line tool that uses this library. It can read
[Markdown], [CommonMark], [PHP Markdown Extra], [GitHub-Flavored
Markdown], [MultiMarkdown], and (subsets of) [Textile],
[reStructuredText], [HTML], [LaTeX], [MediaWiki markup], [TWiki markup],
[TikiWiki markup], [Creole 1.0], [Haddock markup], [OPML], [Emacs Org mode],
[DocBook], [Muse], [txt2tags], [Vimwiki], [EPUB], [ODT], and [Word docx];
and it can write plain text, [Markdown], [CommonMark], [PHP Markdown
Extra], [GitHub-Flavored Markdown], [MultiMarkdown],
[reStructuredText], [XHTML], [HTML5], [LaTeX] \(including
[`beamer`] slide shows\), [ConTeXt], [RTF], [OPML], [DocBook],
[OpenDocument], [ODT], [Word docx], [GNU Texinfo], [MediaWiki
markup], [DokuWiki markup], [ZimWiki markup], [Haddock markup],
[EPUB] \(v2 or v3\), [FictionBook2], [Textile], [groff man],
[groff ms], [Emacs Org mode], [AsciiDoc], [InDesign ICML], [TEI
Simple], [Muse], [PowerPoint] slide shows and [Slidy], [Slideous],
[DZSlides], [reveal.js] or [S5] HTML slide shows. It can also produce
[PDF] output on systems where LaTeX, ConTeXt, `pdfroff`, `wkhtmltopdf`,
`prince`, or `weasyprint` is installed.

Pandoc's enhanced version of Markdown includes syntax for [footnotes],
[tables], flexible [ordered lists], [definition lists], [fenced code
blocks], [superscripts and subscripts], [strikeout], [metadata blocks],
automatic tables of contents, embedded LaTeX [math], [citations], and
[Markdown inside HTML block elements](#extension-markdown_in_html_blocks).
(These enhancements, described further under
[Pandoc's Markdown], can be disabled using the `markdown_strict` input
or output format.)

In contrast to most existing tools for converting Markdown to HTML, which
use regex substitutions, pandoc has a modular design: it consists of a
set of readers, which parse text in a given format and produce a native
representation of the document, and a set of writers, which convert
this native representation into a target format. Thus, adding an input
or output format requires only adding a reader or writer.

Because pandoc's intermediate representation of a document is less
expressive than many of the formats it converts between, one should
not expect perfect conversions between every format and every other.
Pandoc attempts to preserve the structural elements of a document, but
not formatting details such as margin size.  And some document elements,
such as complex tables, may not fit into pandoc's simple document
model.  While conversions from pandoc's Markdown to all formats aspire
to be perfect, conversions from formats more expressive than pandoc's
Markdown can be expected to be lossy.

[Markdown]: http://daringfireball.net/projects/markdown/
[CommonMark]: http://commonmark.org
[PHP Markdown Extra]: https://michelf.ca/projects/php-markdown/extra/
[GitHub-Flavored Markdown]: https://help.github.com/articles/github-flavored-markdown/
[MultiMarkdown]: http://fletcherpenney.net/multimarkdown/
[reStructuredText]: http://docutils.sourceforge.net/docs/ref/rst/introduction.html
[S5]: http://meyerweb.com/eric/tools/s5/
[Slidy]: http://www.w3.org/Talks/Tools/Slidy/
[Slideous]: http://goessner.net/articles/slideous/
[HTML]: http://www.w3.org/html/
[HTML5]: http://www.w3.org/TR/html5/
[XHTML]: http://www.w3.org/TR/xhtml1/
[LaTeX]: http://latex-project.org
[`beamer`]: https://ctan.org/pkg/beamer
[Beamer User's Guide]: http://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/beamer/doc/beameruserguide.pdf
[ConTeXt]: http://www.contextgarden.net/
[RTF]: http://en.wikipedia.org/wiki/Rich_Text_Format
[Creole 1.0]: http://www.wikicreole.org/wiki/Creole1.0
[DocBook]: http://docbook.org
[txt2tags]: http://txt2tags.org
[EPUB]: http://idpf.org/epub
[OPML]: http://dev.opml.org/spec2.html
[OpenDocument]: http://opendocument.xml.org
[ODT]: http://en.wikipedia.org/wiki/OpenDocument
[Textile]: http://redcloth.org/textile
[MediaWiki markup]: https://www.mediawiki.org/wiki/Help:Formatting
[DokuWiki markup]: https://www.dokuwiki.org/dokuwiki
[ZimWiki markup]: http://zim-wiki.org/manual/Help/Wiki_Syntax.html
[TWiki markup]: http://twiki.org/cgi-bin/view/TWiki/TextFormattingRules
[Haddock markup]: https://www.haskell.org/haddock/doc/html/ch03s08.html
[groff man]: http://man7.org/linux/man-pages/man7/groff_man.7.html
[groff ms]: http://man7.org/linux/man-pages/man7/groff_ms.7.html
[Haskell]: https://www.haskell.org
[GNU Texinfo]: http://www.gnu.org/software/texinfo/
[Emacs Org mode]: http://orgmode.org
[AsciiDoc]: http://www.methods.co.nz/asciidoc/
[DZSlides]: http://paulrouget.com/dzslides/
[Word docx]: https://en.wikipedia.org/wiki/Office_Open_XML
[PDF]: https://www.adobe.com/pdf/
[reveal.js]: http://lab.hakim.se/reveal-js/
[FictionBook2]: http://www.fictionbook.org/index.php/Eng:XML_Schema_Fictionbook_2.1
[InDesign ICML]: https://www.adobe.com/content/dam/Adobe/en/devnet/indesign/cs55-docs/IDML/idml-specification.pdf
[TEI Simple]: https://github.com/TEIC/TEI-Simple
[Muse]: https://amusewiki.org/library/manual
[PowerPoint]: https://en.wikipedia.org/wiki/Microsoft_PowerPoint
[Vimwiki]: https://vimwiki.github.io




[footnotes]: http://pandoc.org/MANUAL.html#footnotes
[tables]: http://pandoc.org/MANUAL.html#tables
[ordered lists]: http://pandoc.org/MANUAL.html#ordered-lists
[definition lists]: http://pandoc.org/MANUAL.html#definition-lists
[fenced code blocks]: http://pandoc.org/MANUAL.html#fenced-code-blocks
[superscripts and subscripts]: http://pandoc.org/MANUAL.html#superscripts-and-subscripts
[strikeout]: http://pandoc.org/MANUAL.html#strikeout
[metadata blocks]: http://pandoc.org/MANUAL.html#metadata-blocks
[math]: http://pandoc.org/MANUAL.html#math
[citations]: http://pandoc.org/MANUAL.html#citations
[Markdown inside HTML block elements]: http://pandoc.org/MANUAL.html#extension-markdown_in_html_blocks
[Pandoc's Markdown]: http://pandoc.org/MANUAL.html#pandocs-markdown

Installing
----------

Here's [how to install pandoc](INSTALL.md).

Documentation
-------------

Pandoc's website contains a full [User's Guide](https://pandoc.org/MANUAL.html).
It is also available [here](MANUAL.txt) as pandoc-flavored Markdown.
The website also contains some [examples of the use of
pandoc](https://pandoc.org/demos.html) and a limited [online
demo](https://pandoc.org/try).

Contributing
------------

Pull requests, bug reports, and feature requests are welcome.  Please make
sure to read [the contributor guidelines](CONTRIBUTING.md) before opening a
new issue.


License
-------

© 2006-2017 John MacFarlane (jgm@berkeley.edu). Released under the
[GPL], version 2 or greater.  This software carries no warranty of
any kind.  (See COPYRIGHT for full copyright and warranty notices.)

[GPL]: http://www.gnu.org/copyleft/gpl.html "GNU General Public License"
