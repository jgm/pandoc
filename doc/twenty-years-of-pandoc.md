# Twenty Years of Pandoc

On August 3, 2006, I uploaded the first version of pandoc to my
website, releasing it under the free GPL license. Pandoc 0.1
consisted of about 3000 lines of Haskell code, with no
dependencies aside from GHC's standard library. It could convert
Markdown, reStructuredText, HTML, and LaTeX documents into
any of these formats, plus RTF or S5. I had no idea at the time
that this would just be the first of over two hundred releases
over the next twenty years; that the project would become the
[most popular program written in Haskell][githubranking];
that I would spend countless hours on bug-fixes, improvement, and project
management; that I would collaborate with programmers in many
other countries; that pandoc would come to support over fifty
document formats; that it would allow automatic generation of
citations and bibliographies; that it would become integrated
into academic writing tools like [Quarto](https://quarto.org)
and [Jupyter Notebook](https://jupyter.org); that it
would be installed on millions of computers around the world.

[githubranking]: https://github.com/EvanLi/Github-Ranking/blob/master/Top100/Haskell.md

How did this happen? I want to take advantage of pandoc's birthday
to tell the story of the project, as best I can remember it.

::: {style="text-align:right"}
*John MacFarlane*\
*August 2, 2026*
:::

## Prehistory

People often ask: Why is pandoc written in Haskell? There could
have been good answers to this question: Haskell is a
very good language for writing this kind of application. But in fact,
I didn't decide to write a document converter, then decide to use
Haskell for it. I decided to use Haskell, and then decided to
write a document converter in it.

I had heard about Haskell from the blog of a philosophical logician
friend, [Greg Restall](https://consequently.org).
Of an introductory book on Haskell, he said: "I’m
glad that this wasn’t the textbook in my introductory computer
science course, long ago in 1986. If it were, I may have fallen
in love with computing and never become a philosopher"
([consequently.org](https://consequently.org/news/2004/05/10/haskell_and_logic/)).

Intrigued by this (and not heeding Restall's warning about
the potential effects on my future philosophical productivity), I
read [*A Gentle Introduction to
Haskell*](https://www.haskell.org/tutorial/index.html) to get a
basic understanding of the language. But the only way to really
learn a programming language is to write something in it. I saw
that Haskell was good for writing parsers and compilers, and it
came with a really nice parser combinator library (parsec), so I
decided to write a Markdown parser.

At that time, there were implementations of Markdown in Perl,
Python, Ruby, and PHP; they all transformed Markdown directly to HTML
through a sequence of [regex](https://en.wikipedia.org/wiki/Regular_expression)
transformations. Pandoc
took a different approach. It parsed the Markdown using parser
combinators and produced a real [abstract syntax tree
(AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree), which
it could then render to HTML or another format. This was a more reliable
architecture (avoiding many quirks of the regex versions). It was
also a more extensible one: by writing *N* parsers ("readers")
and *M* renderers ("writers"), one could support *N × M* conversions.
Soon I added a reader for reStructuredText,
because I kept a lot of my lecture notes and handouts in that
format. And I added a writer for LaTeX, because I wanted to be
able to produce PDFs. Then I added a writer for Markdown, so I
could start to convert my reStructuredText notes to Markdown. And
from there the project just snowballed.

Thus, a project that started out as nothing more than the product
of procrastination was nurtured by the joy of writing in
Haskell and by its increasing usefulness for my own academic work.

## First releases (2006--8)

In August 3, 2006, I decided to make the source code available on
my website. By now pandoc supported HTML, LaTeX, RST, and
Markdown as input and output formats, and RTF as an output
format; also PDF via LaTeX.

![The first release](./pandoc-0.1.jpg){width=100% style="border: 1px solid grey; padding: 1em;"}

I made no attempts to advertise the project, other than emailing
two friends. This was before social media (which I've never used
anyway), before GitHub, and before Hackage, the Haskell
package repository. But apparently some people stumbled across it
on my website and started using it. In October I was contacted by
a Turkish developer, Recai Oktaş, who was trying to get certified
as a Debian developer and wanted to package pandoc for Debian
linux. So I worked with him to do that. This was a great learning
experience for me and it greatly increased the visibility of the
project.

During 2007, I continued to improve pandoc, largely guided by my
own needs.  Version 0.3 added the DocBook writer and the now-standard
syntax for footnotes in Markdown. Version 0.4 added support for Markdown
tables, definition lists, super/subscript, strikeout, and
enhanced ordered lists, as well as writers for groff man pages
and ConTeXt. This was the first release to go on the
[Hackage](https://hackage.haskell.org) Haskell package repository, which
was started in 2007. The Hackage archive and the new `cabal-install`
tool, which automatically resolved and fetched dependencies, opened
up the possibility of depending on external packages.

## Pandoc 1 (2008--17)

Pandoc 1.0 was released in September 2008, with new writers for
MediaWiki, GNU Texinfo (contributed by Peter Wang), OpenDocument
(contributed by Andrea Rossato), ODT, and delimited code
blocks (now called "fenced") with automatic syntax highlighting.
Support for ODT requires the ability to create a zip archive, and
at the time there was no Haskell package for this, so I created
one ([`zip-archive`](https://hackage.haskell.org/package/zip-archive)),
using the excellent `binary` package for
binary parsing and serialization. Support for syntax highlighting
required a syntax highlighting library, which also did not exist
in Haskell. For this, I wrote
[`highlighting-kate`](https://hackage.haskell.org/package/highlighting-kate),
which parsed the XML syntax definitions used by the Kate text
editor and turned them into Haskell code highlighters. This allowed
pandoc to support a large number of syntaxes right off the bat.
This version also contained support for automatic generation of
citations and a bibliography using [CSL style](https://citationstyles.org),
using Andrea Rossato's [`citeproc-hs`](https://hackage.haskell.org/package/citeproc-hs) library.

Throughout this period, I was involved in discussions with other
Markdown implementers on the (now defunct)
[`markdown-discuss` mailing
list](https://www.mail-archive.com/markdown-discuss@six.pairlist.net/mail15.html). The syntax for delimited code blocks, which pandoc
supported long before GitHub popularized fenced code blocks, was
worked out in collaboration with Michel Fortin, the maintainer of
PHP Markdown Extra. I took care when adding extensions to
pandoc's Markdown to pay attention to prior art, for example
copying PHP Markdown Extra's definition list syntax. During this
period, I also became aware of many ambiguities in Markdown's
syntax---a situation I would later try to improve in the
commonmark project.

The next big change to pandoc came in version 1.4 (released in
January 2010), which introduced a flexible [template](https://pandoc.org/MANUAL.html#templates) system,
replacing hard-coded headers and making pandoc's output much more
customizable.

In 2010, we moved from [Google Code](https://code.google.com/archive/p/pandoc/)
to [GitHub](https://github.com/jgm/pandoc), which would do even
more to increase the visibility of the project. Further releases
in 2010 and 2011 added support for EPUB output, Org-mode output (due
to Puneeth Chaganti), and Textile input (due to Paul Rivier).
Pandoc also gained support for converting TeX math to MathML
(for DocBook or HTML), via my [`texmath`](https://github.com/jgm/texmath) library.

Pandoc 1.9, published in 2012, finally made it possible to
produce Word docx output. To handle the equations properly,
I added support for Word's OMML format to `texmath`.
This release also added an AsciiDoc writer and support
for Beamer and DZSlides, and in 1.9.3 we gained a DocBook
reader (with contributions from Mauro Bieg, who became a
long-time contributor).

In 2013, we focused on several features that made pandoc
much more flexible and customizable. The first was a
fine-grained system of [Markdown "extensions,"](https://pandoc.org/MANUAL.html#extensions) allowing support
for the many variants of Markdown that were then proliferating.
The second was the ability to include [YAML metadata
blocks](https://pandoc.org/MANUAL.html#extension-yaml_metadata_block)
in Markdown, with arbitrary structured fields that populate
template variables. The third was the ability to create [custom
writers in Lua](https://pandoc.org/custom-writers.html), allowing
ad hoc output formats to be supported by users. The fourth was
the introduction of [JSON filters](https://pandoc.org/filters.html)---user-created programs that
transform a JSON serialization of the pandoc AST, allowing the
document to be customized between the parsing phase and the
rendering phase. Citation processing was moved from the core of
pandoc into an external filter, [`pandoc-citeproc`](https://hackage.haskell.org/package/pandoc-citeproc).

This era saw the addition of reveal.js, EPUB v3, DokuWiki, and
FictionBook2 output; OPML input and output; and Haddock and
MediaWiki input. Notable contributors include David Lazar
(Haddock) and Sergey Astanin (FictionBook2).

The year 2014 saw the arrival of three new contributors who would
go on to make many contributions to the project. Albert Krewinkel
added support for Org-mode input; Jesse Rosenthal added a Word
docx reader (complete with track-changes awareness); and Matthew
Pickering (at the time a student at Oxford whom I "advised" as a
Google Summer of Code Student) added support for EPUB and
Txt2Tags as input formats. Supporting EPUB input required being
able to convert MathML equations, so Pickering also worked on
`texmath`. We were in very different time zones, and I remember
waking up every morning to find all the work Pickering had done
during the night. (Pickering has gone on to become one of the
core maintainers of the `ghc` compiler.) All of these
contributions were released in pandoc 1.13, together with Clare
Macrae's DokuWiki writer.

Since 2012, I had been involved in a working group that aimed to
produce an unambiguous specification of Markdown's syntax,
initiated by Jeff Atwood and including representatives from
GitHub, Reddit, and Stack Overflow. The group held intensive
discussions in 2012, which petered out in 2013. I still believed
in the project and didn't want to let the work we'd done go to
waste, so I sat down in August 2014, before the academic year began,
and wrote up a spec for
Markdown, as well as parsers in JavaScript and C. I sent the
draft spec to John Gruber for comment and did not get a response,
so a few weeks later we posted the spec. At this point, Gruber
strongly objected and demanded that we not call the project
"Standard Markdown," so we changed the name to "commonmark." The
project has been a success, in that with a few exceptions, most
Markdown processors implement the [commonmark spec](https://spec.commonmark.org) for their core
rules. (Commonmark does not concern itself with extensions.)

Pandoc 1.14 (2015) added support for commonmark and a number of
extensions (at first via bindings to the C library [`libcmark`](https://github.com/commonmark/cmark),
but later, in 2020, via my Haskell packages [`commonmark`,
`commonmark-extensions`, and `commonmark-pandoc`](https://github.com/jgm/commonmark-hs)). I intend
eventually to replace pandoc's legacy Markdown parser with a
commonmark core, but there are still a few key extensions that
have not been implemented, so pandoc users must still choose
between parsing their documents as `markdown` (Markdown with
pandoc's extensions) or as `gfm` or `commonmark` or
`commonmark_x` (commonmark with a number of extensions).
Ironically, although I was the author of the commonmark spec,
pandoc still uses a pre-commonmark Markdown parser!

The next year brought some important changes in the pandoc AST,
with the addition of image and link attributes, a SoftBreak
element (enabling pandoc to preserve line breaks from the
original source, or wrap, depending on a command line setting),
and a LineBlock element. MarLinn added an ODT reader,
Chris Forster added a TEI writer, and Ivo Clarysse added support
for DocBook 5.


## Pandoc 2 (2017--23)

Pandoc 2.0 (released in 2017) brought some big architectural
changes, worked out in collaboration with Jesse Rosenthal.
In the past, most of pandoc's readers (parsers) and writers
(renderers) had been
["pure"](https://en.wikipedia.org/wiki/Pure_function) (that is,
they had Haskell types that prevented them from having any side
effects, including I/O operations). But some formats needed to be
able to do I/O for a fully faithful conversion. (For example, reStructuredText has a syntax for including files, so the parser
needs to be able to read files; in some other formats, images
require explicit sizes, so a renderer has to be able to read
image files, perhaps fetching them using HTTP, and determine
their sizes.) We designed a system that allowed pandoc readers
and writers to run in any instance of the [PandocMonad](https://hackage-content.haskell.org/package/pandoc-3.10.1/docs/Text-Pandoc-Class.html#t:PandocMonad)
typeclass, and we provided both a pure instance (which could be
used for controlled testing, and in situations where we wanted to
forbid I/O) and an instance that allowed I/O operations. The
system also provided a way to handle images included as resources
in formats like docx or EPUB.

The other big change was the introduction of [Lua
filters](https://pandoc.org/lua-filters.html): filters running in
an embedded Lua interpreter and operating directly on the pandoc
AST, requiring no software other than pandoc itself and offering
far better performance than JSON filters. This was made possible
by the massive efforts of Albert Krewinkel, building on the
[`hslua`](https://hackage.haskell.org/package/hslua), a
Haskell-Lua bridge library.

In addition, pandoc 2.0 introduced
the raw attribute syntax in pandoc's Markdown, and support
for [GitHub-flavored Markdown](https://github.github.com/gfm/),
Emacs Muse (Alexander Krotov), TikiWiki, Vimwiki (Yuchen
Pei), Creole (Sascha Wilde), groff ms, and JATS. The
old `highlighting-kate` was replaced by the new [`skylighting`](https://github.com/jgm/skylighting),
which offered better performance and more accurate
interpretation of KDE syntax definitions.
A PowerPoint writer (due to Jesse Rosenthal) soon followed, as
well as support for FictionBook2 (Krotov) and man (Yan Pashkovsky
and me) as input formats.

In 2018, the project received a generous $100,000 donation from
[Handshake](https://handshake.org/grant-sponsors/), which we used
over the next five years to give small stipends to the most
active maintainers.

In 2019, support for `ipynb` (Jupyter notebooks) was added, allowing
pandoc to be used in data science workflows, and Jira wiki markup
was supported as an output format. With pandoc 2.8, it became
possible to specify collections of default options using [defaults
files](https://pandoc.org/MANUAL.html#defaults-files).

Users had long complained that pandoc's model of a table was too
restrictive, not even supporting row and colspans. After extensive
discussion of what was needed in a table format, Christian Despres
designed the new types for tables and modified
all of the readers and writers to use it (a big job).

At this point pandoc had supported citation resolution for many years,
by means of the `pandoc-citeproc` filter that used Andrea Rossato's
`citeproc-hs`. This was slow and somewhat buggy, and Rossato had long
since disappeared from the scene, so I wrote a [Haskell citeproc
library](https://hackage.haskell.org/package/citeproc) from
scratch, using just the CSL spec and test cases. Pandoc 2.11
depended on this library and offered far better citation support:
faster, more faithful to CSL, and with no need for an external
filter. In order to get citations to sort properly, I had to
write a another library
([`unicode-collation`](https://hackage.haskell.org/package/unicode-collation))
implementing the Unicode Collation algorithm in pure Haskell.

During this era Pandoc came to support conversions between bibliography
database formats: BibTeX, BibLaTeX, and CSL JSON, EndNote XML and RIS;
conversion from CSV and TSV to pandoc table formats; conversion to Markua;
and conversion from RTF. With pandoc 2.15 a [`--sandbox` option](https://pandoc.org/MANUAL.html#option--sandbox) was
added, which guarantees that pandoc's parsers and renderers have no
I/O side effects. (This was possible because of the PandocMonad
abstraction we added back in pandoc 2.0.) With pandoc 2.16.2 it became
possible to write custom readers in Lua to complement the custom
Lua writers that had been added in 2013. And with pandoc 2.19.1
it became possible to [run pandoc as a web server](https://pandoc.org/pandoc-server.html) exporting an API.

## Pandoc 3 (2023--present)

By 2023, pandoc had become a very big, monolithic project. Some users
wanted a leaner program, one that didn't include a full web server and
Lua interpreter. So with the pandoc 3.0 release, we split pandoc
into four parts: `pandoc` remained the Haskell library,
`pandoc-lua-engine` brought the Lua integration, and
`pandoc-server` exposed the library over HTTP as an API. The
command-line program, now in the `pandoc-cli` package, could
optionally be compiled without server or Lua support.
We also introduced a native Figure element in the AST and
a "chunked HTML" writer for multi-chapter HTML books and
documentation.

The first versions of [Typst](https://typst.app),
a modern LaTeX competitor with incremental compilation, were
released in 2023. I wanted to help the project by providing an
easy on- and off-ramp, making it easy for others to try Typst.
It turned out that creating a Typst reader for pandoc required
implementing an interpreter for a fairly full-featured
programming language. The result was the
[`typst`](https://hackage.haskell.org/package/typst) package on
Hackage. Typst support was added in pandoc 3.1.3.

In 2018 I had published an essay ["Beyond
Markdown"](https://talk.commonmark.org/t/beyond-markdown/2787) in
which I described the six features of Markdown that I thought had
created the most difficulties, both for writing a spec and for
implementations, and I explained how I thought these flaws could
be fixed in a future Markdown-like light markup syntax.
In 2022, I published a syntax description for such a syntax,
[djot](https://djot.net), together with code in Lua, JavaScript
and (later) [Haskell](https://github.com/jgm/djoths). Pandoc
3.1.12, published in 2024, added `djot` as both an input and output
format.

Subsequent releases in 2024 and 2025 saw the addition of
an ANSI writer for formatted terminal output and a reader for
the mdoc and POD formats (all due to Evan Silberman),
a reader and writer for an XML representation of the pandoc
AST (massifrg), a vimdoc writer (reptee), a PowerPoint reader
(Anton Antich), an Excel spreadsheet reader (Anton Antich),
and a BBCode writer (reptee), and an AsciiDoc
reader (supported by my
[`asciidoc`](https://hackage.haskell.org/package/asciidoc)
package).

Pandoc 3.9, released in February 2026, included support for
compiling pandoc to [WASM](https://webassembly.org), which
allowed a full-featured version of pandoc to run in the browser.
Most of the key work was done by TerrorJack. The GUI interface
["pandoc for the people"](https://pandoc.org/app) was designed with
the help of Claude Opus.

I still work on pandoc almost every day. Most of this work
doesn't involve the kind of new features or architectural changes
I have focused on in this narrative. Mostly it consists in fixing
small bugs, making tiny improvements, reviewing issues and pull
requests, repairing infrastructure (continuous integration,
building releases, code signing, website), improving
documentation, and engaging in discussions with maintainers and
users.

## Statistics

Pandoc currently supports 51 input formats and 76 output formats,
thus 3876 distinct conversions
(not counting the variants that are possible by adjusting
extensions).

![Pandoc's conversions](./diagram.svgz){width=100% style="border:1px solid grey;padding:1em"}

The four core packages (`pandoc`, `pandoc-lua-engine`, `pandoc-server`,
`pandoc-cli`) consist of 85,684 lines of Haskell code, not including
tests. If one includes dependencies that exist mainly for the sake of
pandoc (`texmath`, `typst`, `djot`, `commonmark`, `asciidoc`,
`citeproc`, and the pandoc/Lua interface packages), this number
approximately doubles.

On GitHub, 7346 issues have been resolved.

Over 600 people have contributed to pandoc over the years.
The top twenty contributors (measured by numbers of source lines
changed) are:

| Contributor | Lines changed | Years active |
|---|---|---|
| John MacFarlane | 372,317 | 2006– |
| Albert Krewinkel | 77,136 | 2014– |
| Jesse Rosenthal | 39,664 | 2014– |
| Christian Despres | 15,314 | 2019–2021 |
| Alexander Krotov | 8,657 | 2017–2019 |
| Matthew Pickering | 6,919 | 2014–2015 |
| MarLinn | 4,142 | 2015 |
| Evan Silberman | 3,478 | 2024– |
| Nikolay Yakimov | 3,362 | 2014–2020 |
| Mauro Bieg | 3,044 | 2012–2020 |
| Emily Bourke | 2,196 | 2021 |
| Yan Pas | 2,035 | 2018 |
| reptee | 1,732 | 2025 |
| Anton Antich | 1,552 | 2025 |
| massifrg | 1,171 | 2025– |
| Nathan Gass | 1,011 | 2010–2011 |
| Tuong Nguyen Manh | 801 | 2022– |
| Joseph C. Sible | 767 | 2020–2024 |
| Clare Macrae | 759 | 2013–2015 |
| Sergey Astanin | 718 | 2011–2012 |

Here are the twenty contributors who have contributed over the
longest spans of time:

| Contributor | Years active |
|---|---|---|
| John MacFarlane | 2006--2026 |
| Albert Krewinkel | 2014--2026 |
| Andrew Dunning | 2015--2026 |
| Nikolay Yakimov | 2014--2025 |
| Thomas Hodgson | 2015--2026 |
| Mauro Bieg | 2012--2022 |
| Kolen Cheung | 2016--2025 |
| Pablo Rodríguez | 2014--2023 |
| Pascal Wagler | 2019--2026 |
| Felix Yan | 2016--2023 |
| Sergei Trofimovich | 2011--2018 |
| Tristano Ajmone | 2017--2024 |
| Frerich Raabe | 2015--2022 |
| Salim B | 2017--2024 |
| Yihui Xie | 2014--2020 |
| Sascha Wilde | 2017--2023 |
| Jose Luis Duran | 2013--2019 |
| Jesse Rosenthal | 2014--2020 |
| John Muccigrosso | 2016--2022 |
| Jan Tojnar | 2020--2026 |
| Brian Leung | 2018--2023 |

## Retrospective: the choice of Haskell

As I noted at the beginning, I didn't choose Haskell because I
judged it to be the best language to use for a project like
pandoc. But was it?

It's hard to answer this confidently, because I'm not very
familiar with what would now be the most obvious alternative:
Rust. But I have created and maintained significant projects in a
number of languages, including Pascal, C, Ruby, and
JavaScript/TypeScript. I don't think I would have been able to
manage a project like this in my spare time if it had been
written in one of these languages.

Haskell has a number of features that have been very helpful in
developing pandoc:

- Its *algebraic data types* give us a very clean, ergonomic
  representation of a structured document

- Its *strong type system*, which gives you a compiler error
  if you don't combine the types of things in the right way,
  allows one to make big changes to the program with confidence
  that you're not breaking anything; the compiler will show you
  everything that needs to be changed, and when the code
  compiles, you are very often done. When working with languages
  without a strong type system, e.g. Python and
  JavaScript, the lack of these safeguards
  always make me afraid to make big changes, especially when
  I am maintaining code long after I've written it.

- Haskell is a *pure* language; nothing can have side effects
  that aren't explicitly allowed for in the types. If you
  have a pure function, you know it won't create a file or delete
  one or make a web request or launch missiles or change a global
  variable. This is extremely useful for preventing bugs. In
  pandoc we also use it to give us a really strong guarantee
  that, when run in sandbox mode, the readers and writers won't
  touch the file system.

- The choice of Haskell has also led to a high quality and low
  volume of contributors (a combination that is good for a
  project without a lot of resources).

From what I have seen, Rust appears to have many of the good
features of Haskell, while producing faster, more memory-efficient,
and more compact code. But Haskell still strikes me as more
"ergonomic," better suited to express abstractions, and just
closer to the ideal of a language that helps the developer
*think*.

## Whither Pandoc

I plan to continue improving pandoc. There are many ways in which
it can be improved. But sometimes I wonder how long such a tool
will continue to be necessary.

Just as current LLMs can do a very good job translating from one
human language to another, they can do a decent job translating
from one document format to another. In my small tests, ChatGPT
did a good job translating from Markdown to HTML, and a decent
(but notably worse) job converting to reStructuredText. My guess
is that you could write a document in a light markup language
you just had invented, and an LLM could do a decent job guessing your
intent and translating it to HTML or another format.

Perhaps, then, in the future, people will no longer have a need
for tools like pandoc. As things stand now, though, I think
that using pandoc to convert texts has several large advantages
over relying on an LLM. The first is ecological; it simply requires
far less energy for the same conversion. The second is that pandoc's
output is deterministic; if you convert your text with pandoc, you'll
always get the same result, and you'll be able to predict what that
result is. The third is that, for the moment at least, pandoc's
conversions are going to be more reliable. But that could change
in the coming years. Indeed, a time may come when LLMs can
produce more reliable conversions than pandoc or anything that
works like it.

In designing the commonmark
spec, we had the goal of interpreting complex strings in the way
that a human would naturally interpret them. This turns out to be
quite difficult to achieve: witness the complex [rules for
emphasis](https://spec.commonmark.org/0.31.2/#emphasis-and-strong-emphasis).
What we found is that, no matter how complex we made the rules for
nested emphasis, it was always possible to come up with cases
where the algorithm diverges from the meaning a human would
naturally find in the string. In such cases, I would often
remark, "until our programs have AI, we are going to have edge
cases like this; at some point we have to accept that and stop
trying to develop more complex rules." Interestingly, now we do
have tools that can understand (or at least simulate
understanding) of the meaning and intent of the text, and can
potentially do better at recognizing the formatting intended by
the author than any light markup syntax that could be designed.

Whatever the future may bring, I am proud of the 20-year history
of this project, which has saved people all over the world
countless hours of drudgery.
*Happy 20th birthday, pandoc!*

----

In honor of this occasion, I have produced some pandoc mugs and
stickers:

- [Coffee mug with conversion diagram](https://www.redbubble.com/i/mug/Pandoc-diagram-by-fiddlosopher/182488655/7yqg?asc=u)
- [Sticker with pandoc cartoon](https://www.redbubble.com/i/sticker/pandoc-cartoon-with-chalkduster-font-by-fiddlosopher/182504280/7sgk)
- [Sticker with pandoc logo](https://www.redbubble.com/i/sticker/Pandoc-logo-by-fiddlosopher/182488301/7sgk)
- [Mug with pandoc logo](https://www.redbubble.com/i/mug/Pandoc-logo-by-fiddlosopher/182488301/7yqg)

