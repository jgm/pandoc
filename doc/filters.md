% Pandoc filters
% John MacFarlane

# Summary

Pandoc provides an interface for users to write programs (known
as filters) which act on pandoc’s AST.

Pandoc consists of a set of readers and writers. When converting
a document from one format to another, text is parsed by a
reader into pandoc’s intermediate representation of the
document---an "abstract syntax tree" or AST---which is then
converted by the writer into the target format.
The pandoc AST format is defined in the module
[`Text.Pandoc.Definition` in the `pandoc-types` package
](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html).

A "filter" is a program that modifies the AST, between the
reader and the writer.

    INPUT --reader--> AST --filter--> AST --writer--> OUTPUT

Pandoc supports two kinds of filters:

- **Lua filters** use the Lua language to
  define transformations on the pandoc AST.  They are
  described in a [separate document](lua-filters.html).

- **JSON filters**, described here, are pipes that read from
  standard input and write to standard output, consuming and
  producing a JSON representation of the pandoc AST:

                             source format
                                  ↓
                               (pandoc)
                                  ↓
                          JSON-formatted AST
                                  ↓
                            (JSON filter)
                                  ↓
                          JSON-formatted AST
                                  ↓
                               (pandoc)
                                  ↓
                            target format

Lua filters have a couple of advantages.  They use a Lua
interpreter that is embedded in pandoc, so you don't need
to have any external software installed.  And they are
usually faster than JSON filters.  But if you wish to
write your filter in a language other than Lua, you may
prefer to use a JSON filter. JSON filters may be written
in any programming language.

You can use a JSON filter directly in a pipeline:

    pandoc -s input.txt -t json | \
     pandoc-citeproc | \
     pandoc -s -f json -o output.html

But it is more convenient to use the `--filter` option,
which handles the plumbing automatically:

    pandoc -s input.txt --filter pandoc-citeproc -o output.html

For a gentle introduction into writing your own filters,
continue this guide. There’s also a [list of third party filters
on the wiki](https://github.com/jgm/pandoc/wiki/Pandoc-Filters).


# A simple example

Suppose you wanted to replace all level 2+ headings in a markdown
document with regular paragraphs, with text in italics. How would you go
about doing this?

A first thought would be to use regular expressions. Something
like this:

    perl -pe 's/^##+ (.*)$/\*\1\*/' source.txt

This should work most of the time.  But don't forget
that ATX style headings can end with a sequence of `#`s
that is not part of the heading text:

    ## My heading ##

And what if your document contains a line starting with `##` in an HTML
comment or delimited code block?

    <!--
    ## This is just a comment
    -->

    ~~~~
    ### A third level heading in standard markdown
    ~~~~

We don't want to touch *these* lines.  Moreover, what about Setext
style second-level heading?

    A heading
    ---------

We need to handle those too.  Finally, can we be sure that adding
asterisks to each side of our string will put it in italics?
What if the string already contains asterisks around it? Then we'll
end up with bold text, which is not what we want. And what if it contains
a regular unescaped asterisk?

How would you modify your regular expression to handle these cases? It
would be hairy, to say the least.

A better approach is to let pandoc handle the parsing, and
then modify the AST before the document is written. For this,
we can use a filter.

To see what sort of AST is produced when pandoc parses our text,
we can use pandoc's `native` output format:

~~~~
% cat test.txt
## my heading

text with *italics*
% pandoc -s -t native test.txt
Pandoc (Meta {unMeta = fromList []})
[Header 2 ("my-heading",[],[]) [Str "My",Space,Str "heading"]
, Para [Str "text",Space,Str "with",Space,Emph [Str "italics"]] ]
~~~~

A `Pandoc` document consists of a `Meta` block (containing
metadata like title, authors, and date) and a list of `Block`
 elements.  In this case, we have two `Block`s, a `Header` and a `Para`.
Each has as its content a list of `Inline` elements.  For more details on
the pandoc AST, see the [haddock documentation for `Text.Pandoc.Definition`].

[haddock documentation for `Text.Pandoc.Definition`]: https://hackage.haskell.org/package/pandoc-types

We can use Haskell to create a JSON filter that transforms this
AST, replacing each `Header` block with level >= 2 with a `Para`
with its contents wrapped inside an `Emph` inline:

~~~~                          {.haskell}
#!/usr/bin/env runhaskell
-- behead.hs
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter behead

behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x
~~~~

The `toJSONFilter` function does two things.  First, it lifts
the `behead` function (which maps `Block -> Block`) onto a
transformation of the entire `Pandoc` AST, walking the AST
and transforming each block.  Second, it wraps this `Pandoc ->
Pandoc` transformation with the necessary JSON serialization
and deserialization, producing an executable that consumes
JSON from stdin and produces JSON to stdout.

To use the filter, make it executable:

    chmod +x behead.hs

and then

    pandoc -f SOURCEFORMAT -t TARGETFORMAT --filter ./behead.hs

(It is also necessary that `pandoc-types` be installed in the
local package repository. To do this using cabal-install,
`cabal v2-update && cabal v2-install --lib pandoc-types`.)

Alternatively, we could compile the filter:

    ghc -package-env=default --make behead.hs
    pandoc -f SOURCEFORMAT -t TARGETFORMAT --filter ./behead

Note that if the filter is placed in the system PATH, then the initial
`./` is not needed.  Note also that the command line can include
multiple instances of `--filter`:  the filters will be applied in
sequence.


# LaTeX for WordPress

Another easy example. WordPress blogs require a special format for
LaTeX math.  Instead of `$e=mc^2$`, you need: `$LaTeX e=mc^2$`.
How can we convert a markdown document accordingly?

Again, it's difficult to do the job reliably with regexes.
A `$` might be a regular currency indicator, or it might occur in
a comment or code block or inline code span.  We just want to find
the `$`s that begin LaTeX math. If only we had a parser...

We do.  Pandoc already extracts LaTeX math, so:

~~~~                          {.haskell}
#!/usr/bin/env runhaskell
-- wordpressify.hs
import Text.Pandoc.JSON

main = toJSONFilter wordpressify
  where wordpressify (Math x y) = Math x ("LaTeX " ++ y)
        wordpressify x = x
~~~~

Mission accomplished. (I've omitted type signatures here,
just to show it can be done.)


# But I don't want to learn Haskell!

While it's easiest to write pandoc filters in Haskell, it is fairly
easy to write them in python using the `pandocfilters` package.
The package is in PyPI and can be installed using `pip install
pandocfilters` or `easy_install pandocfilters`.

Here's our "beheading" filter in python:

~~~ {.python}
#!/usr/bin/env python

"""
Pandoc filter to convert all level 2+ headings to paragraphs with
emphasized text.
"""

from pandocfilters import toJSONFilter, Emph, Para

def behead(key, value, format, meta):
  if key == 'Header' and value[0] >= 2:
    return Para([Emph(value[2])])

if __name__ == "__main__":
  toJSONFilter(behead)
~~~

`toJSONFilter(behead)` walks the AST and applies the `behead` action
to each element.  If `behead` returns nothing, the node is unchanged;
if it returns an object, the node is replaced; if it returns a list,
the new list is spliced in.

Note that, although these parameters are not used in this example,
`format` provides access to the target format, and `meta` provides access to
the document's metadata.

There are many examples of python filters in [the pandocfilters
repository](https://github.com/jgm/pandocfilters).

For a more Pythonic alternative to pandocfilters, see
the [panflute](https://pypi.org/project/panflute) library.
Don't like Python? There are also ports of pandocfilters in

- [PHP](https://github.com/vinai/pandocfilters-php),
- [perl](https://metacpan.org/pod/Pandoc::Filter),
- TypeScript/JavaScript via Node.js
  - [pandoc-filter](https://github.com/mvhenderson/pandoc-filter-node),
  - [node-pandoc-filter](https://github.com/mu-io/node-pandoc-filter),
- [Groovy](https://github.com/dfrommi/groovy-pandoc), and
- [Ruby](https://heerdebeer.org/Software/markdown/paru/).

Starting with pandoc 2.0, pandoc includes built-in support for
writing filters in lua.  The lua interpreter is built in to
pandoc, so a lua filter does not require any additional software
to run.  See the [documentation on lua
filters](https://pandoc.org/lua-filters.html).

# Include files

So none of our transforms have involved IO. How about a script that
reads a markdown document, finds all the inline code blocks with
attribute `include`, and replaces their contents with the contents of
the file given?

~~~~                          {.haskell}
#!/usr/bin/env runhaskell
-- includes.hs
import Text.Pandoc.JSON
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> CodeBlock (id, classes, namevals) <$>
                      TIO.readFile (T.unpack f)
       Nothing    -> return cb
doInclude x = return x

main :: IO ()
main = toJSONFilter doInclude
~~~~

Try this on the following:

    Here's the pandoc README:

    ~~~~ {include="README"}
    this will be replaced by contents of README
    ~~~~

# Removing links

What if we want to remove every link from a document, retaining
the link's text?

~~~~                          {.haskell}
#!/usr/bin/env runhaskell
-- delink.hs
import Text.Pandoc.JSON

main = toJSONFilter delink

delink :: Inline -> [Inline]
delink (Link _ txt _) = txt
delink x              = [x]
~~~~

Note that `delink` can't be a function of type `Inline -> Inline`,
because the thing we want to replace the link with is not a single
`Inline` element, but a list of them. So we make `delink` a function
from an `Inline` element to a list of `Inline` elements.
`toJSONFilter` can still lift this function to a transformation of type
`Pandoc -> Pandoc`.

# A filter for ruby text

Finally, here's a nice real-world example, developed on the
[pandoc-discuss](https://groups.google.com/group/pandoc-discuss/browse_thread/thread/7baea325565878c8) list.  Qubyte wrote:

> I'm interested in using pandoc to turn my markdown notes on Japanese
> into nicely set HTML and (Xe)LaTeX. With HTML5, ruby (typically used to
> phonetically read chinese characters by placing text above or to the
> side) is standard, and support from browsers is emerging (Webkit based
> browsers appear to fully support it). For those browsers that don't
> support it yet (notably Firefox) the feature falls back in a nice way
> by placing the phonetic reading inside brackets to the side of each
> Chinese character, which is suitable for other output formats too. As
> for (Xe)LaTeX, ruby is not an issue.
>
> At the moment, I use inline HTML to achieve the result when the
> conversion is to HTML, but it's ugly and uses a lot of keystrokes, for
> example
>
> ~~~ {.xml}
> <ruby>ご<rt></rt>飯<rp>（</rp><rt>はん</rt><rp>）</rp></ruby>
> ~~~
>
> sets ご飯 "gohan" with "han" spelt phonetically above the second
> character, or to the right of it in brackets if the browser does not
> support ruby.  I'd like to have something more like
>
>     r[はん](飯)
>
> or any keystroke saving convention would be welcome.

We came up with the following script, which uses the convention that a
markdown link with a URL beginning with a hyphen is interpreted as ruby:

    [はん](-飯)

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
-- handleruby.hs
import Text.Pandoc.JSON
import System.Environment (getArgs)
import qualified Data.Text as T

handleRuby :: Maybe Format -> Inline -> Inline
handleRuby (Just format) x@(Link attr [Str ruby] (src,_)) =
  case T.uncons src of
    Just ('-',kanji)
      | format == Format "html" -> RawInline format $
        "<ruby>" <> kanji <> "<rp>(</rp><rt>" <> ruby <>
        "</rt><rp>)</rp></ruby>"
      | format == Format "latex" -> RawInline format $
        "\\ruby{" <> kanji <> "}{" <> ruby <> "}"
      | otherwise -> Str ruby
    _ -> x
handleRuby _ x = x

main :: IO ()
main = toJSONFilter handleRuby
~~~

Note that, when a script is called using `--filter`, pandoc passes
it the target format as the first argument.  When a function's
first argument is of type `Maybe Format`, `toJSONFilter` will
automatically assign it `Just` the target format or `Nothing`.

We compile our script:

    # first, make sure pandoc-types is installed:
    cabal install --lib pandoc-types --package-env .
    ghc --make handleRuby

Then run it:

    % pandoc -F ./handleRuby -t html
    [はん](-飯)
    ^D
    <p><ruby>飯<rp>(</rp><rt>はん</rt><rp>)</rp></ruby></p>
    % pandoc -F ./handleRuby -t latex
    [はん](-飯)
    ^D
    \ruby{飯}{はん}

Note:  to use this to generate PDFs via LaTeX, you'll need
to use `--pdf-engine=xelatex`, specify a `mainfont` that has
the Japanese characters (e.g. "Noto Sans CJK TC"), and add
`\usepackage{ruby}` to your template or header-includes.

# Exercises

1.  Put all the regular text in a markdown document in ALL CAPS
    (without touching text in URLs or link titles).

2.  Remove all horizontal rules from a document.

3.  Renumber all enumerated lists with roman numerals.

4.  Replace each delimited code block with class `dot` with an
    image generated by running `dot -Tpng` (from graphviz) on the
    contents of the code block.

5.  Find all code blocks with class `python` and run them
    using the python interpreter, printing the results to the console.

# Technical details of JSON filters

A JSON filter is any program which can consume and produce a
valid pandoc JSON document representation. This section describes
the technical details surrounding the invocation of filters.

## Arguments

The program will always be called with the target format as the
only argument. A pandoc invocation like

    pandoc --filter demo --to=html

will cause pandoc to call the program `demo` with argument `html`.

## Environment variables

Pandoc sets additional environment variables before calling a
filter.

`PANDOC_VERSION`
:   The version of the pandoc binary used to process the document.
    Example: `2.11.1`.

`PANDOC_READER_OPTIONS`
:   JSON object representation of the options passed to the input
    parser.

    Object fields:

    `readerAbbreviations`
    :   set of known abbreviations (array of strings).

    `readerColumns`
    :   number of columns in terminal; an integer.

    `readerDefaultImageExtension`
    :   default extension for images; a string.

    `readerExtensions`
    :   integer representation of the syntax extensions bit
        field.

    `readerIndentedCodeClasses`
    :   default classes for indented code blocks; array of
        strings.

    `readerStandalone`
    :   whether the input was a standalone document with header;
        either `true` or `false`.

    `readerStripComments`
    :   HTML comments are stripped instead of parsed as raw HTML;
        either `true` or `false`.

    `readerTabStop`
    :   width (i.e. equivalent number of spaces) of tab stops;
        integer.

    `readerTrackChanges`
    :   track changes setting for docx; one of
        `"accept-changes"`, `"reject-changes"`, and
        `"all-changes"`.

## Supported interpreters

Files passed to the `--filter`/`-F` parameter are expected to be
executable. However, if the executable bit is not set, then
pandoc tries to guess a suitable interpreter from the file
extension.

  file extension   interpreter
  ---------------- --------------
  .py              `python`
  .hs              `runhaskell`
  .pl              `perl`
  .rb              `ruby`
  .php             `php`
  .js              `node`
  .r               `Rscript`
