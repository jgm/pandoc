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
`Text.Pandoc.Definition` in
[pandoc-types](https://hackage.haskell.org/package/pandoc-types).

A "filter" is a program that modifies the AST, between the
reader and the writer:

    INPUT --reader--> AST --filter--> AST --writer--> OUTPUT

Filters are "pipes" that read from standard input and write to
standard output.  They consume and produce a JSON representation
of the pandoc AST.  (In recent versions, this representation
includes a `pandoc-api-version` field which refers to a
version of `pandoc-types`.)  Filters may be written in any programming
language.  To use a filter, you need only specify it on the
command line using `--filter`, e.g.

    pandoc -s input.txt --filter pandoc-citeproc -o output.htl

For a gentle introduction into writing your own filters,
continue this guide. There’s also a [list of third party filters
on the wiki](https://github.com/jgm/pandoc/wiki/Pandoc-Filters).


# A simple example

Suppose you wanted to replace all level 2+ headers in a markdown
document with regular paragraphs, with text in italics. How would you go
about doing this?

A first thought would be to use regular expressions. Something
like this:

    perl -pe 's/^##+ (.*)$/\*\1\*/' source.txt

This should work most of the time.  But don't forget
that ATX style headers can end with a sequence of `#`s
that is not part of the header text:

    ## My header ##

And what if your document contains a line starting with `##` in an HTML
comment or delimited code block?

    <!--
    ## This is just a comment
    -->

    ~~~~
    ### A third level header in standard markdown
    ~~~~

We don't want to touch *these* lines.  Moreover, what about setext
style second-level headers?

    A header
    --------

We need to handle those too.  Finally, can we be sure that adding
asterisks to each side of our string will put it in italics?
What if the string already contains asterisks around it? Then we'll
end up with bold text, which is not what we want. And what if it contains
a regular unescaped asterisk?

How would you modify your regular expression to handle these cases? It
would be hairy, to say the least. What we need is a real parser.

Well, pandoc has a real markdown parser, the library function
`readMarkdown`. This transforms markdown text to an abstract syntax tree
(AST) that represents the document structure. Why not manipulate the
AST directly in a short Haskell script, then convert the result back to
markdown using `writeMarkdown`?

First, let's see what this AST looks like. We can use pandoc's `native`
output format:

~~~~
% cat test.txt
## my header

text with *italics*
% pandoc -s -t native test.txt
Pandoc (Meta {unMeta = fromList []})
[Header 3 ("my-header",[],[]) [Str "My",Space,Str "header"]
, Para [Str "text",Space,Str "with",Space,Emph [Str "italics"]] ]
~~~~

A `Pandoc` document consists of a `Meta` block (containing
metadata like title, authors, and date) and a list of `Block`
 elements.  In this case, we have two `Block`s, a `Header` and a `Para`.
Each has as its content a list of `Inline` elements.  For more details on
the pandoc AST, see the [haddock documentation for `Text.Pandoc.Definition`].

[haddock documentation for `Text.Pandoc.Definition`]: http://hackage.haskell.org/package/pandoc-types

Here's a short Haskell script that reads markdown, changes level
2+ headers to regular paragraphs, and writes the result as markdown.
If you save it as `behead.hs`, you can run it using `runhaskell behead.hs`.
It will act like a unix pipe, reading from `stdin` and writing to `stdout`.
Or, if you want, you can compile it, using `ghc --make behead`, then run
the resulting executable `behead`.

~~~~                          {.haskell}
-- behead.hs
import Text.Pandoc
import Text.Pandoc.Walk (walk)

behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x

readDoc :: String -> Pandoc
readDoc s = readMarkdown def s
-- or, for pandoc 1.14 and greater, use:
-- readDoc s = case readMarkdown def s of
--                  Right doc -> doc
--                  Left err  -> error (show err)

writeDoc :: Pandoc -> String
writeDoc doc = writeMarkdown def doc

main :: IO ()
main = interact (writeDoc . walk behead . readDoc)
~~~~

The magic here is the `walk` function, which converts
our `behead` function (a function from `Block` to `Block`) to
a transformation on whole `Pandoc` documents.
(See the [haddock documentation for `Text.Pandoc.Walk`].)

[haddock documentation for `Text.Pandoc.Walk`]: http://hackage.haskell.org/package/pandoc-types

# Queries: listing URLs

We can use this same technique to do much more complex transformations
and queries.  Here's how we could extract all the URLs linked to in
a markdown document (again, not an easy task with regular expressions):

~~~~                          {.haskell}
-- extracturls.hs
import Text.Pandoc

extractURL :: Inline -> [String]
extractURL (Link _ _ (u,_)) = [u]
extractURL (Image _ _ (u,_)) = [u]
extractURL _ = []

extractURLs :: Pandoc -> [String]
extractURLs = query extractURL

readDoc :: String -> Pandoc
readDoc = readMarkdown def
-- or, for pandoc 1.14, use:
-- readDoc s = case readMarkdown def s of
--                Right doc -> doc
--                Left err  -> error (show err)

main :: IO ()
main = interact (unlines . extractURLs . readDoc)
~~~~

`query` is the query counterpart of `walk`: it lifts
a function that operates on `Inline` elements to one that operates
on the whole `Pandoc` AST.  The results returned by applying
`extractURL` to each `Inline` element are concatenated in the
result.

# JSON filters

`behead.hs` is a very special-purpose program.  It reads a
specific input format (markdown) and writes a specific output format
(HTML), with a specific set of options (here, the defaults).
But the basic operation it performs is one that would be useful
in many document transformations.  It would be nice to isolate the
part of the program that transforms the pandoc AST, leaving the rest
to pandoc itself.  What we want is a *filter* that *just* operates
on the AST---or rather, on a JSON representation of the AST that
pandoc can produce and consume:

                             source format
                                  ↓
                               (pandoc)
                                  ↓
                          JSON-formatted AST
                                  ↓
                               (filter)
                                  ↓
                          JSON-formatted AST
                                  ↓
                               (pandoc)
                                  ↓
                            target format

The module `Text.Pandoc.JSON` (from `pandoc-types`) contains a
function `toJSONFilter` that makes it easy to write such
filters.  Here is a filter version of `behead.hs`:

~~~~                          {.haskell}
#!/usr/bin/env runhaskell
-- behead2.hs
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter behead
  where behead (Header n _ xs) | n >= 2 = Para [Emph xs]
        behead x = x
~~~~

It can be used this way:

    pandoc -f SOURCEFORMAT -t json | runhaskell behead2.hs | \
      pandoc -f json -t TARGETFORMAT

But it is easier to use the `--filter` option with pandoc:

    pandoc -f SOURCEFORMAT -t TARGETFORMAT --filter ./behead2.hs

Note that this approach requires that `behead2.hs` be executable,
so we must

    chmod +x behead2.hs

Alternatively, we could compile the filter:

    ghc --make behead2.hs
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
Pandoc filter to convert all level 2+ headers to paragraphs with
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
repository](http://github.com/jgm/pandocfilters).

For a more Pythonic alternative to pandocfilters, see
the [panflute](http://scorreia.com/software/panflute/) library.
Don't like Python?   There are also ports of pandocfilters in
[PHP](https://github.com/vinai/pandocfilters-php),
[perl](https://metacpan.org/pod/Pandoc::Filter),
[javascript/node.js](https://github.com/mvhenderson/pandoc-filter-node),
[Groovy](https://github.com/dfrommi/groovy-pandoc), and
[Ruby](https://heerdebeer.org/Software/markdown/paru/).

Starting with pandoc 2.0, pandoc includes built-in support for
writing filters in lua.  The lua interpreter is built in to
pandoc, so a lua filter does not require any additional software
to run.  See the [documentation on lua
filters](lua-filters.html).

# Include files

So none of our transforms have involved IO. How about a script that
reads a markdown document, finds all the inline code blocks with
attribute `include`, and replaces their contents with the contents of
the file given?

~~~~                          {.haskell}
#!/usr/bin/env runhaskell
-- includes.hs
import Text.Pandoc.JSON

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> return . (CodeBlock (id, classes, namevals)) =<< readFile f
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
[pandoc-discuss](http://groups.google.com/group/pandoc-discuss/browse_thread/thread/7baea325565878c8) list.  Qubyte wrote:

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
-- handleruby.hs
import Text.Pandoc.JSON
import System.Environment (getArgs)

handleRuby :: Maybe Format -> Inline -> Inline
handleRuby (Just format) (Link _ [Str ruby] ('-':kanji,_))
  | format == Format "html"  = RawInline format
    $ "<ruby>" ++ kanji ++ "<rp>(</rp><rt>" ++ ruby ++ "</rt><rp>)</rp></ruby>"
  | format == Format "latex" = RawInline format
    $ "\\ruby{" ++ kanji ++ "}{" ++ ruby ++ "}"
  | otherwise = Str ruby
handleRuby _ x = x

main :: IO ()
main = toJSONFilter handleRuby
~~~

Note that, when a script is called using `--filter`, pandoc passes
it the target format as the first argument.  When a function's
first argument is of type `Maybe Format`, `toJSONFilter` will
automatically assign it `Just` the target format or `Nothing`.

We compile our script:

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

