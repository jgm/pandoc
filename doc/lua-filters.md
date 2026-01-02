---
author:
- Albert Krewinkel
- John MacFarlane
date: 'January 10, 2020'
title: Pandoc Lua Filters
---

# Introduction

Pandoc has long supported filters, which allow the pandoc
abstract syntax tree (AST) to be manipulated between the parsing
and the writing phase. [Traditional pandoc
filters](https://pandoc.org/filters.html) accept a JSON
representation of the pandoc AST and produce an altered JSON
representation of the AST. They may be written in any
programming language, and invoked from pandoc using the
`--filter` option.

Although traditional filters are very flexible, they have a
couple of disadvantages. First, there is some overhead in
writing JSON to stdout and reading it from stdin (twice, once on
each side of the filter). Second, whether a filter will work
will depend on details of the user's environment. A filter may
require an interpreter for a certain programming language to be
available, as well as a library for manipulating the pandoc AST
in JSON form. One cannot simply provide a filter that can be
used by anyone who has a certain version of the pandoc
executable.

Starting with version 2.0, pandoc makes it possible to write
filters in Lua without any external dependencies at all. A Lua
interpreter (version 5.4) and a Lua library for creating pandoc
filters is built into the pandoc executable. Pandoc data types
are marshaled to Lua directly, avoiding the overhead of writing
JSON to stdout and reading it from stdin.

Here is an example of a Lua filter that converts strong emphasis
to small caps:

``` lua
return {
  Strong = function (elem)
    return pandoc.SmallCaps(elem.content)
  end,
}
```

or equivalently,

``` lua
function Strong(elem)
  return pandoc.SmallCaps(elem.content)
end
```

This says: walk the AST, and when you find a Strong element,
replace it with a SmallCaps element with the same content.

To run it, save it in a file, say `smallcaps.lua`, and invoke
pandoc with `--lua-filter=smallcaps.lua`.

Here's a quick performance comparison, converting the pandoc
manual (MANUAL.txt) to HTML, with versions of the same JSON
filter written in compiled Haskell (`smallcaps`) and interpreted
Python (`smallcaps.py`):

  Command                                 Time
  --------------------------------------- -------
  `pandoc`                                1.01s
  `pandoc --filter ./smallcaps`           1.36s
  `pandoc --filter ./smallcaps.py`        1.40s
  `pandoc --lua-filter ./smallcaps.lua`   1.03s

As you can see, the Lua filter avoids the substantial overhead
associated with marshaling to and from JSON over a pipe.

# Lua filter structure

Lua filters are tables with element names as keys and values
consisting of functions acting on those elements.

Filters are expected to be put into separate files and are
passed via the `--lua-filter` command-line argument. For
example, if a filter is defined in a file `current-date.lua`,
then it would be applied like this:

    pandoc --lua-filter=current-date.lua -f markdown MANUAL.txt

The `--lua-filter` option may be supplied multiple times. Pandoc
applies all filters (including JSON filters specified via
`--filter` and Lua filters specified via `--lua-filter`) in the
order they appear on the command line.

Pandoc expects each Lua file to return a filter. If there is
no value returned by the filter script, then pandoc will try to
generate a single filter by collecting all top-level functions
whose names correspond to those of pandoc elements (e.g., `Str`,
`Para`, `Meta`, or `Pandoc`). (That is why the two examples above
are equivalent.)

It is currently also possible to return a list of filters
from a Lua file which are called sequentially. Before the
[walk](#type-pandoc:walk) method was made available, this was
the only way to run multiple filters from one Lua file. However,
returning a list of filters is now discouraged in favor of using
the [walk](#type-pandoc:walk) method, and this functionality may
be removed at some point.

For each filter, the document is traversed and each element
subjected to the filter. Elements for which the filter contains
an entry (i.e. a function of the same name) are passed to Lua
element filtering function. In other words, filter entries will
be called for each corresponding element in the document,
getting the respective element as input.

The return value of a filter function must be one of the
following:

-   nil: this means that the object should remain unchanged.
-   a pandoc object: this must be of the same type as the input
    and will replace the original object.
-   a list of pandoc objects: these will replace the original
    object; the list is merged with the neighbors of the
    original objects (spliced into the list the original object
    belongs to); returning an empty list deletes the object.

The function's output must result in an element of the same type
as the input. This means a filter function acting on an inline
element must return either nil, an inline, or a list of inlines,
and a function filtering a block element must return one of nil,
a block, or a list of block elements. Pandoc will throw an error
if this condition is violated.

If there is no function matching the element's node type, then
the filtering system will look for a more general fallback
function. Two fallback functions are supported, `Inline` and
`Block`. Each matches elements of the respective type.

Elements without matching functions are left untouched.

See [module documentation](#module-pandoc) for a list of pandoc
elements.

## Filters on element sequences

For some filtering tasks, it is necessary to know the order
in which elements occur in the document. It is not enough then to
inspect a single element at a time.

There are two special function names, which can be used to define
filters on lists of blocks or lists of inlines.

[`Inlines (inlines)`]{#inlines-filter}
:   If present in a filter, this function will be called on all
    lists of inline elements, like the content of a [Para]
    (paragraph) block, or the description of an [Image]. The
    `inlines` argument passed to the function will be a [List] of
    [Inline] elements for each call.

[`Blocks (blocks)`]{#blocks-filter}
:   If present in a filter, this function will be called on all
    lists of block elements, like the content of a [MetaBlocks]
    meta element block, on each item of a list, and the main
    content of the [Pandoc] document. The `blocks` argument
    passed to the function will be a [List] of [Block] elements
    for each call.

These filter functions are special in that the result must either
be nil, in which case the list is left unchanged, or must be a
list of the correct type, i.e., the same type as the input
argument. Single elements are **not** allowed as return values,
as a single element in this context usually hints at a bug.

See ["Remove spaces before normal citations"][Inlines filter
example] for an example.

This functionality has been added in pandoc 2.9.2.

[Inlines filter example]: #remove-spaces-before-citations

## Traversal order

The traversal order of filters can be selected by setting the key
`traverse` to either `'topdown'` or `'typewise'`; the default is
`'typewise'`.

Example:

``` lua
local filter = {
  traverse = 'topdown',
  -- ... filter functions ...
}
return filter
```

Support for this was added in pandoc 2.17; previous versions
ignore the `traverse` setting.

### Typewise traversal

Element filter functions within a filter set are called in a
fixed order, skipping any which are not present:

  1. functions for [*Inline* elements](#type-inline),
  2. the [`Inlines`](#inlines-filter) filter function,
  2. functions for [*Block* elements](#type-block) ,
  2. the [`Blocks`](#inlines-filter) filter function,
  3. the [`Meta`](#type-meta) filter function, and last
  4. the [`Pandoc`](#type-pandoc) filter function.

It is still possible to force a different order by manually
running the filters using the [walk](#type-pandoc:walk) method.
For example, if the filter for *Meta* is to be run before that
for *Str*, one can write

``` lua
function Pandoc(doc)
  doc = doc:walk { Meta = Meta } -- (1)
  return doc:walk { Str = Str }  -- (2)
end
```

### Topdown traversal

It is sometimes more natural to traverse the document tree
depth-first from the root towards the leaves, and all in a single
run.

For example, a block list `[Plain [Str "a"], Para [Str
"b"]]`{.haskell} will try the following filter functions, in
order: `Blocks`, `Plain`, `Inlines`, `Str`, `Para`, `Inlines`,
`Str`.

Topdown traversals can be cut short by returning `false` as a
second value from the filter function. No child-element of
the returned element is processed in that case.

For example, to exclude the contents of a footnote from being
processed, one might write

``` lua
traverse = 'topdown'
function Note (n)
  return n, false
end
```

## Global variables

Pandoc passes additional data to Lua filters by setting global
variables.

`FORMAT`
:   The global `FORMAT` is set to the format of the pandoc
    writer being used (`html5`, `latex`, etc.), so the behavior
    of a filter can be made conditional on the eventual output
    format.

`PANDOC_READER_OPTIONS`
:   Table of the options which were provided to the parser.
    ([ReaderOptions](#type-readeroptions))

`PANDOC_WRITER_OPTIONS`
:   Table of the options that will be passed to the writer.
    While the object can be modified, the changes will **not**
    be picked up by pandoc.
    ([WriterOptions](#type-writeroptions))

    Accessing this variable in **custom writers** is
    **deprecated**. Starting with pandoc 3.0, it is set to a
    placeholder value (the default options) in custom writers.
    Access to the actual writer options is provided via the
    `Writer` or `ByteStringWriter` function, to which the options
    are passed as the second function argument.

    *Since: pandoc 2.17*

`PANDOC_VERSION`
:   Contains the pandoc version as a [Version] object which
    behaves like a numerically indexed table, most significant
    number first. E.g., for pandoc 2.7.3, the value of the
    variable is equivalent to a table `{2, 7, 3}`. Use
    `tostring(PANDOC_VERSION)` to produce a version string. This
    variable is also set in custom writers.

`PANDOC_API_VERSION`
:   Contains the version of the pandoc-types API against which
    pandoc was compiled. It is given as a numerically indexed
    table, most significant number first. E.g., if pandoc was
    compiled against pandoc-types 1.17.3, then the value of the
    variable will behave like the table `{1, 17, 3}`. Use
    `tostring(PANDOC_API_VERSION)` to produce a version string.
    This variable is also set in custom writers.

`PANDOC_SCRIPT_FILE`
:   The name used to involve the filter. This value can be used
    to find files relative to the script file. This variable is
    also set in custom writers.

`PANDOC_STATE`
:   The state shared by all readers and writers. It is used by
    pandoc to collect and pass information. The value of this
    variable is of type [CommonState] and
    is read-only.

`pandoc`
:   The *pandoc* module, described in the next section, is
    available through the global `pandoc`. The other modules
    described herein are loaded as subfields under their
    respective name.

`lpeg`
:   This variable holds the `lpeg` module, a package based on
    Parsing Expression Grammars (PEG).  It provides excellent
    parsing utilities and is documented on the official [LPeg
    homepage].  Pandoc uses a built-in version of the library,
    unless it has been configured by the package maintainer to
    rely on a system-wide installation.

`re`
:   Contains the LPeg.re module, which is built on top of LPeg
    and offers an implementation of a [regex engine].  Pandoc
    uses a built-in version of the library, unless it has been
    configured by the package maintainer to rely on a system-wide
    installation.

[LPeg homepage]: http://www.inf.puc-rio.br/~roberto/lpeg/
[regex engine]: http://www.inf.puc-rio.br/~roberto/lpeg/re.html

# Pandoc Module

The `pandoc` Lua module is loaded into the filter's Lua
environment and provides a set of functions and constants to
make creation and manipulation of elements easier. The global
variable `pandoc` is bound to the module and should generally
not be overwritten for this reason.

Two major functionalities are provided by the module: element
creator functions and access to some of pandoc's main
functionalities.

## Element creation

Element creator functions like `Str`, `Para`, and `Pandoc` are
designed to allow easy creation of new elements that are simple
to use and can be read back from the Lua environment.
Internally, pandoc uses these functions to create the Lua
objects which are passed to element filter functions. This means
that elements created via this module will behave exactly as
those elements accessible through the filter function parameter.

## Exposed pandoc functionality

Some pandoc functions have been made available in Lua:

-   [`walk_block`](#pandoc.walk_block) and
    [`walk_inline`](#pandoc.walk_inline) allow filters to be applied
    inside specific block or inline elements;
-   [`read`](#pandoc.read) allows filters to parse strings into pandoc
    documents;
-   [`pipe`](#pandoc.pipe) runs an external command with input from and
    output to strings;
-   the [`pandoc.mediabag`](#module-pandoc.mediabag) module
    allows access to the "mediabag," which stores binary content
    such as images that may be included in the final document;
-   the [`pandoc.utils`](#module-pandoc.utils) module contains
    various utility functions.

# Lua interpreter initialization

Initialization of pandoc's Lua interpreter can be controlled by
placing a file `init.lua` in pandoc's data directory. A common
use-case would be to load additional modules, or even to alter
default modules.

The following snippet is an example of code that might be useful
when added to `init.lua`. The snippet adds all Unicode-aware
functions defined in the [`text` module](#module-pandoc.text) to the
default `string` module, prefixed with the string `uc_`.

``` lua
for name, fn in pairs(require 'text') do
  string['uc_' .. name] = fn
end
```

This makes it possible to apply these functions on strings using
colon syntax (`mystring:uc_upper()`).

# Debugging Lua filters

Many errors can be avoided by performing static analysis.
[`luacheck`](https://github.com/lunarmodules/luacheck) may be used
for this purpose. A Luacheck configuration file for pandoc filters
is available at <https://github.com/rnwst/pandoc-luacheckrc>.

William Lupton has written a Lua module with some handy
functions for debugging Lua filters, including functions
that can pretty-print the Pandoc AST elements manipulated
by the filters: it is available at
<https://github.com/wlupton/pandoc-lua-logging>.

It is possible to use a debugging interface to halt execution and
step through a Lua filter line by line as it is run inside Pandoc.
This is accomplished using the remote-debugging interface of the
package [`mobdebug`](https://github.com/pkulchenko/MobDebug).
Although mobdebug can be run from the terminal, it is more useful
run within the donation-ware Lua editor and IDE, [ZeroBrane
Studio](https://studio.zerobrane.com/). ZeroBrane offers a REPL
console and UI to step-through and view all variables and state.

ZeroBrane doesn't come with Lua 5.4 bundled, but it can debug it, so
you should install Lua 5.4, and then add
[`mobdebug`](https://luarocks.org/modules/paulclinger/mobdebug) and
its dependency
[`luasocket`](https://luarocks.org/modules/luasocket/luasocket) using
[`luarocks`](https://luarocks.org). ZeroBrane can use your Lua 5.4
install by adding `path.lua = "/path/to/your/lua"` in your ZeroBrane
settings file. Next, open your Lua filter in ZeroBrane, and add
`require('mobdebug').start()` at the line where you want your
breakpoint. Then make sure the Project > Lua Interpreter is set to the
"Lua" you added in settings and enable "Start Debugger Server" [see
detailed instructions
here](https://studio.zerobrane.com/doc-remote-debugging). Run Pandoc
as you normally would, and ZeroBrane should break at the correct line.

## Common pitfalls

AST elements not updated
:   A filtered element will only be updated if the filter
    function returns a new element to replace it. A function like
    the below has no effect, as the function returns no value:

    ``` lua
    function Str (str)
      str.text = string.upper(str.text)
    end
    ```

    The correct version would be

    ``` lua
    function Str (str)
      str.text = string.upper(str.text)
      return str
    end
    ```

Pattern behavior is locale dependent
:   The character classes in Lua's pattern library depend on the
    current locale: E.g., the character `©` will be treated as
    punctuation, and matched by the pattern `%p`, on CP-1252
    locales, but not on systems using a UTF-8 locale.

    A reliable way to ensure unified handling of patterns and
    character classes is to use the "C" locale by adding
    `os.setlocale 'C'` to the top of the Lua script.

String library is not Unicode aware
:   Lua's `string` library treats each byte as a single
    character. A function like `string.upper` will not have the
    intended effect when applied to words with non-ASCII
    characters. Similarly, a pattern like `[☃]` will match *any*
    of the bytes `\240`, `\159`, `\154`, and `\178`, but
    **won't** match the "snowman" Unicode character.

    Use the [pandoc.text](#module-pandoc.text) module for Unicode-aware
    transformation, and consider using using the lpeg or re
    library for pattern matching.

# Examples

The following filters are presented as examples. A repository of
useful Lua filters (which may also serve as good examples) is
available at <https://github.com/pandoc/lua-filters>.

## Macro substitution

The following filter converts the string `{{helloworld}}` into
emphasized text "Hello, World".

``` lua
return {
  Str = function (elem)
    if elem.text == "{{helloworld}}" then
      return pandoc.Emph {pandoc.Str "Hello, World"}
    else
      return elem
    end
  end,
}
```

## Center images in LaTeX and HTML output

For LaTeX, wrap an image in LaTeX snippets which cause the image
to be centered horizontally. In HTML, the image element's style
attribute is used to achieve centering.

``` lua
-- Filter images with this function if the target format is LaTeX.
if FORMAT:match 'latex' then
  function Image (elem)
    -- Surround all images with image-centering raw LaTeX.
    return {
      pandoc.RawInline('latex', '\\hfill\\break{\\centering'),
      elem,
      pandoc.RawInline('latex', '\\par}')
    }
  end
end

-- Filter images with this function if the target format is HTML
if FORMAT:match 'html' then
  function Image (elem)
    -- Use CSS style to center image
    elem.attributes.style = 'margin:auto; display: block;'
    return elem
  end
end
```

## Setting the date in the metadata

This filter sets the date in the document's metadata to the
current date, if a date isn't already set:

``` lua
function Meta(m)
  if m.date == nil then
    m.date = os.date("%B %e, %Y")
    return m
  end
end
```

## Remove spaces before citations

This filter removes all spaces preceding an "author-in-text"
citation. In Markdown, author-in-text citations (e.g.,
`@citekey`), must be preceded by a space. If these spaces are
undesired, they must be removed with a filter.

``` lua
local function is_space_before_author_in_text(spc, cite)
  return spc and spc.t == 'Space'
    and cite and cite.t == 'Cite'
    -- there must be only a single citation, and it must have
    -- mode 'AuthorInText'
    and #cite.citations == 1
    and cite.citations[1].mode == 'AuthorInText'
end

function Inlines (inlines)
  -- Go from end to start to avoid problems with shifting indices.
  for i = #inlines-1, 1, -1 do
    if is_space_before_author_in_text(inlines[i], inlines[i+1]) then
      inlines:remove(i)
    end
  end
  return inlines
end
```

## Replacing placeholders with their metadata value

Lua filter functions are run in the order

> *Inlines → Blocks → Meta → Pandoc*.

Passing information from a higher level (e.g., metadata) to a
lower level (e.g., inlines) is still possible by using two
filters living in the same file:

``` lua
local vars = {}

local function get_vars (meta)
  for k, v in pairs(meta) do
    if pandoc.utils.type(v) == 'Inlines' then
      vars["%" .. k .. "%"] = {table.unpack(v)}
    end
  end
end

local function replace (el)
  if vars[el.text] then
    return pandoc.Span(vars[el.text])
  else
    return el
  end
end

function Pandoc(doc)
  return doc:walk { Meta = get_vars }:walk { Str = replace }
end
```

If the contents of file `occupations.md` are

``` markdown
---
name: Samuel Q. Smith
occupation: Professor of Oenology
---

Name

:   %name%

Occupation

:   %occupation%
```

then running `pandoc --lua-filter=meta-vars.lua occupations.md`
will output:

``` html
<dl>
<dt>Name</dt>
<dd><p><span>Samuel Q. Smith</span></p>
</dd>
<dt>Occupation</dt>
<dd><p><span>Professor of Oenology</span></p>
</dd>
</dl>
```
Note that the placeholders must not contain any spaces, otherwise
they will turn into two separate Str elements and the filter
won't work.

## Modifying pandoc's `MANUAL.txt` for man pages

This is the filter we use when converting `MANUAL.txt` to man
pages. It converts level-1 headers to uppercase (using
[`walk`](#type-block:walk) to transform inline elements inside
headers), removes footnotes, and replaces links with regular
text.

``` lua
-- we use pandoc.text to get a UTF-8 aware 'upper' function
local text = pandoc.text

function Header(el)
    if el.level == 1 then
      return el:walk {
        Str = function(el)
            return pandoc.Str(text.upper(el.text))
        end
      }
    end
end

function Link(el)
    return el.content
end

function Note(el)
    return {}
end
```

## Creating a handout from a paper

This filter extracts all the numbered examples, section headers,
block quotes, and figures from a document, in addition to any
divs with class `handout`. (Note that only blocks at the "outer
level" are included; this ignores blocks inside nested
constructs, like list items.)

``` lua
-- creates a handout from an article, using its headings,
-- blockquotes, numbered examples, figures, and any
-- Divs with class "handout"

function Pandoc(doc)
    local hblocks = {}
    for i,el in pairs(doc.blocks) do
        if (el.t == "Div" and el.classes[1] == "handout") or
           (el.t == "BlockQuote") or
           (el.t == "OrderedList" and el.style == "Example") or
           (el.t == "Para" and #el.c == 1 and el.c[1].t == "Image") or
           (el.t == "Header") then
           table.insert(hblocks, el)
        end
    end
    return pandoc.Pandoc(hblocks, doc.meta)
end
```

## Counting words in a document

This filter counts the words in the body of a document (omitting
metadata like titles and abstracts), including words in code. It
should be more accurate than `wc -w` run directly on a Markdown
document, since the latter will count markup characters, like
the `#` in front of an ATX header, or tags in HTML documents, as
words. To run it, `pandoc --lua-filter wordcount.lua myfile.md`.

``` lua
-- counts words in a document

words = 0

wordcount = {
  Str = function(el)
    -- we don't count a word if it's entirely punctuation:
    if el.text:match("%P") then
        words = words + 1
    end
  end,

  Code = function(el)
    _,n = el.text:gsub("%S+","")
    words = words + n
  end,

  CodeBlock = function(el)
    _,n = el.text:gsub("%S+","")
    words = words + n
  end
}

function Pandoc(el)
    -- skip metadata, just count body:
    el.blocks:walk(wordcount)
    print(words .. " words in body")
    os.exit(0)
end
```

## Creating a table

This filter creates a document that contains the following 
table with 5 columns. It serves as a working example of how 
to use the [`pandoc.Table`](#pandoc.Table) constructor. 

+--------+--------+--------+--------+---+
| This   | is my  | table  | header |   |
+========+:=======+:======:+=======:+===+
| Cell 1 | Cell 2 | Cell 3 |        |   |
+--------+--------+--------+--------+---+
| Cell 4 | Cell 5 | Cell 6 |        |   |
+========+========+========+========+===+
| This is my table footer.          |   |
+===================================+===+

: This is my table caption.

Note that:

- The number of columns in the resulting Table element is 
  equal to the number of entries in the `colspecs` parameter.

- A [ColSpec] object must contain the cell alignment, but the 
  column width is optional.

- A [TableBody] object is specified using a Lua table in the 
  `bodies` parameter because there is no `pandoc.TableBody` 
  constructor.

```lua
function Pandoc ()
  local caption = pandoc.Caption( "This is my table caption." )
  local colspecs = {
    { pandoc.AlignLeft },
    { pandoc.AlignDefault }, 
    { pandoc.AlignCenter }, 
    { pandoc.AlignRight },
    { pandoc.AlignDefault }
  }
  local head = pandoc.TableHead{
    pandoc.Row{
      pandoc.Cell( "This" ), 
      pandoc.Cell( "is my" ), 
      pandoc.Cell( "table" ),
      pandoc.Cell( "header" )
    }
  }
  local bodies = {
    {
      attr={},
      body={ 
        pandoc.Row{
          pandoc.Cell( "Cell 1" ), 
          pandoc.Cell( "Cell 2" ), 
          pandoc.Cell( "Cell 3" )
        },
        pandoc.Row{
          pandoc.Cell( "Cell 4" ), 
          pandoc.Cell( "Cell 5" ), 
          pandoc.Cell( "Cell 6" )
        }
      },
      head={},
      row_head_columns=0
    }
  }
  local foot = pandoc.TableFoot{
    pandoc.Row{
      pandoc.Cell( "This is my table footer.", pandoc.AlignDefault, 1, 4 )
    }
  }
  return pandoc.Pandoc { 
    pandoc.Table(caption, colspecs, head, bodies, foot) 
  }
end
```

## Extracting links from a document

This filter creates a document containing a table that lists
the URLs the input document links to, together with the 
number of links to each URL.

```lua
links = {}

function Link (el)
  if links[el.target] then
    links[el.target] = links[el.target] + 1
  else
    links[el.target] = 1
  end
  return el
end

function Pandoc ()
  local caption = pandoc.Caption("Link count.")
  local colspecs = { 
    { pandoc.AlignDefault, 0.8 }, 
    { pandoc.AlignLeft, 0.2 }
  }
  local head = pandoc.TableHead{
    pandoc.Row{ pandoc.Cell("Target"), pandoc.Cell("Count") }
  }
  local foot = pandoc.TableFoot()
  local rows = {}
  for link, count in pairs(links) do
    rows[#rows + 1] = pandoc.Row{ 
        pandoc.Cell( link ), 
        pandoc.Cell( pandoc.utils.stringify(count) ) 
    }
  end
  local bodies = {
    {
      attr={},
      body=rows,
      head={},
      row_head_columns=0
    }
  }
  return pandoc.Pandoc {
    pandoc.Table(caption, colspecs, head, bodies, foot)
  }
end
```

## Converting ABC code to music notation

This filter replaces code blocks with class `abc` with images
created by running their contents through `abcm2ps` and
ImageMagick's `convert`. (For more on ABC notation, see
<https://abcnotation.com>.)

Images are added to the mediabag. For output to binary formats,
pandoc will use images in the mediabag. For textual formats, use
`--extract-media` to specify a directory where the files in the
mediabag will be written, or (for HTML only) use
`--embed-resources`.

``` lua
-- Pandoc filter to process code blocks with class "abc" containing
-- ABC notation into images.
--
-- * Assumes that abcm2ps and ImageMagick's convert are in the path.
-- * For textual output formats, use --extract-media=abc-images
-- * For HTML formats, you may alternatively use --embed-resources

local filetypes = { html = {"png", "image/png"}
                  , latex = {"pdf", "application/pdf"}
                  }
local filetype = filetypes[FORMAT][1] or "png"
local mimetype = filetypes[FORMAT][2] or "image/png"

local function abc2eps(abc, filetype)
    local eps = pandoc.pipe("abcm2ps", {"-q", "-O", "-", "-"}, abc)
    local final = pandoc.pipe("convert", {"-", filetype .. ":-"}, eps)
    return final
end

function CodeBlock(block)
    if block.classes[1] == "abc" then
        local img = abc2eps(block.text, filetype)
        local fname = pandoc.sha1(img) .. "." .. filetype
        pandoc.mediabag.insert(fname, mimetype, img)
        return pandoc.Para{ pandoc.Image({pandoc.Str("abc tune")}, fname) }
    end
end
```

## Building images with Ti*k*Z

This filter converts raw LaTeX Ti*k*Z environments into images. It
works with both PDF and HTML output. The Ti*k*Z code is compiled
to an image using `pdflatex`, and the image is converted from
pdf to svg format using
[`pdf2svg`](https://github.com/dawbarton/pdf2svg), so both of
these must be in the system path. Converted images are cached in
the working directory and given filenames based on a hash of the
source, so that they need not be regenerated each time the
document is built. (A more sophisticated version of this might
put these in a special cache directory.)

``` lua
local system = require 'pandoc.system'

local tikz_doc_template = [[
\documentclass{standalone}
\usepackage{xcolor}
\usepackage{tikz}
\begin{document}
\nopagecolor
%s
\end{document}
]]

local function tikz2image(src, filetype, outfile)
  system.with_temporary_directory('tikz2image', function (tmpdir)
    system.with_working_directory(tmpdir, function()
      local f = io.open('tikz.tex', 'w')
      f:write(tikz_doc_template:format(src))
      f:close()
      os.execute('pdflatex tikz.tex')
      if filetype == 'pdf' then
        os.rename('tikz.pdf', outfile)
      else
        os.execute('pdf2svg tikz.pdf ' .. outfile)
      end
    end)
  end)
end

extension_for = {
  html = 'svg',
  html4 = 'svg',
  html5 = 'svg',
  latex = 'pdf',
  beamer = 'pdf' }

local function file_exists(name)
  local f = io.open(name, 'r')
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

local function starts_with(start, str)
  return str:sub(1, #start) == start
end


function RawBlock(el)
  if starts_with('\\begin{tikzpicture}', el.text) then
    local filetype = extension_for[FORMAT] or 'svg'
    local fbasename = pandoc.sha1(el.text) .. '.' .. filetype
    local fname = system.get_working_directory() .. '/' .. fbasename
    if not file_exists(fname) then
      tikz2image(el.text, filetype, fname)
    end
    return pandoc.Para({pandoc.Image({}, fbasename)})
  else
   return el
  end
end
```

Example of use:

    pandoc --lua-filter tikz.lua -s -o cycle.html <<EOF
    Here is a diagram of the cycle:

    \begin{tikzpicture}

    \def \n {5}
    \def \radius {3cm}
    \def \margin {8} % margin in angles, depends on the radius

    \foreach \s in {1,...,\n}
    {
      \node[draw, circle] at ({360/\n * (\s - 1)}:\radius) {$\s$};
      \draw[->, >=latex] ({360/\n * (\s - 1)+\margin}:\radius)
        arc ({360/\n * (\s - 1)+\margin}:{360/\n * (\s)-\margin}:\radius);
    }
    \end{tikzpicture}
    EOF

# Lua type reference

This section describes the types of objects available to Lua
filters. See the [pandoc module](#module-pandoc) for
functions to create these objects.

## Shared Properties

### `clone`

`clone ()`

All instances of the types listed here, with the exception of
read-only objects, can be cloned via the `clone()` method.

Usage:

    local emph = pandoc.Emph {pandoc.Str 'important'}
    local cloned_emph = emph:clone()  -- note the colon

## Pandoc {#type-pandoc}

Pandoc document

Values of this type can be created with the
[`pandoc.Pandoc`](#pandoc.Pandoc) constructor. Pandoc values are
equal in Lua if and only if they are equal in Haskell.

`blocks`
:   document content ([Blocks][])

`meta`
:   document meta information ([Meta] object)

### Methods {#type-pandoc-methods}

#### normalize

`normalize(self)`

Perform a normalization of Pandoc documents. E.g., multiple
successive spaces are collapsed, and tables are normalized, so
that all rows and columns contain the same number of cells.

Parameters:

`self`
:   the element ([Pandoc][])

Results:

-   cloned and normalized document. ([Pandoc][])

### walk {#type-pandoc:walk}

`walk(self, lua_filter)`

Applies a Lua filter to the Pandoc element. Just as for
full-document filters, the order in which elements are traversed
can be controlled by setting the `traverse` field of the filter;
see the section on [traversal order][Traversal order]. Returns a
(deep) copy on which the filter has been applied: the original
element is left untouched.

Parameters:

`self`
:   the element ([Pandoc](#type-pandoc))

`lua_filter`
:   map of filter functions (table)

Result:

-   filtered document ([Pandoc][])

Usage:

    -- returns `pandoc.Pandoc{pandoc.Para{pandoc.Str 'Bye'}}`
    return pandoc.Pandoc{pandoc.Para('Hi')}:walk {
      Str = function (_) return 'Bye' end,
    }


## Meta {#type-meta}

Meta information on a document; string-indexed collection of
[MetaValues].

Values of this type can be created with the
[`pandoc.Meta`](#pandoc.meta) constructor. Meta values are equal
in Lua if and only if they are equal in Haskell.

## MetaValue {#type-metavalue}

Document meta information items. This is not a separate type, but
describes a set of types that can be used in places were a
MetaValue is expected. The types correspond to the following
Haskell type constructors:

- boolean → MetaBool
- string or number → MetaString
- Inlines → MetaInlines
- Blocks → MetaBlocks
- List/integer indexed table → MetaList
- string-indexed table → MetaMap

The corresponding constructors
[`pandoc.MetaBool`](#pandoc.metabool),
[`pandoc.MetaString`](#pandoc.metastring),
[`pandoc.MetaInlines`](#pandoc.metainlines),
[`pandoc.MetaBlocks`](#pandoc.metablocks),
[`pandoc.MetaList`](#pandoc.metalist), and
[`pandoc.MetaMap`](#pandoc.metamap)
can be used to ensure that a value is treated in the intended
way. E.g., an empty table is normally treated as a `MetaMap`, but
can be made into an empty `MetaList` by calling
`pandoc.MetaList{}`. However, the same can be accomplished by
using the generic functions like `pandoc.List`, `pandoc.Inlines`,
or `pandoc.Blocks`.

Use the function [`pandoc.utils.type`](#pandoc.utils.type) to
get the type of a metadata value.

## Block {#type-block}

Block values are equal in Lua if and only if they are equal in
Haskell.

### Common methods

#### walk {#type-block:walk}

`walk(self, lua_filter)`

Applies a Lua filter to the block element. Just as for
full-document filters, the order in which elements are traversed
can be controlled by setting the `traverse` field of the filter;
see the section on [traversal order][Traversal order]. Returns a
(deep) copy on which the filter has been applied: the original
element is left untouched.

Note that the filter is applied to the subtree, but not to the
`self` block element. The rationale is that otherwise the element
could be deleted by the filter, or replaced with multiple block
elements, which might lead to possibly unexpected results.

Parameters:

`self`
:   the element ([Block](#type-block))

`lua_filter`
:   map of filter functions (table)

Result:

-   filtered block ([Block][])

Usage:

    -- returns `pandoc.Para{pandoc.Str 'Bye'}`
    return pandoc.Para('Hi'):walk {
      Str = function (_) return 'Bye' end,
    }


### BlockQuote {#type-blockquote}

A block quote element.

Values of this type can be created with the
[`pandoc.BlockQuote`](#pandoc.BlockQuote) constructor.

Fields:

`content`
:   block content ([Blocks][])

`tag`, `t`
:   the literal `BlockQuote` (string)

### BulletList {#type-bulletlist}

A bullet list.

Values of this type can be created with the
[`pandoc.BulletList`](#pandoc.BulletList) constructor.

Fields:

`content`
:   list items ([List] of items, i.e., [List] of [Blocks][])

`tag`, `t`
:   the literal `BulletList` (string)

### CodeBlock {#type-codeblock}

Block of code.

Values of this type can be created with the
[`pandoc.CodeBlock`](#pandoc.CodeBlock) constructor.

Fields:

`text`
:   code string (string)

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `CodeBlock` (string)

### DefinitionList {#type-definitionlist}

Definition list, containing terms and their explanation.

Values of this type can be created with the
[`pandoc.DefinitionList`](#pandoc.DefinitionList) constructor.

Fields:

`content`
:   list of items

`tag`, `t`
:   the literal `DefinitionList` (string)

### Div {#type-div}

Generic block container with attributes.

Values of this type can be created with the
[`pandoc.Div`](#pandoc.Div) constructor.

Fields:

`content`
:   block content ([Blocks][])

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of
    strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Div` (string)

### Figure {#type-figure}

Figure with caption and arbitrary block contents.

Values of this type can be created with the
[`pandoc.Figure`](#pandoc.Figure) constructor.

Fields:

`content`
:   block content ([Blocks][])

`caption`
:   figure caption ([Caption][])

`attr`
:   element attributes ([Attr][])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List][] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes][])

`tag`, `t`
:   the literal `Figure` (string)

### Header {#type-header}

Creates a header element.

Values of this type can be created with the
[`pandoc.Header`](#pandoc.Header) constructor.

Fields:

`level`
:   header level (integer)

`content`
:   inline content ([Inlines][])

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes`
    ([Attributes])

`tag`, `t`
:   the literal `Header` (string)

### HorizontalRule {#type-horizontalrule}

A horizontal rule.

Values of this type can be created with the
[`pandoc.HorizontalRule`](#pandoc.HorizontalRule) constructor.

Fields:

`tag`, `t`
:   the literal `HorizontalRule` (string)

### LineBlock {#type-lineblock}

A line block, i.e. a list of lines, each separated from the next
by a newline.

Values of this type can be created with the
[`pandoc.LineBlock`](#pandoc.LineBlock) constructor.

Fields:

`content`
:   inline content ([List] of lines, i.e. [List] of [Inlines][])

`tag`, `t`
:   the literal `LineBlock` (string)

### OrderedList {#type-orderedlist}

An ordered list.

Values of this type can be created with the
[`pandoc.OrderedList`](#pandoc.OrderedList) constructor.

Fields:

`content`
:   list items ([List] of items, i.e., [List] of [Blocks][])

`listAttributes`
:   list parameters ([ListAttributes])

`start`
:   alias for `listAttributes.start` (integer)

`style`
:   alias for `listAttributes.style` (string)

`delimiter`
:   alias for `listAttributes.delimiter` (string)

`tag`, `t`
:   the literal `OrderedList` (string)

### Para {#type-para}

A paragraph.

Values of this type can be created with the
[`pandoc.Para`](#pandoc.Para) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Para` (string)

### Plain {#type-plain}

Plain text, not a paragraph.

Values of this type can be created with the
[`pandoc.Plain`](#pandoc.Plain) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Plain` (string)

### RawBlock {#type-rawblock}

Raw content of a specified format.

Values of this type can be created with the
[`pandoc.RawBlock`](#pandoc.RawBlock) constructor.

Fields:

`format`
:   format of content (string)

`text`
:   raw content (string)

`tag`, `t`
:   the literal `RawBlock` (string)

### Table {#type-table}

A table.

Values of this type can be created with the
[`pandoc.Table`](#pandoc.Table) constructor.

Fields:

`attr`
:   table attributes ([Attr])

`caption`
:   table caption ([Caption])

`colspecs`
:   column specifications, i.e., alignments and widths ([List] of
    [ColSpec]s)

`head`
:   table head ([TableHead])

`bodies`
:   table bodies ([List] of [TableBody]s)

`foot`
:   table foot ([TableFoot])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Table` (string)

A [table cell]{#type-table-cell} is a list of blocks.

*[Alignment]{#type-alignment}* is a string value indicating the
horizontal alignment of a table column. `AlignLeft`,
`AlignRight`, and `AlignCenter` leads cell content to be
left-aligned, right-aligned, and centered, respectively. The
default alignment is `AlignDefault` (often equivalent to
centered).

## Blocks {#type-blocks}

List of [Block] elements, with the same methods as a generic
[List](#type-list). It is usually not necessary to create values
of this type in user scripts, as pandoc can convert other types
into Blocks wherever a value of this type is expected:

-   a list of [Block] (or Block-like) values is used directly;
-   a single [Inlines][] value is wrapped into a [Plain] element;
-   string values are turned into an [Inlines][] value by splitting
    the string into words (see [Inlines](#type-inlines)), and
    then wrapping the result into a Plain singleton.

### Methods

Lists of type `Blocks` share all methods available in generic
lists, see the [`pandoc.List` module](#module-pandoc.list).

Additionally, the following methods are available on Blocks
values:

#### walk {#type-blocks:walk}

`walk(self, lua_filter)`

Applies a Lua filter to the Blocks list. Just as for
full-document filters, the order in which elements are traversed
can be controlled by setting the `traverse` field of the filter;
see the section on [traversal order][Traversal order]. Returns a
(deep) copy on which the filter has been applied: the original
list is left untouched.

Parameters:

`self`
:   the list ([Blocks][])

`lua_filter`
:   map of filter functions (table)

Result:

-   filtered list ([Blocks][])

Usage:

    -- returns `pandoc.Blocks{pandoc.Para('Salve!')}`
    return pandoc.Blocks{pandoc.Plain('Salve!)}:walk {
      Plain = function (p) return pandoc.Para(p.content) end,
    }

## Inline {#type-inline}

Inline values are equal in Lua if and only if they are equal in
Haskell.

### Common methods

#### walk {#type-inline:walk}

`walk(self, lua_filter)`

Applies a Lua filter to the Inline element. Just as for
full-document filters, the order in which elements are traversed
can be controlled by setting the `traverse` field of the filter;
see the section on [traversal order][Traversal order]. Returns a
(deep) copy on which the filter has been applied: the original
element is left untouched.

Note that the filter is applied to the subtree, but not to the
`self` inline element. The rationale is that otherwise the
element could be deleted by the filter, or replaced with multiple
inline elements, which might lead to possibly unexpected results.

Parameters:

`self`
:   the element ([Inline](#type-inline))

`lua_filter`
:   map of filter functions (table)

Result:

-   filtered inline element ([Inline][])

Usage:

    -- returns `pandoc.SmallCaps('SPQR)`
    return pandoc.SmallCaps('spqr'):walk {
      Str = function (s) return string.upper(s.text) end,
    }

### Cite {#type-cite}

Citation.

Values of this type can be created with the
[`pandoc.Cite`](#pandoc.Cite) constructor.

Fields:

`content`
:   ([Inlines][])

`citations`
:   citation entries ([List] of [Citations])

`tag`, `t`
:   the literal `Cite` (string)

### Code {#type-code}

Inline code

Values of this type can be created with the
[`pandoc.Code`](#pandoc.Code) constructor.

Fields:

`text`
:   code string (string)

`attr`
:   attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Code` (string)

### Emph {#type-emph}

Emphasized text

Values of this type can be created with the
[`pandoc.Emph`](#pandoc.Emph) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Emph` (string)

### Image {#type-image}

Image: alt text (list of inlines), target

Values of this type can be created with the
[`pandoc.Image`](#pandoc.Image) constructor.

Fields:

`caption`
:   text used to describe the image ([Inlines][])

`src`
:   path to the image file (string)

`title`
:   brief image description (string)

`attr`
:   attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Image` (string)

### LineBreak {#type-linebreak}

Hard line break

Values of this type can be created with the
[`pandoc.LineBreak`](#pandoc.LineBreak) constructor.

Fields:

`tag`, `t`
:   the literal `LineBreak` (string)

### Link {#type-link}

Hyperlink: alt text (list of inlines), target

Values of this type can be created with the
[`pandoc.Link`](#pandoc.Link) constructor.

Fields:

`attr`
:   attributes ([Attr])

`content`
:   text for this link ([Inlines][])

`target`
:   the link target (string)

`title`
:   brief link description

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Link` (string)

### Math {#type-math}

TeX math (literal)

Values of this type can be created with the
[`pandoc.Math`](#pandoc.Math) constructor.

Fields:

`mathtype`
:   specifier determining whether the math content should be
    shown inline (`InlineMath`) or on a separate line
    (`DisplayMath`) (string)

`text`
:   math content (string)

`tag`, `t`
:   the literal `Math` (string)

### Note {#type-note}

Footnote or endnote

Values of this type can be created with the
[`pandoc.Note`](#pandoc.Note) constructor.

Fields:

`content`
:   ([Blocks][])

`tag`, `t`
:   the literal `Note` (string)

### Quoted {#type-quoted}

Quoted text

Values of this type can be created with the
[`pandoc.Quoted`](#pandoc.Quoted) constructor.

Fields:

`quotetype`
:   type of quotes to be used; one of `SingleQuote` or
    `DoubleQuote` (string)

`content`
:   quoted text ([Inlines][])

`tag`, `t`
:   the literal `Quoted` (string)

### RawInline {#type-rawinline}

Raw inline

Values of this type can be created with the
[`pandoc.RawInline`](#pandoc.RawInline) constructor.

Fields:

`format`
:   the format of the content (string)

`text`
:   raw content (string)

`tag`, `t`
:   the literal `RawInline` (string)

### SmallCaps {#type-smallcaps}

Small caps text

Values of this type can be created with the
[`pandoc.SmallCaps`](#pandoc.Smallcaps) constructor.

Fields:

`content`
:   ([Inlines][])

`tag`, `t`
:   the literal `SmallCaps` (string)

### SoftBreak {#type-softbreak}

Soft line break

Values of this type can be created with the
[`pandoc.SoftBreak`](#pandoc.Softbreak) constructor.

Fields:

`tag`, `t`
:   the literal `SoftBreak` (string)

### Space {#type-space}

Inter-word space

Values of this type can be created with the
[`pandoc.Space`](#pandoc.Space) constructor.

Fields:

`tag`, `t`
:   the literal `Space` (string)

### Span {#type-span}

Generic inline container with attributes

Values of this type can be created with the
[`pandoc.Span`](#pandoc.Span) constructor.

Fields:

`attr`
:   attributes ([Attr])

`content`
:   wrapped content ([Inlines][])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Span` (string)

### Str {#type-str}

Text

Values of this type can be created with the
[`pandoc.Str`](#pandoc.Str) constructor.

Fields:

`text`
:   content (string)

`tag`, `t`
:   the literal `Str` (string)

### Strikeout {#type-strikeout}

Strikeout text

Values of this type can be created with the
[`pandoc.Strikeout`](#pandoc.Strikeout) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Strikeout` (string)

### Strong {#type-strong}

Strongly emphasized text

Values of this type can be created with the
[`pandoc.Strong`](#pandoc.Strong) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Strong` (string)

### Subscript {#type-subscript}

Subscripted text

Values of this type can be created with the
[`pandoc.Subscript`](#pandoc.Subscript) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Subscript` (string)

### Superscript {#type-superscript}

Superscripted text

Values of this type can be created with the
[`pandoc.Superscript`](#pandoc.Superscript) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Superscript` (string)

### Underline {#type-underline}

Underlined text

Values of this type can be created with the
[`pandoc.Underline`](#pandoc.Underline) constructor.

Fields:

`content`
:   inline content ([Inlines][])

`tag`, `t`
:   the literal `Underline` (string)

## Inlines {#type-inlines}

List of [Inline] elements, with the same methods as a generic
[List](#type-list). It is usually not necessary to create values
of this type in user scripts, as pandoc can convert other types
into Inlines wherever a value of this type is expected:

-   lists of [Inline] (or Inline-like) values are used directly;
-   single [Inline] values are converted into a list containing
    just that element;
-   String values are split into words, converting line breaks
    into [SoftBreak](#type-softbreak) elements, and other
    whitespace characters into [Spaces](#type-space).

### Methods

Lists of type `Inlines` share all methods available in generic
lists, see the [`pandoc.List` module](#module-pandoc.list).

Additionally, the following methods are available on *Inlines*
values:

#### walk {#type-inlines:walk}

`walk(self, lua_filter)`

Applies a Lua filter to the Inlines list. Just as for
full-document filters, the order in which elements are handled
are Inline → Inlines → Block → Blocks. The filter is applied
to all list items *and* to the list itself. Returns a (deep)
copy on which the filter has been applied: the original list is
left untouched.

Parameters:

`self`
:   the list ([Inlines](#type-inlines))

`lua_filter`
:   map of filter functions (table)

Result:

-   filtered list ([Inlines](#type-inlines))

Usage:

    -- returns `pandoc.Inlines{pandoc.SmallCaps('SPQR')}`
    return pandoc.Inlines{pandoc.Emph('spqr')}:walk {
      Str = function (s) return string.upper(s.text) end,
      Emph = function (e) return pandoc.SmallCaps(e.content) end,
    }


## Element components

### Attr {#type-attr}

A set of element attributes. Values of this type can be created
with the [`pandoc.Attr`](#pandoc.Attr) constructor. For
convenience, it is usually not necessary to construct the value
directly if it is part of an element, and it is sufficient to
pass an HTML-like table. E.g., to create a span with identifier
"text" and classes "a" and "b", one can write:

    local span = pandoc.Span('text', {id = 'text', class = 'a b'})

This also works when using the `attr` setter:

    local span = pandoc.Span 'text'
    span.attr = {id = 'text', class = 'a b', other_attribute = '1'}

Attr values are equal in Lua if and only if they are equal in
Haskell.

Fields:

`identifier`
:   element identifier (string)

`classes`
:   element classes ([List] of strings)

`attributes`
:   collection of key/value pairs ([Attributes])

### Attributes {#type-attributes}

List of key/value pairs. Values can be accessed by using keys as
indices to the list table.

Attributes values are equal in Lua if and only if they are equal
in Haskell.

### Caption {#type-caption}

The caption of a table, with an optional short caption.

Fields:

`long`
:   long caption ([Blocks][])

`short`
:   short caption ([Inlines][])

### Cell {#type-cell}

A table cell.

Fields:

`attr`
:   cell attributes

`alignment`
:   individual cell alignment ([Alignment]).

`contents`
:   cell contents ([Blocks][]).

`col_span`
:   number of columns spanned by the cell; the width of the cell
    in columns (integer).

`row_span`
:   number of rows spanned by the cell; the height of the cell in
    rows (integer).

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

### Citation {#type-citation}

Single citation entry

Values of this type can be created with the
[`pandoc.Citation`](#pandoc.citation) constructor.

Citation values are equal in Lua if and only if they are equal in
Haskell.

Fields:

`id`
:   citation identifier, e.g., a bibtex key (string)

`mode`
:   citation mode, one of `AuthorInText`, `SuppressAuthor`, or
    `NormalCitation` (string)

`prefix`
:   citation prefix ([Inlines][])

`suffix`
:   citation suffix ([Inlines][])

`note_num`
:   note number (integer)

`hash`
:   hash (integer)

### ColSpec {#type-colspec}

Column alignment and width specification for a single table
column.

This is a pair, i.e., a plain table, with the following
components:

1. cell alignment ([Alignment]).
2. table column width, as a fraction of the page width (number).

### ListAttributes {#type-listattributes}

List attributes

Values of this type can be created with the
[`pandoc.ListAttributes`](#pandoc.ListAttributes) constructor.

Fields:

`start`
:   number of the first list item (integer)

`style`
:   style used for list numbers; possible values are
    `DefaultStyle`, `Example`, `Decimal`, `LowerRoman`,
    `UpperRoman`, `LowerAlpha`, and `UpperAlpha` (string)

`delimiter`
:   delimiter of list numbers; one of `DefaultDelim`, `Period`,
    `OneParen`, and `TwoParens` (string)

### Row {#type-row}

A table row.

Fields:

`attr`
:   element attributes ([Attr][])

`cells`
:   list of table cells ([List][] of [Cell][]s)

### TableBody {#type-tablebody}

A body of a table, with an intermediate head and the specified
number of row header columns.

Fields:

`attr`
:   table body attributes ([Attr][])

`body`
:   table body rows ([List][] of [Row][]s)

`head`
:   intermediate head ([List][] of [Row][]s)

`row_head_columns`
:   number of columns taken up by the row head of each row of a
    [TableBody][]. The row body takes up the remaining columns.

### TableFoot {#type-tablefoot}

The foot of a table.

Fields:

`attr`
:   element attributes ([Attr][])

`rows`
:   list of rows ([List][] of [Row][]s)

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List][] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes][])

### TableHead {#type-tablehead}

The head of a table.

Fields:

`attr`
:   element attributes ([Attr][])

`rows`
:   list of rows ([List][] of [Row][]s)

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List][] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes][])

## ReaderOptions {#type-readeroptions}

Pandoc reader options

Fields:

`abbreviations`
:   set of known abbreviations (set of strings)

`columns`
:   number of columns in terminal (integer)

`default_image_extension`
:   default extension for images (string)

`extensions`
:   string representation of the syntax extensions bit field
    (sequence of strings)

`indented_code_classes`
:   default classes for indented code blocks (list of strings)

`standalone`
:   whether the input was a standalone document with header
    (boolean)

`strip_comments`
:   HTML comments are stripped instead of parsed as raw HTML
    (boolean)

`tab_stop`
:   width (i.e. equivalent number of spaces) of tab stops
    (integer)

`track_changes`
:   track changes setting for docx; one of `accept-changes`,
    `reject-changes`, and `all-changes` (string)

## WriterOptions {#type-writeroptions}

Pandoc writer options

Fields:

`chunk_template`
:   Template used to generate chunked HTML filenames (string)

`cite_method`
:   How to print cites -- one of 'citeproc', 'natbib', or
    'biblatex' (string)

`columns`
:   Characters in a line (for text wrapping) (integer)

`dpi`
:   DPI for pixel to/from inch/cm conversions (integer)

`email_obfuscation`
:   How to obfuscate emails -- one of 'none', 'references', or
    'javascript' (string)

`epub_chapter_level`
:   Header level for chapters, i.e., how the document is split
    into separate files (integer)

`epub_fonts`
:   Paths to fonts to embed (sequence of strings)

`epub_metadata`
:   Metadata to include in EPUB (string|nil)

`epub_subdirectory`
:   Subdir for epub in OCF (string)

`extensions`
:   Markdown extensions that can be used (sequence of strings)

`highlight_style`
:   Style to use for highlighting; see the output of `pandoc
    --print-highlight-style=...` for an example structure. The
    value `nil` means that no highlighting is used. (table|nil)

`html_math_method`
:   How to print math in HTML; one of 'plain', 'mathjax',
    'mathml', 'webtex', 'katex', 'gladtex', or a table with keys
    `method` and `url`. (string|table)

`html_q_tags`
:   Use `<q>` tags for quotes in HTML (boolean)

`identifier_prefix`
:   Prefix for section & note ids in HTML and for footnote marks
    in markdown (string)

`incremental`
:   True if lists should be incremental (boolean)

`listings`
:   Use listings package for code (boolean)

`number_offset`
:   Starting number for section, subsection, ... (sequence of
    integers)

`number_sections`
:   Number sections in LaTeX (boolean)

`prefer_ascii`
:   Prefer ASCII representations of characters when possible
    (boolean)

`reference_doc`
:   Path to reference document if specified (string|nil)

`reference_links`
:   Use reference links in writing markdown, rst (boolean)

`reference_location`
:   Location of footnotes and references for writing markdown;
    one of 'end-of-block', 'end-of-section', 'end-of-document'.
    The common prefix may be omitted when setting this value.
    (string)

`section_divs`
:   Put sections in div tags in HTML (boolean)

`setext_headers`
:   Use setext headers for levels 1-2 in markdown (boolean)

`slide_level`
:   Force header level of slides (integer\|nil)

`tab_stop`
:   Tabstop for conversion btw spaces and tabs (integer)

`table_of_contents`
:   Include table of contents (boolean)

`template`
:   Template to use ([Template](#type-template)|nil)

`toc_depth`
:   Number of levels to include in TOC (integer)

`top_level_division`
:   Type of top-level divisions; one of 'top-level-part',
    'top-level-chapter', 'top-level-section', or
    'top-level-default'. The prefix `top-level` may be omitted
    when setting this value. (string)

`variables`
:   Variables to set in template; string-indexed table (table)

`wrap_text`
:   Option for wrapping text; one of 'wrap-auto', 'wrap-none',
    or 'wrap-preserve'. The `wrap-` prefix may be omitted when
    setting this value. (string)


## CommonState {#type-commonstate}

The state used by pandoc to collect information and make it
available to readers and writers.

Fields:

`input_files`
:   List of input files from command line
    ([List] of strings)

`output_file`
:   Output file from command line (string or nil)

`log`
:   A list of log messages ([List] of [LogMessage]s)

`request_headers`
:   Headers to add for HTTP requests; table with header names as
    keys and header contents as value (table)

`resource_path`
:   Path to search for resources like included images
    ([List] of strings)

`source_url`
:   Absolute URL or directory of first source file (string or
    nil)

`user_data_dir`
:   Directory to search for data files (string or nil)

`trace`
:   Whether tracing messages are issued (boolean)

`verbosity`
:   Verbosity level; one of `INFO`, `WARNING`, `ERROR` (string)

## Doc {#type-doc}

Reflowable plain-text document. A Doc value can be rendered and
reflown to fit a given column width.

The [`pandoc.layout`](#module-pandoc.layout) module can be used to
create and modify Doc values. All functions in that module that
take a Doc value as their first argument are also available as Doc
methods. E.g., `(pandoc.layout.literal 'text'):render()`.

If a string is passed to a function expecting a Doc, then the
string is treated as a literal value. I.e., the following two
lines are equivalent:

``` lua
test = pandoc.layout.quotes(pandoc.layout.literal 'this')
test = pandoc.layout.quotes('this')
```
### Operators {#type-doc-operators}

#### `..` {#type-doc.__concat}

Concatenate two `Doc` elements.

#### `+` {#type-doc.__add}

Concatenate two `Doc`s, inserting a reflowable space between them.

#### `/` {#type-doc.__div}

If `a` and `b` are `Doc` elements, then `a / b` puts `a` above `b`.

#### `//` {#type-doc.__idiv}

If `a` and `b` are `Doc` elements, then `a // b` puts `a` above
`b`, inserting a blank line between them.


## List {#type-list}

A list is any Lua table with integer indices. Indices start at
one, so if `alist = {'value'}` then `alist[1] == 'value'`.

Lists, when part of an element, or when generated during
marshaling, are made instances of the `pandoc.List` type for
convenience. The `pandoc.List` type is defined in the
[*pandoc.List*](#module-pandoc.list) module. See there for
available methods.

Values of this type can be created with the
[`pandoc.List`](#pandoc.list) constructor, turning a normal Lua
table into a List.

## LogMessage {#type-logmessage}

A pandoc log message. Objects have no fields, but can be
converted to a string via `tostring`.

## SimpleTable {#type-simpletable}

A simple table is a table structure which resembles the old (pre
pandoc 2.10) Table type. Bi-directional conversion from and to
[Tables](#type-table) is possible with the
[`pandoc.utils.to_simple_table`](#pandoc.utils.to_simple_table)
and
[`pandoc.utils.from_simple_table`](#pandoc.utils.from_simple_table)
function, respectively. Instances of this type can also be created
directly with the [`pandoc.SimpleTable`](#pandoc.simpletable)
constructor.

Fields:

`caption`
:   [Inlines][]

`aligns`
:   column alignments ([List] of [Alignments](#type-alignment))

`widths`
:   column widths; a  ([List] of numbers)

`headers`
:   table header row ([List] of simple cells, i.e., [List] of
    [Blocks][])

`rows`
:   table rows ([List] of rows, where a row is a list of simple
    cells, i.e., [List] of [Blocks][])

## Template {#type-template}

Opaque type holding a compiled template.

## Version {#type-version}

A version object. This represents a software version like
"2.7.3". The object behaves like a numerically indexed table,
i.e., if `version` represents the version `2.7.3`, then

    version[1] == 2
    version[2] == 7
    version[3] == 3
    #version == 3   -- length

Comparisons are performed element-wise, i.e.

    Version '1.12' > Version '1.9'

Values of this type can be created with the
[`pandoc.types.Version`](#pandoc.types.Version) constructor.

### `must_be_at_least`

`must_be_at_least(actual, expected [, error_message])`

Raise an error message if the actual version is older than the
expected version; does nothing if `actual` is equal to or newer
than the expected version.

Parameters:

`actual`
:   actual version specifier ([Version])

`expected`
:   minimum expected version ([Version])

`error_message`
:   optional error message template. The string is used as
    format string, with the expected and actual versions as
    arguments. Defaults to
    `"expected version %s or newer, got %s"`.

Usage:

    PANDOC_VERSION:must_be_at_least '2.7.3'
    PANDOC_API_VERSION:must_be_at_least(
      '1.17.4',
      'pandoc-types is too old: expected version %s, got %s'
    )

[Attributes]: #type-attributes
[Cells]: #type-cell
[Citation]: #type-citation
[Citations]: #type-citation
[ColSpec]: #type-colspec
[CommonState]: #type-commonstate
[Div]: #type-div
[Image]: #type-image
[List]: #type-list
[ListAttributes]: #type-listattributes
[Meta]: #type-meta
[MetaBlocks]: #type-metablocks
[MetaValue]: #type-metavalue
[MetaValues]: #type-metavalue
[LogMessage]: #type-logmessage
[Pandoc]: #type-pandoc
[Para]: #type-para
[Plain]: #type-plain
[Rows]: #type-row
[SimpleTable]: #type-simpletable
[Version]: #type-version

## Chunk {#type-chunk}

Part of a document; usually chunks are each written to a separate
file.

Fields:

`heading`
:   heading text ([Inlines][])

`id`
:   identifier (string)

`level`
:   level of topmost heading in chunk (integer)

`number`
:   chunk number (integer)

`section_number`
:   hierarchical section number (string)

`path`
:   target filepath for this chunk (string)

`up`
:   link to the enclosing section, if any ([Chunk][]|nil)

`prev`
:   link to the previous section, if any ([Chunk][]|nil)

`next`
:   link to the next section, if any ([Chunk][]|nil)

`unlisted`
:   whether the section in this chunk should be listed in the TOC
    even if the chunk has no section number. (boolean)

`contents`
:   the chunk's block contents ([Blocks][])

## ChunkedDoc {#type-chunkeddoc}

A Pandoc document divided into [Chunks]{#type-chunk}.

The table of contents info in field `toc` is rose-tree structure
represented as a list. The node item is always placed at index
`0`; subentries make up the rest of the list. Each node item
contains the fields `title` ([Inlines][]), `number` (string|nil),
`id` (string), `path` (string), and `level` (integer).

Fields:

`chunks`
:   list of chunks that make up the document (list of
    [Chunks](#type-chunk)).

`meta`
:   the document's metadata ([Meta][])

`toc`
:   table of contents information (table)

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc -->

# Module pandoc

Fields and functions for pandoc scripts; includes constructors for
document tree elements, functions to parse text in a given format,
and functions to filter and modify a subtree.

## Fields {#pandoc-fields}

### readers {#pandoc.readers}

Set of formats that pandoc can parse. All keys in this table can
be used as the `format` value in `pandoc.read`. (table)

### writers {#pandoc.writers}

Set of formats that pandoc can generate. All keys in this table
can be used as the `format` value in `pandoc.write`. (table)

## Functions {#pandoc-functions}

### Pandoc {#pandoc.Pandoc}

`Pandoc (blocks[, meta])`

Parameters:

`blocks`
:   document contents ([Blocks])

`meta`
:   document metadata ([Meta])

Returns:

- new Pandoc document ([Pandoc])

### Meta {#pandoc.Meta}

`Meta (meta)`

Parameters:

`meta`
:   table containing meta information (table)

Returns:

- new Meta table (table)

### MetaBlocks {#pandoc.MetaBlocks}

`MetaBlocks (content)`

Creates a value to be used as a MetaBlocks value in meta data;
creates a copy of the input list via `pandoc.Blocks`, discarding
all non-list keys.

Parameters:

`content`
:   block content ([Blocks])

Returns:

- list of Block elements ([Blocks])

### MetaBool {#pandoc.MetaBool}

`MetaBool (bool)`

Parameters:

`bool`
:   true or false (boolean)

Returns:

- input, unchanged (boolean)

### MetaInlines {#pandoc.MetaInlines}

`MetaInlines (inlines)`

Creates a value to be used as a MetaInlines value in meta data;
creates a copy of the input list via `pandoc.Inlines`, discarding
all non-list keys.

Parameters:

`inlines`
:   inline elements ([Inlines])

Returns:

- list of Inline elements ([Inlines])

### MetaList {#pandoc.MetaList}

`MetaList (values)`

Creates a value to be used as a MetaList in meta data; creates a
copy of the input list via `pandoc.List`, discarding all non-list
keys.

Parameters:

`values`
:   value, or list of values ([MetaValue]\|{[MetaValue],\...})

Returns:

- list of meta values ([List]{unknown-type="List"})

### MetaMap {#pandoc.MetaMap}

`MetaMap (key_value_map)`

Creates a value to be used as a MetaMap in meta data; creates a
copy of the input table, keeping only pairs with string keys and
discards all other keys.

Parameters:

`key_value_map`
:   a string-indexed map of meta values (table)

Returns:

- map of meta values (table)

### MetaString {#pandoc.MetaString}

`MetaString (s)`

Creates a value to be used as a MetaString in meta data; this is
the identity function for boolean values and exists only for
completeness.

Parameters:

`s`
:   string value (string)

Returns:

- unchanged input (string)

### BlockQuote {#pandoc.BlockQuote}

`BlockQuote (content)`

Creates a block quote element

Parameters:

`content`
:   block content ([Blocks])

Returns:

- BlockQuote element ([Block])

### BulletList {#pandoc.BulletList}

`BulletList (items)`

Creates a bullet list.

Parameters:

`items`
:   list items ({[Blocks],\...})

Returns:

- BulletList element ([Block])

### CodeBlock {#pandoc.CodeBlock}

`CodeBlock (text[, attr])`

Creates a code block element.

Parameters:

`text`
:   code string (string)

`attr`
:   element attributes ([Attr])

Returns:

- CodeBlock element ([Block])

### DefinitionList {#pandoc.DefinitionList}

`DefinitionList (content)`

Creates a definition list, containing terms and their explanation.

Parameters:

`content`
:   definition items ([{{Inlines,
    {Blocks,\...}},\...}]{unknown-type="{{Inlines, {Blocks,...}},...}"})

Returns:

- DefinitionList element ([Block])

### Div {#pandoc.Div}

`Div (content[, attr])`

Creates a div element

Parameters:

`content`
:   block content ([Blocks])

`attr`
:   element attributes ([Attr])

Returns:

- Div element ([Block])

### Figure {#pandoc.Figure}

`Figure (content[, caption[, attr]])`

Creates a [Figure] element.

Parameters:

`content`
:   figure block content ([Blocks])

`caption`
:   figure caption ([Caption])

`attr`
:   element attributes ([Attr])

Returns:

- Figure object ([Block])

### Header {#pandoc.Header}

`Header (level, content[, attr])`

Creates a header element.

Parameters:

`level`
:   heading level ([integer]{unknown-type="integer"})

`content`
:   inline content ([Inlines])

`attr`
:   element attributes ([Attr])

Returns:

- Header element ([Block])

### HorizontalRule {#pandoc.HorizontalRule}

`HorizontalRule ()`

Creates a horizontal rule.

Returns:

- HorizontalRule element ([Block])

### LineBlock {#pandoc.LineBlock}

`LineBlock (content)`

Creates a line block element.

Parameters:

`content`
:   lines ({[Inlines],\...})

Returns:

- LineBlock element ([Block])

### OrderedList {#pandoc.OrderedList}

`OrderedList (items[, listAttributes])`

Creates an ordered list.

Parameters:

`items`
:   list items ({[Blocks],\...})

`listAttributes`
:   list parameters ([ListAttributes])

Returns:

- OrderedList element ([Block])

### Para {#pandoc.Para}

`Para (content)`

Creates a para element.

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- Para element ([Block])

### Plain {#pandoc.Plain}

`Plain (content)`

Creates a plain element.

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- Plain element ([Block])

### RawBlock {#pandoc.RawBlock}

`RawBlock (format, text)`

Creates a raw content block of the specified format.

Parameters:

`format`
:   format of content (string)

`text`
:   raw content (string)

Returns:

- RawBlock element ([Block])

### Table {#pandoc.Table}

`Table (caption, colspecs, head, bodies, foot[, attr])`

Creates a table element.

Parameters:

`caption`
:   table caption ([Caption])

`colspecs`
:   column alignments and widths ({[ColSpec],\...})

`head`
:   table head ([TableHead])

`bodies`
:   table bodies ({[TableBody],\...})

`foot`
:   table foot ([TableFoot])

`attr`
:   element attributes ([Attr])

Returns:

- Table element ([Block])

### Blocks {#pandoc.Blocks}

`Blocks (block_like_elements)`

Creates a [Blocks] list.

Parameters:

`block_like_elements`
:   List where each element can be treated as a [Block] value, or
    a single such value. ([Blocks])

Returns:

- list of block elements ([Blocks])

### Cite {#pandoc.Cite}

`Cite (content, citations)`

Creates a Cite inline element

Parameters:

`content`
:   placeholder content ([Inlines])

`citations`
:   List of Citations ({[Citation]{unknown-type="Citation"},\...})

Returns:

- cite element ([Inline])

### Code {#pandoc.Code}

`Code (code[, attr])`

Creates a Code inline element

Parameters:

`code`
:   code string (string)

`attr`
:   additional attributes ([Attr])

Returns:

- code element ([Inline])

### Emph {#pandoc.Emph}

`Emph (content)`

Creates an inline element representing emphasized text.

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### Image {#pandoc.Image}

`Image (caption, src[, title[, attr]])`

Creates an Image element

Parameters:

`caption`
:   text used to describe the image ([Inlines])

`src`
:   path to the image file (string)

`title`
:   brief image description (string)

`attr`
:   image attributes ([Attr])

Returns:

- Image element ([Inline])

### LineBreak {#pandoc.LineBreak}

`LineBreak ()`

Create a LineBreak inline element

Returns:

- line break ([Inline])

### Link {#pandoc.Link}

`Link (content, target[, title[, attr]])`

Creates a link inline element, usually a hyperlink.

Parameters:

`content`
:   text for this link ([Inlines])

`target`
:   the link target (string)

`title`
:   brief link description (string)

`attr`
:   link attributes ([Attr])

Returns:

- link element ([Inline])

### Math {#pandoc.Math}

`Math (mathtype, text)`

Creates a Math element, either inline or displayed.

Parameters:

`mathtype`
:   rendering specifier ([MathType]{unknown-type="MathType"})

`text`
:   math content (string)

Returns:

- math element ([Inline])

### Note {#pandoc.Note}

`Note (content)`

Creates a Note inline element

Parameters:

`content`
:   footnote block content ([Blocks])

Returns:

- note ([Inline])

### Quoted {#pandoc.Quoted}

`Quoted (quotetype, content)`

Creates a Quoted inline element given the quote type and quoted
content.

Parameters:

`quotetype`
:   type of quotes ([QuoteType]{unknown-type="QuoteType"})

`content`
:   inlines in quotes ([Inlines])

Returns:

- quoted element ([Inline])

### RawInline {#pandoc.RawInline}

`RawInline (format, text)`

Creates a raw inline element

Parameters:

`format`
:   format of content (string)

`text`
:   string content (string)

Returns:

- raw inline element ([Inline])

### SmallCaps {#pandoc.SmallCaps}

`SmallCaps (content)`

Creates text rendered in small caps

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### SoftBreak {#pandoc.SoftBreak}

`SoftBreak ()`

Creates a SoftBreak inline element.

Returns:

- soft break ([Inline])

### Space {#pandoc.Space}

`Space ()`

Create a Space inline element

Returns:

- new space ([Inline])

### Span {#pandoc.Span}

`Span (content[, attr])`

Creates a Span inline element

Parameters:

`content`
:   inline content ([Inlines])

`attr`
:   additional attributes ([Attr])

Returns:

- [Span] object ([Inline])

### Str {#pandoc.Str}

`Str (text)`

Creates a Str inline element

Parameters:

`text`
:    (string)

Returns:

- [Str] object ([Inline])

### Strikeout {#pandoc.Strikeout}

`Strikeout (content)`

Creates text which is struck out.

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### Strong {#pandoc.Strong}

`Strong (content)`

Creates a Strong element, whose text is usually displayed in a
bold font.

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### Subscript {#pandoc.Subscript}

`Subscript (content)`

Creates a Subscript inline element

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### Superscript {#pandoc.Superscript}

`Superscript (content)`

Creates a Superscript inline element

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### Underline {#pandoc.Underline}

`Underline (content)`

Creates an Underline inline element

Parameters:

`content`
:   inline content ([Inlines])

Returns:

- new object ([Inline])

### Inlines {#pandoc.Inlines}

`Inlines (inline_like_elements)`

Converts its argument into an [Inlines] list:

- copies a list of [Inline] elements into a fresh list; any string
  `s` within the list is treated as `pandoc.Str(s)`;
- turns a single [Inline] into a singleton list;
- splits a string into `Str`-wrapped words, treating interword
  spaces as `Space`s or `SoftBreak`s.

Parameters:

`inline_like_elements`
:   List where each element can be treated as an [Inline] value,
    or just a single such value. ([Inlines])

Returns:

- list of inline elements ([Inlines])

### Attr {#pandoc.Attr}

`Attr ([identifier[, classes[, attributes]]])`

Create a new set of attributes

Parameters:

`identifier`
:   element identifier (string\|table\|[Attr])

`classes`
:   element classes ({string,\...})

`attributes`
:   table containing string keys and values
    (table\|[AttributeList])

Returns:

- new Attr object ([Attr])

### Caption {#pandoc.Caption}

`Caption ([long[, short]])`

Creates a new Caption object.

Parameters:

`long`
:   full caption ([Blocks])

`short`
:   short summary caption ([Inlines])

Returns:

- new Caption object ([Caption])

*Since: 3.6.1*

### Cell {#pandoc.Cell}

`Cell (blocks[, align[, rowspan[, colspan[, attr]]]])`

Create a new table cell.

Parameters:

`blocks`
:   cell contents ([Blocks])

`align`
:   text alignment; defaults to `AlignDefault` ([Alignment])

`rowspan`
:   number of rows occupied by the cell; defaults to `1`
    ([integer]{unknown-type="integer"})

`colspan`
:   number of columns occupied by the cell; defaults to `1`
    ([integer]{unknown-type="integer"})

`attr`
:   cell attributes ([Attr])

Returns:

- new Cell object ([Cell])

### AttributeList {#pandoc.AttributeList}

`AttributeList (attribs)`

Parameters:

`attribs`
:   an attribute list (table\|[AttributeList])

Returns:

- new AttributeList object ([AttributeList])

### Citation {#pandoc.Citation}

`Citation (id, mode[, prefix[, suffix[, note_num[, hash]]]])`

Creates a single citation.

Parameters:

`id`
:   citation ID (e.g. BibTeX key) (string)

`mode`
:   citation rendering mode
    ([CitationMode]{unknown-type="CitationMode"})

`prefix`
:    ([Inlines])

`suffix`
:    ([Inlines])

`note_num`
:   note number ([integer]{unknown-type="integer"})

`hash`
:   hash number ([integer]{unknown-type="integer"})

Returns:

- new citation object ([Citation]{unknown-type="Citation"})

### ListAttributes {#pandoc.ListAttributes}

`ListAttributes ([start[, style[, delimiter]]])`

Creates a new ListAttributes object.

Parameters:

`start`
:   number of the first list item
    ([integer]{unknown-type="integer"})

`style`
:   style used for list numbering (string)

`delimiter`
:   delimiter of list numbers (string)

Returns:

- new ListAttributes object ([ListAttributes])

### Row {#pandoc.Row}

`Row ([cells[, attr]])`

Creates a table row.

Parameters:

`cells`
:   list of table cells in this row ({[Cell],\...})

`attr`
:   row attributes ([Attr])

Returns:

- new Row object ([Row])

### TableFoot {#pandoc.TableFoot}

`TableFoot ([rows[, attr]])`

Creates a table foot.

Parameters:

`rows`
:   list of table rows ({[Row],\...})

`attr`
:   table foot attributes ([Attr])

Returns:

- new TableFoot object ([TableFoot])

### TableHead {#pandoc.TableHead}

`TableHead ([rows[, attr]])`

Creates a table head.

Parameters:

`rows`
:   list of table rows ({[Row],\...})

`attr`
:   table head attributes ([Attr])

Returns:

- new TableHead object ([TableHead])

### SimpleTable {#pandoc.SimpleTable}

`SimpleTable (caption, align, widths, header, rows)`

Usage:

    local caption = "Overview"
    local aligns = {pandoc.AlignDefault, pandoc.AlignDefault}
    local widths = {0, 0} -- let pandoc determine col widths
    local headers = {{pandoc.Plain({pandoc.Str "Language"})},
                     {pandoc.Plain({pandoc.Str "Typing"})}}
    local rows = {
      {{pandoc.Plain "Haskell"}, {pandoc.Plain "static"}},
      {{pandoc.Plain "Lua"}, {pandoc.Plain "Dynamic"}},
    }
    simple_table = pandoc.SimpleTable(
      caption,
      aligns,
      widths,
      headers,
      rows
    )

Parameters:

`caption`
:   table caption ([Inlines])

`align`
:   column alignments ({[Alignment],\...})

`widths`
:   relative column widths ({number,\...})

`header`
:   table header row ({[Blocks],\...})

`rows`
:   table rows ({{[Blocks],\...},\...})

Returns:

- new SimpleTable object ([SimpleTable])

<!-- END: AUTOGENERATED CONTENT -->

## Constants

[`AuthorInText`]{#pandoc.authorintext}

:   Author name is mentioned in the text.

    See also: [Citation](#type-citation)

[`SuppressAuthor`]{#pandoc.suppressauthor}

:   Author name is suppressed.

    See also: [Citation](#type-citation)

[`NormalCitation`]{#pandoc.normalcitation}

:   Default citation style is used.

    See also: [Citation](#type-citation)

[`DisplayMath`]{#pandoc.displaymath}

:   Math style identifier, marking that the formula should be show
    in "display" style, i.e., on a separate line.

    See also: [Math](#type-math)

[`InlineMath`]{#pandoc.inlinemath}

:   Math style identifier, marking that the formula should be show
    inline.

    See also: [Math](#type-math)

[`SingleQuote`]{#pandoc.singlequote}

:   Quote type used with [Quoted](#type-quoted), indicating
    that the string is enclosed in *single* quotes.

    See also: [Quoted](#type-quoted)

[`DoubleQuote`]{#pandoc.doublequote}

:   Quote type used with [Quoted](#type-quoted), indicating
    that the string is enclosed in *double* quotes.

    See also: [Quoted](#type-quoted)

[`AlignLeft`]{#pandoc.alignleft}

:   Table cells aligned left.

    See also: [Table](#type-alignment)

[`AlignRight`]{#pandoc.alignright}

:   Table cells right-aligned.

    See also: [Table](#type-alignment)

[`AlignCenter`]{#pandoc.aligncenter}

:   Table cell content is centered.

    See also: [Table](#type-alignment)

[`AlignDefault`]{#pandoc.aligndefault}

:   Table cells are alignment is unaltered.

    See also: [Table](#type-alignment)

[`DefaultDelim`]{#pandoc.defaultdelim}

:   Default list number delimiters are used.

    See also: [ListAttributes](#type-listattributes)

[`Period`]{#pandoc.period}

:   List numbers are delimited by a period.

    See also: [ListAttributes](#type-listattributes)

[`OneParen`]{#pandoc.oneparen}

:   List numbers are delimited by a single parenthesis.

    See also: [ListAttributes](#type-listattributes)

[`TwoParens`]{#pandoc.twoparens}

:   List numbers are delimited by a double parentheses.

    See also: [ListAttributes](#type-listattributes)

[`DefaultStyle`]{#pandoc.defaultstyle}

:   List are numbered in the default style

    See also: [ListAttributes](#type-listattributes)

[`Example`]{#pandoc.example}

:   List items are numbered as examples.

    See also: [ListAttributes](#type-listattributes)

[`Decimal`]{#pandoc.decimal}

:   List are numbered using decimal integers.

    See also: [ListAttributes](#type-listattributes)

[`LowerRoman`]{#pandoc.lowerroman}

:   List are numbered using lower-case roman numerals.

    See also: [ListAttributes](#type-listattributes)

[`UpperRoman`]{#pandoc.upperroman}

:   List are numbered using upper-case roman numerals

    See also: [ListAttributes](#type-listattributes)

[`LowerAlpha`]{#pandoc.loweralpha}

:   List are numbered using lower-case alphabetic characters.

    See also: [ListAttributes](#type-listattributes)

[`UpperAlpha`]{#pandoc.upperalpha}

:   List are numbered using upper-case alphabetic characters.

    See also: [ListAttributes](#type-listattributes)

[`sha1`]{#pandoc.sha1}

:   Alias for [`pandoc.utils.sha1`](#pandoc.utils.sha1)
    (DEPRECATED, use `pandoc.utils.sha1` instead).

## Other constructors

### `ReaderOptions (opts)` {#pandoc.readeroptions}

Creates a new [ReaderOptions] value.

Parameters

`opts`
:   Either a table with a subset of the properties of a
    [ReaderOptions] object, or another ReaderOptions object.
    Uses the defaults specified in the manual for all
    properties that are not explicitly specified. Throws an
    error if a table contains properties which are not present
    in a ReaderOptions object. ([ReaderOptions]|table)

Returns: new [ReaderOptions] object

Usage:

    -- copy of the reader options that were defined on the command line.
    local cli_opts = pandoc.ReaderOptions(PANDOC_READER_OPTIONS)

    -- default reader options, but columns set to 66.
    local short_colums_opts = pandoc.ReaderOptions {columns = 66}

### `WriterOptions (opts)` {#pandoc.writeroptions}

Creates a new [WriterOptions][] value.

Parameters

`opts`
:   Either a table with a subset of the properties of a
    [WriterOptions] object, or another WriterOptions object.
    Uses the defaults specified in the manual for all
    properties that are not explicitly specified. Throws an
    error if a table contains properties which are not present
    in a WriterOptions object. ([WriterOptions]|table)

Returns: new [WriterOptions] object

Usage:

    -- copy of the writer options that were defined on the command line.
    local cli_opts = pandoc.WriterOptions(PANDOC_WRITER_OPTIONS)

    -- default writer options, but DPI set to 300.
    local short_colums_opts = pandoc.WriterOptions {dpi = 300}

## Helper functions

### `pipe (command, args, input)` {#pandoc.pipe}

Runs command with arguments, passing it some input, and returns
the output.

Parameters:

`command`
:   program to run; the executable will be resolved using default
    system methods (string).

`args`
:   list of arguments to pass to the program (list of strings).

`input`
:   data which is piped into the program via stdin (string).

Returns:

-   Output of command, i.e. data printed to stdout (string)

Raises:

-   A table containing the keys `command`, `error_code`, and
    `output` is thrown if the command exits with a non-zero
    error code.

Usage:

    local output = pandoc.pipe("sed", {"-e","s/a/b/"}, "abc")

### `walk_block (element, filter)` {#pandoc.walk_block}

Apply a filter inside a block element, walking its contents.
Returns a (deep) copy on which the filter has been applied:
the original element is left untouched.

Parameters:

`element`
:   the block element

`filter`
:   a Lua filter (table of functions) to be applied within the
    block element

Returns: the transformed block element

### `walk_inline (element, filter)` {#pandoc.walk_inline}

Apply a filter inside an inline element, walking its contents.
Returns a (deep) copy on which the filter has been applied:
the original element is left untouched.

Parameters:

`element`
:   the inline element

`filter`
:   a Lua filter (table of functions) to be applied within the
    inline element

Returns: the transformed inline element

### `with_state (options, callback)` {#pandoc.with_state}

Runs a function with a modified pandoc state.

The given callback is invoked after setting the pandoc state to the
given values. The modifiable options are restored to their original
values once the callback has returned.

The following state variables can be controlled:

  - `request_headers` (list of key-value tuples)
  - `resource_path` (list of filepaths)
  - `user_data_dir` (string)

Other options are ignored, and the rest of the state is not modified.

Usage:

    local opts = {
      request_headers = {
        {'Authorization', 'Basic my-secret'}
      }
    }
    pandoc.with_state(opts, function ()
      local mime, contents = pandoc.mediabag.fetch(image_url)
    )

### `read (markup[, format[, reader_options[, read_env]]])` {#pandoc.read}

Parse the given string into a Pandoc document.

The parser is run in the same environment that was used to read
the main input files; it has full access to the file-system and
the mediabag. This means that if the document specifies files to
be included, as is possible in formats like LaTeX,
reStructuredText, and Org, then these will be included in the
resulting document. Any media elements are added to those
retrieved from the other parsed input files. Use the `read_env`
parameter to modify this behavior.

The `format` parameter defines the format flavor that will be
parsed. This can be either a string, using `+` and `-` to enable
and disable extensions, or a table with fields `format` (string)
and `extensions` (table). The `extensions` table can be a list of
all enabled extensions, or a table with extensions as keys and
their activation status as values (`true` or `'enable'` to enable
an extension, `false` or `'disable'` to disable it).

Note: The extensions field in `reader_options` is ignored, as the
function will always use the format extensions specified via the
`format` parameter.

Parameters:

`markup`
:   the markup to be parsed (string|Sources)

`format`
:   format specification; defaults to `"markdown"`. See the
    description above for a complete description of this
    parameter. (string|table)

`reader_options`
:   options passed to the reader; may be a ReaderOptions object or
    a table with a subset of the keys and values of a
    ReaderOptions object; defaults to the default values
    documented in the manual. ([ReaderOptions]|table)

`read_env`
:   If the value is not given or `nil`, then the global
    environment is used. Passing a list of filenames causes the
    reader to be run in a sandbox. The given files are read from
    the file system and provided to the sandbox via an ersatz file
    system. The table can also contain mappings from filenames to
    contents, which will be used to populate the ersatz file
    system.

Returns: pandoc document ([Pandoc](#type-pandoc))

Usage:

    local org_markup = "/emphasis/"  -- Input to be read
    local document = pandoc.read(org_markup, "org")
    -- Get the first block of the document
    local block = document.blocks[1]
    -- The inline element in that block is an `Emph`
    assert(block.content[1].t == "Emph")

[ReaderOptions]: #type-readeroptions

### `write (doc[, format[, writer_options]])` {#pandoc.write}

Converts a document to the given target format.

Note: The extensions field in `writer_options` is ignored, as the
function will always use the format extensions specified via the
`format` parameter.

Parameters:

`doc`
:   document to convert ([Pandoc](#type-pandoc))

`format`
:   format specification; defaults to `"html"`. See the
    documentation of [`pandoc.read`](#pandoc.read) for a complete
    description of this parameter. (string|table)

`writer_options`
:   options passed to the writer; may be a WriterOptions object
    or a table with a subset of the keys and values of a
    WriterOptions object; defaults to the default values
    documented in the manual. ([WriterOptions]|table)

Returns:
-   converted document (string)

Usage:

    local doc = pandoc.Pandoc(
      {pandoc.Para {pandoc.Strong 'Tea'}}
    )
    local html = pandoc.write(doc, 'html')
    assert(html == "<p><strong>Tea</strong></p>")

### `write_classic (doc[, writer_options])` {#pandoc.write_custom}

Runs a classic custom Lua writer, using the functions defined
in the current environment.

Parameters:

`doc`
:   document to convert ([Pandoc](#type-pandoc))

`writer_options`
:   options passed to the writer; may be a [WriterOptions] object
    or a table with a subset of the keys and values of a
    WriterOptions object; defaults to the default values
    documented in the manual. ([WriterOptions]|table)

Returns:
-   converted document (string)

Usage:

    -- Adding this function converts a classic writer into a
    -- new-style custom writer.
    function Writer (doc, opts)
      PANDOC_DOCUMENT = doc
      PANDOC_WRITER_OPTIONS = opts
      loadfile(PANDOC_SCRIPT_FILE)()
      return pandoc.write_classic(doc, opts)
    end

[WriterOptions]: #type-writeroptions

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.cli -->

# Module pandoc.cli

Command line options and argument parsing.

## Fields {#pandoc.cli-fields}

### default_options {#pandoc.cli.default_options}

Default CLI options, using a JSON-like representation. (table)

## Functions {#pandoc.cli-functions}

### parse_options {#pandoc.cli.parse_options}

`parse_options (args)`

Parses command line arguments into pandoc options. Typically this
function will be used in stand-alone pandoc Lua scripts, taking
the list of arguments from the global `arg`.

Parameters:

`args`
:   list of command line arguments ({string,\...})

Returns:

- parsed options, using their JSON-like representation. (table)

*Since: 3.0*

### repl {#pandoc.cli.repl}

`repl ([env])`

Starts a read-eval-print loop (REPL). The function returns all
values of the last evaluated input. Exit the REPL by pressing
`ctrl-d` or `ctrl-c`; press `F1` to get a list of all key
bindings.

The REPL is started in the global namespace, unless the `env`
parameter is specified. In that case, the global namespace is
merged into the given table and the result is used as `_ENV` value
for the repl.

Specifically, local variables *cannot* be accessed, unless they
are explicitly passed via the `env` parameter; e.g.

    function Pandoc (doc)
      -- start repl, allow to access the `doc` parameter
      -- in the repl
      return pandoc.cli.repl{ doc = doc }
    end

**Note**: it seems that the function exits immediately on Windows,
without prompting for user input.

Parameters:

`env`
:   Extra environment; the global environment is merged into this
    table. (table)

Returns:

The result(s) of the last evaluated input, or nothing if the last
input resulted in an error.

*Since: 3.1.2*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.utils -->

# Module pandoc.utils

This module exposes internal pandoc functions and utility
functions.

## Functions {#pandoc.utils-functions}

### blocks_to_inlines {#pandoc.utils.blocks_to_inlines}

`blocks_to_inlines (blocks[, sep])`

Squash a list of blocks into a list of inlines.

Usage

    local blocks = {
      pandoc.Para{ pandoc.Str 'Paragraph1' },
      pandoc.Para{ pandoc.Emph 'Paragraph2' }
    }
    local inlines = pandoc.utils.blocks_to_inlines(blocks)
    assert(
      inlines == pandoc.Inlines {
        pandoc.Str 'Paragraph1',
        pandoc.Linebreak(),
        pandoc.Emph{ pandoc.Str 'Paragraph2' }
      }
    )

Parameters:

`blocks`
:   List of [Block] elements to be flattened. ([Blocks])

`sep`
:   List of [Inline] elements inserted as separator between two
    consecutive blocks; defaults to `{pandoc.LineBreak()}`.
    ([Inlines])

Returns:

-  ([Inlines])

*Since: 2.2.3*

### citeproc {#pandoc.utils.citeproc}

`citeproc (doc)`

Process the citations in the file, replacing them with rendered
citations and adding a bibliography. See the manual section on
citation rendering for details.

Usage:

    -- Lua filter that behaves like `--citeproc`
    function Pandoc (doc)
      return pandoc.utils.citeproc(doc)
    end

Parameters:

`doc`
:   document ([Pandoc])

Returns:

- processed document ([Pandoc])

*Since: 2.19.1*

### equals {#pandoc.utils.equals}

`equals (element1, element2)`

Test equality of AST elements. Elements in Lua are considered
equal if and only if the objects obtained by unmarshaling are
equal.

**This function is deprecated.** Use the normal Lua `==` equality
operator instead.

Parameters:

`element1`
:    (any)

`element2`
:    (any)

Returns:

- Whether the two objects represent the same element (boolean)

*Since: 2.5*

### from_simple_table {#pandoc.utils.from_simple_table}

`from_simple_table (simple_tbl)`

Creates a [Table] block element from a [SimpleTable]. This is
useful for dealing with legacy code which was written for pandoc
versions older than 2.10.

Usage:

    local simple = pandoc.SimpleTable(table)
    -- modify, using pre pandoc 2.10 methods
    simple.caption = pandoc.SmallCaps(simple.caption)
    -- create normal table block again
    table = pandoc.utils.from_simple_table(simple)

Parameters:

`simple_tbl`
:    ([SimpleTable])

Returns:

- table block element ([Block])

*Since: 2.11*

### make_sections {#pandoc.utils.make_sections}

`make_sections (number_sections, baselevel, blocks)`

Converts a list of [Block] elements into sections. `Div`s will be
created beginning at each `Header` and containing following
content until the next `Header` of comparable level. If
`number_sections` is true, a `number` attribute will be added to
each `Header` containing the section number. If `base_level` is
non-null, `Header` levels will be reorganized so that there are no
gaps, and so that the base level is the level specified.

Parameters:

`number_sections`
:   whether section divs should get an additional `number`
    attribute containing the section number. (boolean)

`baselevel`
:   shift top-level headings to this level
    ([integer]{unknown-type="integer"}\|nil)

`blocks`
:   list of blocks to process ([Blocks])

Returns:

- blocks with sections ([Blocks])

*Since: 2.8*

### normalize_date {#pandoc.utils.normalize_date}

`normalize_date (date)`

Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
limit years to the range 1601-9999 (ISO 8601 accepts greater than
or equal to 1583, but MS Word only accepts dates starting 1601).
Returns nil instead of a string if the conversion failed.

Parameters:

`date`
:   the date string (string)

Returns:

- normalized date, or nil if normalization failed. ([string or
  nil]{unknown-type="string or nil"})

*Since: 2.0.6*

### references {#pandoc.utils.references}

`references (doc)`

Get references defined inline in the metadata and via an external
bibliography. Only references that are actually cited in the
document (either with a genuine citation or with `nocite`) are
returned. URL variables are converted to links.

The structure used represent reference values corresponds to that
used in CSL JSON; the return value can be use as `references`
metadata, which is one of the values used by pandoc and citeproc
when generating bibliographies.

Usage:

    -- Include all cited references in document
    function Pandoc (doc)
      doc.meta.references = pandoc.utils.references(doc)
      doc.meta.bibliography = nil
      return doc
    end

Parameters:

`doc`
:   document ([Pandoc])

Returns:

- lift of references. (table)

*Since: 2.17*

### run_json_filter {#pandoc.utils.run_json_filter}

`run_json_filter (doc, filter[, args])`

Filter the given doc by passing it through a JSON filter.

Parameters:

`doc`
:   the Pandoc document to filter ([Pandoc])

`filter`
:   filter to run (string)

`args`
:   list of arguments passed to the filter. Defaults to
    `{FORMAT}`. ({string,\...})

Returns:

- filtered document ([Pandoc])

*Since: 2.1.1*

### run_lua_filter {#pandoc.utils.run_lua_filter}

`run_lua_filter (doc, filter[, env])`

Filter the given doc by passing it through a Lua filter.

The filter will be run in the current Lua process.

Parameters:

`doc`
:   the Pandoc document to filter ([Pandoc])

`filter`
:   filepath of the filter to run (string)

`env`
:   environment to load and run the filter in (table)

Returns:

- filtered document ([Pandoc])

*Since: 3.2.1*

### sha1 {#pandoc.utils.sha1}

`sha1 (input)`

Computes the SHA1 hash of the given string input.

Parameters:

`input`
:    (string)

Returns:

- hexadecimal hash value (string)

*Since: 2.0.6*

### stringify {#pandoc.utils.stringify}

`stringify (element)`

Converts the given element (Pandoc, Meta, Block, or Inline) into a
string with all formatting removed.

Parameters:

`element`
:   some pandoc AST element ([AST
    element]{unknown-type="AST element"})

Returns:

- A plain string representation of the given element. (string)

*Since: 2.0.6*

### to_roman_numeral {#pandoc.utils.to_roman_numeral}

`to_roman_numeral (n)`

Converts an integer \< 4000 to uppercase roman numeral.

Usage:

    local to_roman_numeral = pandoc.utils.to_roman_numeral
    local pandoc_birth_year = to_roman_numeral(2006)
    -- pandoc_birth_year == 'MMVI'

Parameters:

`n`
:   positive integer below 4000
    ([integer]{unknown-type="integer"})

Returns:

- A roman numeral. (string)

*Since: 2.0.6*

### to_simple_table {#pandoc.utils.to_simple_table}

`to_simple_table (tbl)`

Converts a table into an old/simple table.

Usage:

    local simple = pandoc.utils.to_simple_table(table)
    -- modify, using pre pandoc 2.10 methods
    simple.caption = pandoc.SmallCaps(simple.caption)
    -- create normal table block again
    table = pandoc.utils.from_simple_table(simple)

Parameters:

`tbl`
:   a table ([Block])

Returns:

- SimpleTable object ([SimpleTable])

*Since: 2.11*

### type {#pandoc.utils.type}

`type (value)`

Pandoc-friendly version of Lua's default `type` function,
returning type information similar to what is presented in the
manual.

The function works by checking the metafield `__name`. If the
argument has a string-valued metafield `__name`, then it returns
that string. Otherwise it behaves just like the normal `type`
function.

Usage:

    -- Prints one of 'string', 'boolean', 'Inlines', 'Blocks',
    -- 'table', and 'nil', corresponding to the Haskell constructors
    -- MetaString, MetaBool, MetaInlines, MetaBlocks, MetaMap,
    -- and an unset value, respectively.

    function Meta (meta)
      print('type of metavalue `author`:', pandoc.utils.type(meta.author))
    end

Parameters:

`value`
:   any Lua value (any)

Returns:

- type of the given value (string)

*Since: 2.17*

### Version {#pandoc.utils.Version}

`Version (v)`

Creates a Version object.

Parameters:

`v`
:   version description ([version string, list of integers, or
    integer]{unknown-type="version string, list of integers, or integer"})

Returns:

- new Version object ([Version])

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.mediabag -->

# Module pandoc.mediabag

The `pandoc.mediabag` module allows accessing pandoc's media
storage. The "media bag" is used when pandoc is called with the
`--extract-media` or (for HTML only) `--embed-resources` option.

The module is loaded as part of module `pandoc` and can either be
accessed via the `pandoc.mediabag` field, or explicitly required,
e.g.:

    local mb = require 'pandoc.mediabag'

## Functions {#pandoc.mediabag-functions}

### delete {#pandoc.mediabag.delete}

`delete (filepath)`

Removes a single entry from the media bag.

Parameters:

`filepath`
:   Filename of the item to deleted. The media bag will be left
    unchanged if no entry with the given filename exists. (string)

*Since: 2.7.3*

### empty {#pandoc.mediabag.empty}

`empty ()`

Clear-out the media bag, deleting all items.

*Since: 2.7.3*

### fetch {#pandoc.mediabag.fetch}

`fetch (source)`

Fetches the given source from a URL or local file. Returns two
values: the contents of the file and the MIME type (or an empty
string).

The function will first try to retrieve `source` from the
mediabag; if that fails, it will try to download it or read it
from the local file system while respecting pandoc's "resource
path" setting.

Usage:

    local diagram_url = 'https://pandoc.org/diagram.jpg'
    local mt, contents = pandoc.mediabag.fetch(diagram_url)

Parameters:

`source`
:   path to a resource; either a local file path or URI (string)

Returns:

- The entry's MIME type, or `nil` if the file was not found.
  (string)
- Contents of the file, or `nil` if the file was not found.
  (string)

*Since: 2.0*

### fill {#pandoc.mediabag.fill}

`fill (doc)`

Fills the mediabag with the images in the given document. An image
that cannot be retrieved will be replaced with a Span of class
"image" that contains the image description.

Images for which the mediabag already contains an item will not be
processed again.

Parameters:

`doc`
:   document from which to fill the mediabag ([Pandoc])

Returns:

- modified document ([Pandoc])

*Since: 2.19*

### insert {#pandoc.mediabag.insert}

`insert (filepath, mimetype, contents)`

Adds a new entry to pandoc's media bag. Replaces any existing
media bag entry the same `filepath`.

Usage:

    local fp = 'media/hello.txt'
    local mt = 'text/plain'
    local contents = 'Hello, World!'
    pandoc.mediabag.insert(fp, mt, contents)

Parameters:

`filepath`
:   filename and path relative to the output folder. (string)

`mimetype`
:   the item's MIME type; omit if unknown or unavailable. (string)

`contents`
:   the binary contents of the file. (string)

*Since: 2.0*

### items {#pandoc.mediabag.items}

`items ()`

Returns an iterator triple to be used with Lua's generic `for`
statement. The iterator returns the filepath, MIME type, and
content of a media bag item on each invocation. Items are
processed one-by-one to avoid excessive memory use.

This function should be used only when full access to all items,
including their contents, is required. For all other cases,
[`list`] should be preferred.

Usage:

    for fp, mt, contents in pandoc.mediabag.items() do
      -- print(fp, mt, contents)
    end

Returns:

Iterator triple:

- The iterator function; must be called with the iterator state
  and the current iterator value.
- Iterator state -- an opaque value to be passed to the iterator
  function.
- Initial iterator value.

*Since: 2.7.3*

### list {#pandoc.mediabag.list}

`list ()`

Get a summary of the current media bag contents.

Usage:

    -- calculate the size of the media bag.
    local mb_items = pandoc.mediabag.list()
    local sum = 0
    for i = 1, #mb_items do
        sum = sum + mb_items[i].length
    end
    print(sum)

Returns:

- A list of elements summarizing each entry in the media bag. The
  summary item contains the keys `path`, `type`, and `length`,
  giving the filepath, MIME type, and length of contents in bytes,
  respectively. (table)

*Since: 2.0*

### lookup {#pandoc.mediabag.lookup}

`lookup (filepath)`

Lookup a media item in the media bag, and return its MIME type and
contents.

Usage:

    local filename = 'media/diagram.png'
    local mt, contents = pandoc.mediabag.lookup(filename)

Parameters:

`filepath`
:   name of the file to look up. (string)

Returns:

- The entry's MIME type, or nil if the file was not found.
  (string)
- Contents of the file, or nil if the file was not found. (string)

*Since: 2.0*

### make_data_uri {#pandoc.mediabag.make_data_uri}

`make_data_uri (mime_type, raw_data)`

Convert the input data into a data URI as defined by RFC 2397.

Example:

    -- Embed an unofficial pandoc logo
    local pandoc_logo_url = 'https://raw.githubusercontent.com/'
      .. 'tarleb/pandoc-logo/main/pandoc.svg'

    local datauri = pandoc.mediabag.make_data_uri(
      pandoc.mediabag.fetch(pandoc_logo_url)
    )

    local image = pandoc.Image('Logo', datauri)

Parameters:

`mime_type`
:   MIME type of the data (string)

`raw_data`
:   data to encode (string)

Returns:

- data uri (string)

*Since: 3.7.1*

### write {#pandoc.mediabag.write}

`write (dir[, fp])`

Writes the contents of mediabag to the given target directory. If
`fp` is given, then only the resource with the given name will be
extracted. Omitting that parameter means that the whole mediabag
gets extracted. An error is thrown if `fp` is given but cannot be
found in the mediabag.

Parameters:

`dir`
:   path of the target directory (string)

`fp`
:   canonical name (relative path) of resource (string)

*Since: 3.0*

<!-- END: AUTOGENERATED CONTENT -->

# Module pandoc.List

This module defines pandoc's list type. It comes with useful
methods and convenience functions.

## Constructor

[`pandoc.List([table])`]{#pandoc.list}

:   Create a new List. If the optional argument `table` is given,
    set the metatable of that value to `pandoc.List`. This is an
    alias for [`pandoc.List:new([table])`](#pandoc.list:new).

## Metamethods

### `pandoc.List:__concat (list)` {#pandoc.list:__concat}

Concatenates two lists.

Parameters:

`list`
:   second list concatenated to the first

Returns: a new list containing all elements from list1 and
list2

### `pandoc.List:__eq (a, b)` {#pandoc.list:__eq}

Compares two lists for equality. The lists are taken as equal
if and only if they are of the same type (i.e., have the same
non-nil metatable), have the same length, and if all elements
are equal.

Parameters:

`a`, `b`
:   any Lua object

Returns:

-   `true` if the two lists are equal, `false` otherwise.

## Methods

### `pandoc.List:at` {#pandoc.list:at}

`:at (index[, default])`

Returns the element at the given index, or `default` if the list
contains no item at the given position.

Negative integers count back from the last item in the list.

Parameters:

`index`
:   element position (integer)

`default`
:   the default value that is returned if the index is out of
    range (any)

Returns:

-   the list item at `index`, or `default`.

### `pandoc.List:clone ()` {#pandoc.list:clone}

Returns a (shallow) copy of the list. (To get a deep copy
of the list, use `walk` with an empty filter.)

### `pandoc.List:extend (list)` {#pandoc.list:extend}

Adds the given list to the end of this list.

Parameters:

`list`
:   list to appended

### `pandoc.List:find (needle, init)` {#pandoc.list:find}

Returns the value and index of the first occurrence of the
given item.

Parameters:

`needle`
:   item to search for

`init`
:   index at which the search is started

Returns: first item equal to the needle, or nil if no such
item exists.

### `pandoc.List:find_if (pred, init)` {#pandoc.list:find_if}

Returns the value and index of the first element for which
the predicate holds true.

Parameters:

`pred`
:   the predicate function

`init`
:   index at which the search is started

Returns: first item for which \`test\` succeeds, or nil if
no such item exists.

### `pandoc.List:filter (pred)` {#pandoc.list:filter}

Returns a new list containing all items satisfying a given
condition.

Parameters:

`pred`
:   condition items must satisfy.

Returns: a new list containing all items for which \`test\`
was true.

### `pandoc.List:includes (needle, init)` {#pandoc.list:includes}

Checks if the list has an item equal to the given needle.

Parameters:

`needle`
:   item to search for

`init`
:   index at which the search is started

Returns: true if a list item is equal to the needle, false
otherwise

### `pandoc.List:insert ([pos], value)` {#pandoc.list:insert}

Inserts element `value` at position `pos` in list, shifting
elements to the next-greater index if necessary.

This function is identical to
[`table.insert`](https://www.lua.org/manual/5.4/manual.html#6.6).

Parameters:

`pos`
:   index of the new value; defaults to length of the list + 1

`value`
:   value to insert into the list

### `pandoc.List:iter ([step])` {#pandoc.list:iter}

Create an iterator over the list. The resulting function returns the
next value each time it is called.

Usage:

    for item in List{1, 1, 2, 3, 5, 8}:iter() do
      -- process item
    end

Parameters:

`step`
:   step width with which to step through the list. Negative step sizes
    will cause the iterator to start from the end of the list. Defaults
    to 1. (integer)

Returns:

-   iterator (function)

### `pandoc.List:map (fn)` {#pandoc.list:map}

Returns a copy of the current list by applying the given
function to all elements.

Parameters:

`fn`
:   function which is applied to all list items.

### `pandoc.List:new([table])` {#pandoc.list:new}

Create a new List. If the optional argument `table` is given,
set the metatable of that value to `pandoc.List`.

The function also accepts an iterator, in which case it creates a
new list from the return values of the iterator function.

Parameters:

`table`
:   table which should be treatable as a list; defaults to an
    empty table

Returns: the updated input value

### `pandoc.List:remove ([pos])` {#pandoc.list:remove}

Removes the element at position `pos`, returning the value
of the removed element.

This function is identical to
[`table.remove`](https://www.lua.org/manual/5.4/manual.html#6.6).

Parameters:

`pos`
:   position of the list value that will be removed; defaults
    to the index of the last element

Returns: the removed element

### `pandoc.List:sort ([comp])` {#pandoc.list:sort}

Sorts list elements in a given order, in-place. If `comp` is
given, then it must be a function that receives two list
elements and returns true when the first element must come
before the second in the final order (so that, after the
sort, `i < j` implies `not comp(list[j],list[i]))`. If comp
is not given, then the standard Lua operator `<` is used
instead.

Note that the comp function must define a strict partial
order over the elements in the list; that is, it must be
asymmetric and transitive. Otherwise, no valid sort may be
possible.

The sort algorithm is not stable: elements considered equal
by the given order may have their relative positions changed
by the sort.

This function is identical to
[`table.sort`](https://www.lua.org/manual/5.4/manual.html#6.6).

Parameters:

`comp`
:   Comparison function as described above.

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.format -->

# Module pandoc.format

Information about the formats supported by pandoc.

## Functions {#pandoc.format-functions}

### all_extensions {#pandoc.format.all_extensions}

`all_extensions (format)`

Returns the list of all valid extensions for a format. No
distinction is made between input and output; an extension can
have an effect when reading a format but not when writing it, or
*vice versa*.

Parameters:

`format`
:   format name (string)

Returns:

- all extensions supported for `format`
  ([FormatExtensions]{unknown-type="FormatExtensions"})

*Since: 3.0*

### default_extensions {#pandoc.format.default_extensions}

`default_extensions (format)`

Returns the list of default extensions of the given format; this
function does not check if the format is supported, it will return
a fallback list of extensions even for unknown formats.

Parameters:

`format`
:   format name (string)

Returns:

- default extensions enabled for `format`
  ([FormatExtensions]{unknown-type="FormatExtensions"})

*Since: 3.0*

### extensions {#pandoc.format.extensions}

`extensions (format)`

Returns the extension configuration for the given format. The
configuration is represented as a table with all supported
extensions as keys and their default status as value, with `true`
indicating that the extension is enabled by default, while `false`
marks a supported extension that's disabled.

This function can be used to assign a value to the `Extensions`
global in custom readers and writers.

Parameters:

`format`
:   format identifier (string)

Returns:

- extensions config (table)

*Since: 3.0*

### from_path {#pandoc.format.from_path}

`from_path (path)`

Parameters:

`path`
:   file path, or list of paths (string\|{string,\...})

Returns:

- format determined by heuristic (string\|nil)

*Since: 3.1.2*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.image -->

# Module pandoc.image

Basic image querying functions.

## Functions {#pandoc.image-functions}

### size {#pandoc.image.size}

`size (image[, opts])`

Returns a table containing the size and resolution of an image;
throws an error if the given string is not an image, or if the
size of the image cannot be determined.

The resulting table has four entries: *width*, *height*,
*dpi_horz*, and *dpi_vert*.

The `opts` parameter, when given, should be either a WriterOptions
object such as `PANDOC_WRITER_OPTIONS`, or a table with a `dpi`
entry. It affects the calculation for vector image formats such as
SVG.

Parameters:

`image`
:   image data (string)

`opts`
:   writer options ([WriterOptions]\|table)

Returns:

- image size information or error message (table)

*Since: 3.1.13*

### format {#pandoc.image.format}

`format (image)`

Returns the format of an image as a lowercase string.

Formats recognized by pandoc include *png*, *gif*, *tiff*, *jpeg*,
*pdf*, *svg*, *eps*, and *emf*.

Parameters:

`image`
:   binary image data (string)

Returns:

- image format, or nil if the format cannot be determined
  (string\|nil)

*Since: 3.1.13*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.json -->

# Module pandoc.json

JSON module to work with JSON; based on the Aeson Haskell package.

## Fields {#pandoc.json-fields}

### null {#pandoc.json.null}

Value used to represent the `null` JSON value. (light userdata)

## Functions {#pandoc.json-functions}

### decode {#pandoc.json.decode}

`decode (str[, pandoc_types])`

Creates a Lua object from a JSON string. If the input can be
decoded as representing an [Inline], [Block], [Pandoc], [Inlines],
or [Blocks] element the function will return an object of the
appropriate type. Otherwise, if the input does not represent any
of the AST types, the default decoding is applied: Objects and
arrays are represented as tables, the JSON `null` value becomes
[null], and JSON booleans, strings, and numbers are converted
using the Lua types of the same name.

The special handling of AST elements can be disabled by setting
`pandoc_types` to `false`.

Parameters:

`str`
:   JSON string (string)

`pandoc_types`
:   whether to use pandoc types when possible. (boolean)

Returns:

- decoded object (any)

*Since: 3.1.1*

### encode {#pandoc.json.encode}

`encode (object)`

Encodes a Lua object as JSON string.

If the object has a metamethod with name `__tojson`, then the
result is that of a call to that method with `object` passed as
the sole argument. The result of that call is expected to be a
valid JSON string, but this is not checked.

Parameters:

`object`
:   object to convert (any)

Returns:

- JSON encoding of the given `object` (string)

*Since: 3.1.1*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.log -->

# Module pandoc.log

Access to pandoc's logging system.

## Functions {#pandoc.log-functions}

### info {#pandoc.log.info}

`info (message)`

Reports a ScriptingInfo message to pandoc's logging system.

Parameters:

`message`
:   the info message (string)

*Since: 3.2*

### silence {#pandoc.log.silence}

`silence (fn)`

Applies the function to the given arguments while preventing log
messages from being added to the log. The warnings and info
messages reported during the function call are returned as the
first return value, with the results of the function call
following thereafter.

Parameters:

`fn`
:   function to be silenced (function)

Returns:

List of log messages triggered during the function call, and any
value returned by the function.

*Since: 3.2*

### warn {#pandoc.log.warn}

`warn (message)`

Reports a ScriptingWarning to pandoc's logging system. The warning
will be printed to stderr unless logging verbosity has been set to
*ERROR*.

Parameters:

`message`
:   the warning message (string)

*Since: 3.2*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.path -->

# Module pandoc.path

Module for file path manipulations.

## Fields {#pandoc.path-fields}

### separator {#pandoc.path.separator}

The character that separates directories. (string)

### search_path_separator {#pandoc.path.search_path_separator}

The character that is used to separate the entries in the `PATH`
environment variable. (string)

## Functions {#pandoc.path-functions}

### directory {#pandoc.path.directory}

`directory (filepath)`

Gets the directory name, i.e., removes the last directory
separator and everything after from the given path.

Parameters:

`filepath`
:   path (string)

Returns:

- The filepath up to the last directory separator. (string)

*Since: 2.12*

### exists {#pandoc.path.exists}

`exists (path[, type])`

Check whether there exists a filesystem object at the given path.
If `type` is given and either *directory* or *file*, then the
function returns `true` if and only if the file system object has
the given type, or if it's a symlink pointing to an object of that
type. Passing *symlink* as type requires the path itself to be a
symlink. Types other than those will cause an error.

Parameters:

`path`
:   file path to check (string)

`type`
:   the required type of the filesystem object (string)

Returns:

- whether a filesystem object of type `type` exists at `path`.
  (boolean)

*Since: 3.7.1*

### filename {#pandoc.path.filename}

`filename (filepath)`

Get the file name.

Parameters:

`filepath`
:   path (string)

Returns:

- File name part of the input path. (string)

*Since: 2.12*

### is_absolute {#pandoc.path.is_absolute}

`is_absolute (filepath)`

Checks whether a path is absolute, i.e. not fixed to a root.

Parameters:

`filepath`
:   path (string)

Returns:

- `true` iff `filepath` is an absolute path, `false` otherwise.
  (boolean)

*Since: 2.12*

### is_relative {#pandoc.path.is_relative}

`is_relative (filepath)`

Checks whether a path is relative or fixed to a root.

Parameters:

`filepath`
:   path (string)

Returns:

- `true` iff `filepath` is a relative path, `false` otherwise.
  (boolean)

*Since: 2.12*

### join {#pandoc.path.join}

`join (filepaths)`

Join path elements back together by the directory separator.

Parameters:

`filepaths`
:   path components ({string,\...})

Returns:

- The joined path. (string)

*Since: 2.12*

### make_relative {#pandoc.path.make_relative}

`make_relative (path, root[, unsafe])`

Contract a filename, based on a relative path. Note that the
resulting path will never introduce `..` paths, as the presence of
symlinks means `../b` may not reach `a/b` if it starts from `a/c`.
For a worked example see [this blog post].

Parameters:

`path`
:   path to be made relative (string)

`root`
:   root path (string)

`unsafe`
:   whether to allow `..` in the result. (boolean)

Returns:

- contracted filename (string)

*Since: 2.12*

### normalize {#pandoc.path.normalize}

`normalize (filepath)`

Normalizes a path.

- `//` makes sense only as part of a (Windows) network drive;
  elsewhere, multiple slashes are reduced to a single
  `path.separator` (platform dependent).
- `/` becomes `path.separator` (platform dependent).
- `./` is removed.
- an empty path becomes `.`

Parameters:

`filepath`
:   path (string)

Returns:

- The normalized path. (string)

*Since: 2.12*

### split {#pandoc.path.split}

`split (filepath)`

Splits a path by the directory separator.

Parameters:

`filepath`
:   path (string)

Returns:

- List of all path components. ({string,\...})

*Since: 2.12*

### split_extension {#pandoc.path.split_extension}

`split_extension (filepath)`

Splits the last extension from a file path and returns the parts.
The extension, if present, includes the leading separator; if the
path has no extension, then the empty string is returned as the
extension.

Parameters:

`filepath`
:   path (string)

Returns:

- filepath without extension (string)
- extension or empty string (string)

*Since: 2.12*

### split_search_path {#pandoc.path.split_search_path}

`split_search_path (search_path)`

Takes a string and splits it on the `search_path_separator`
character. Blank items are ignored on Windows, and converted to
`.` on Posix. On Windows path elements are stripped of quotes.

Parameters:

`search_path`
:   platform-specific search path (string)

Returns:

- list of directories in search path ({string,\...})

*Since: 2.12*

### treat_strings_as_paths {#pandoc.path.treat_strings_as_paths}

`treat_strings_as_paths ()`

Augment the string module such that strings can be used as path
objects.

*Since: 2.12*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.structure -->

# Module pandoc.structure

Access to the higher-level document structure, including
hierarchical sections and the table of contents.

## Functions {#pandoc.structure-functions}

### make_sections {#pandoc.structure.make_sections}

`make_sections (blocks[, opts])`

Puts [Blocks] into a hierarchical structure: a list of sections
(each a Div with class "section" and first element a Header).

The optional `opts` argument can be a table; two settings are
recognized: If `number_sections` is true, a `number` attribute
containing the section number will be added to each `Header`. If
`base_level` is an integer, then `Header` levels will be
reorganized so that there are no gaps, with numbering levels
shifted by the given value. Finally, an integer `slide_level`
value triggers the creation of slides at that heading level.

Note that a [WriterOptions] object can be passed as the opts
table; this will set the `number_section` and `slide_level` values
to those defined on the command line.

Usage:

    local blocks = {
      pandoc.Header(2, pandoc.Str 'first'),
      pandoc.Header(2, pandoc.Str 'second'),
    }
    local opts = PANDOC_WRITER_OPTIONS
    local newblocks = pandoc.structure.make_sections(blocks, opts)

Parameters:

`blocks`
:   document blocks to process ([Blocks]\|[Pandoc])

`opts`
:   options (table)

Returns:

- processed blocks ([Blocks])

*Since: 3.0*

### slide_level {#pandoc.structure.slide_level}

`slide_level (blocks)`

Find level of header that starts slides (defined as the least
header level that occurs before a non-header/non-hrule in the
blocks).

Parameters:

`blocks`
:   document body ([Blocks]\|[Pandoc])

Returns:

- slide level ([integer]{unknown-type="integer"})

*Since: 3.0*

### split_into_chunks {#pandoc.structure.split_into_chunks}

`split_into_chunks (doc[, opts])`

Converts a [Pandoc] document into a [ChunkedDoc].

Parameters:

`doc`
:   document to split ([Pandoc])

`opts`

:   Splitting options.

    The following options are supported:

        `path_template`
        :   template used to generate the chunks' filepaths
            `%n` will be replaced with the chunk number (padded with
            leading 0s to 3 digits), `%s` with the section number of
            the heading, `%h` with the (stringified) heading text,
            `%i` with the section identifier. For example,
            `"section-%s-%i.html"` might be resolved to
            `"section-1.2-introduction.html"`.

            Default is `"chunk-%n"` (string)

        `number_sections`
        :   whether sections should be numbered; default is `false`
            (boolean)

        `chunk_level`
        :   The heading level the document should be split into
            chunks. The default is to split at the top-level, i.e.,
            `1`. (integer)

        `base_heading_level`
        :   The base level to be used for numbering. Default is `nil`
            (integer|nil)

    (table)

Returns:

-  ([ChunkedDoc])

*Since: 3.0*

### table_of_contents {#pandoc.structure.table_of_contents}

`table_of_contents (toc_source[, opts])`

Generates a table of contents for the given object.

Parameters:

`toc_source`
:   list of command line arguments
    ([Blocks]\|[Pandoc]\|[ChunkedDoc])

`opts`
:   options ([WriterOptions])

Returns:

- Table of contents as a BulletList object ([Block])

*Since: 3.0*

### unique_identifier {#pandoc.structure.unique_identifier}

`unique_identifier (inlines[, used[, exts]])`

Generates a unique identifier from a list of inlines, similar to
what's generated by the `auto_identifiers` extension.

The method used to generated identifiers can be modified through
`ext`, which is a list of format extensions.

It can be used to generate IDs similar to what the
`auto_identifiers` extension provides.

Example:

     local used_ids = {}
     function Header (h)
       local id =
         pandoc.structure.unique_identifier(h.content, used_ids)
       used_ids[id] = true
       h.identifier = id
       return h
     end

Parameters:

`inlines`
:   base for identifier ([Inlines])

`used`
:   set of identifiers (string keys, boolean values) that have
    already been used. (table)

`exts`
:   list of format extensions ({string,\...})

Returns:

- unique identifier (string)

*Since: 3.8*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.system -->

# Module pandoc.system

Access to the system's information and file functionality.

## Fields {#pandoc.system-fields}

### arch {#pandoc.system.arch}

The machine architecture on which the program is running. (string)

### os {#pandoc.system.os}

The operating system on which the program is running. The most
common values are `darwin` (macOS), `freebsd`, `linux`,
`linux-android`, `mingw32` (Windows), `netbsd`, `openbsd`.
(string)

## Functions {#pandoc.system-functions}

### cputime {#pandoc.system.cputime}

`cputime ()`

Returns the number of picoseconds CPU time used by the current
program. The precision of this result may vary in different
versions and on different platforms.

Returns:

- CPU time in picoseconds ([integer]{unknown-type="integer"})

*Since: 3.1.1*

### command {#pandoc.system.command}

`command (command, args[, input[, opts]])`

Executes a system command with the given arguments and `input` on
*stdin*.

Parameters:

`command`
:   command to execute (string)

`args`
:   command arguments ({string,\...})

`input`
:   input on stdin (string)

`opts`
:   process options (table)

Returns:

- exit code -- `false` on success, an integer otherwise
  ([integer]{unknown-type="integer"}\|boolean)
- stdout (string)
- stderr (string)

*Since: 3.7.1*

### copy {#pandoc.system.copy}

`copy (source, target)`

Copy a file with its permissions. If the destination file already
exists, it is overwritten.

Parameters:

`source`
:   source file (string)

`target`
:   target destination (string)

*Since: 3.7.1*

### environment {#pandoc.system.environment}

`environment ()`

Retrieves the entire environment as a string-indexed table.

Returns:

- A table mapping environment variable names to their value.
  (table)

*Since: 2.7.3*

### get_working_directory {#pandoc.system.get_working_directory}

`get_working_directory ()`

Obtain the current working directory as an absolute path.

Returns:

- The current working directory. (string)

*Since: 2.8*

### list_directory {#pandoc.system.list_directory}

`list_directory ([directory])`

List the contents of a directory.

Parameters:

`directory`
:   Path of the directory whose contents should be listed.
    Defaults to `.`. (string)

Returns:

- A table of all entries in `directory`, except for the special
  entries (`.` and `..`). (table)

*Since: 2.19*

### make_directory {#pandoc.system.make_directory}

`make_directory (dirname[, create_parent])`

Create a new directory which is initially empty, or as near to
empty as the operating system allows. The function throws an error
if the directory cannot be created, e.g., if the parent directory
does not exist or if a directory of the same name is already
present.

If the optional second parameter is provided and truthy, then all
directories, including parent directories, are created as
necessary.

Parameters:

`dirname`
:   name of the new directory (string)

`create_parent`
:   create parent directory if necessary (boolean)

*Since: 2.19*

### read_file {#pandoc.system.read_file}

`read_file (filepath)`

Parameters:

`filepath`
:   File to read (string)

Returns:

- file contents (string)

*Since: 3.7.1*

### rename {#pandoc.system.rename}

`rename (old, new)`

Change the name of an existing path from `old` to `new`.

If `old` is a directory and `new` is a directory that already
exists, then `new` is atomically replaced by the `old` directory.
On Win32 platforms, this function fails if `new` is an existing
directory.

If `old` does not refer to a directory, then neither may `new`.

Renaming may not work across file system boundaries or due to
other system-specific reasons. It's generally more robust to copy
the source path to its destination before deleting the source.

Parameters:

`old`
:   original path (string)

`new`
:   new path (string)

*Since: 3.7.1*

### remove {#pandoc.system.remove}

`remove (filename)`

Removes the directory entry for an existing file.

Parameters:

`filename`
:   file to remove (string)

*Since: 3.7.1*

### remove_directory {#pandoc.system.remove_directory}

`remove_directory (dirname[, recursive])`

Remove an existing, empty directory. If `recursive` is given, then
delete the directory and its contents recursively.

Parameters:

`dirname`
:   name of the directory to delete (string)

`recursive`
:   delete content recursively (boolean)

*Since: 2.19*

### times {#pandoc.system.times}

`times (filepath)`

Obtain the modification and access time of a file or directory.
The times are returned as strings using the ISO 8601 format.

Parameters:

`filepath`
:   file or directory path (string)

Returns:

- time at which the file or directory was last modified (table)
- time at which the file or directory was last accessed (table)

*Since: 3.7.1*

### with_environment {#pandoc.system.with_environment}

`with_environment (environment, callback)`

Run an action within a custom environment. Only the environment
variables given by `environment` will be set, when `callback` is
called. The original environment is restored after this function
finishes, even if an error occurs while running the callback
action.

Parameters:

`environment`
:   Environment variables and their values to be set before
    running `callback` (table)

`callback`
:   Action to execute in the custom environment (function)

Returns:

The results of the call to `callback`.

*Since: 2.7.3*

### with_temporary_directory {#pandoc.system.with_temporary_directory}

`with_temporary_directory (parent_dir, templ, callback)`

Create and use a temporary directory inside the given directory.
The directory is deleted after the callback returns.

Parameters:

`parent_dir`
:   Parent directory to create the directory in. If this parameter
    is omitted, the system's canonical temporary directory is
    used. (string)

`templ`
:   Directory name template. (string)

`callback`
:   Function which takes the name of the temporary directory as
    its first argument. (function)

Returns:

The results of the call to `callback`.

*Since: 2.8*

### with_working_directory {#pandoc.system.with_working_directory}

`with_working_directory (directory, callback)`

Run an action within a different directory. This function will
change the working directory to `directory`, execute `callback`,
then switch back to the original working directory, even if an
error occurs while running the callback action.

Parameters:

`directory`
:   Directory in which the given `callback` should be executed
    (string)

`callback`
:   Action to execute in the given directory (function)

Returns:

The results of the call to `callback`.

*Since: 2.7.3*

### write_file {#pandoc.system.write_file}

`write_file (filepath, contents)`

Writes a string to a file.

Parameters:

`filepath`
:   path to target file (string)

`contents`
:   file contents (string)

*Since: 3.7.1*

### xdg {#pandoc.system.xdg}

`xdg (xdg_directory_type[, filepath])`

Access special directories and directory search paths.

Special directories for storing user-specific application data,
configuration, and cache files, as specified by the [XDG Base
Directory Specification].

Parameters:

`xdg_directory_type`

:   The type of the XDG directory or search path. Must be one of
    `config`, `data`, `cache`, `state`, `datadirs`, or
    `configdirs`.

    Matching is case-insensitive, and underscores and `XDG`
    prefixes are ignored, so a value like `XDG_DATA_DIRS` is also
    acceptable.

    The `state` directory might not be available, depending on the
    version of the underlying Haskell library. (string)

`filepath`
:   relative path that is appended to the path; ignored if the
    result is a list of search paths. (string)

Returns:

- Either a single file path, or a list of search paths.
  (string\|{string,\...})

*Since: 3.7.1*

<!-- END: AUTOGENERATED CONTENT -->


<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.layout -->

# Module pandoc.layout

Plain-text document layouting.

## Fields {#pandoc.layout-fields}

### blankline {#pandoc.layout.blankline}

Inserts a blank line unless one exists already. ([Doc])

### cr {#pandoc.layout.cr}

A carriage return. Does nothing if we're at the beginning of a
line; otherwise inserts a newline. ([Doc])

### empty {#pandoc.layout.empty}

The empty document. ([Doc])

### space {#pandoc.layout.space}

A breaking (reflowable) space. ([Doc])

## Functions {#pandoc.layout-functions}

### after_break {#pandoc.layout.after_break}

`after_break (text)`

Creates a [Doc] which is conditionally included only if it comes
at the beginning of a line.

An example where this is useful is for escaping line-initial `.`
in roff man.

Parameters:

`text`
:   content to include when placed after a break (string)

Returns:

- new doc ([Doc])

*Since: 2.18*

### before_non_blank {#pandoc.layout.before_non_blank}

`before_non_blank (doc)`

Conditionally includes the given `doc` unless it is followed by a
blank space.

Parameters:

`doc`
:   document ([Doc])

Returns:

- conditional doc ([Doc])

*Since: 2.18*

### blanklines {#pandoc.layout.blanklines}

`blanklines (n)`

Inserts blank lines unless they exist already.

Parameters:

`n`
:   number of blank lines ([integer]{unknown-type="integer"})

Returns:

- conditional blank lines ([Doc])

*Since: 2.18*

### braces {#pandoc.layout.braces}

`braces (doc)`

Puts the `doc` in curly braces.

Parameters:

`doc`
:   document ([Doc])

Returns:

- `doc` enclosed by {}. ([Doc])

*Since: 2.18*

### brackets {#pandoc.layout.brackets}

`brackets (doc)`

Puts the `doc` in square brackets

Parameters:

`doc`
:   document ([Doc])

Returns:

- doc enclosed by \[\]. ([Doc])

*Since: 2.18*

### cblock {#pandoc.layout.cblock}

`cblock (doc, width)`

Creates a block with the given width and content, aligned
centered.

Parameters:

`doc`
:   document ([Doc])

`width`
:   block width in chars ([integer]{unknown-type="integer"})

Returns:

- doc, aligned centered in a block with max `width` chars per
  line. ([Doc])

*Since: 2.18*

### chomp {#pandoc.layout.chomp}

`chomp (doc)`

Chomps trailing blank space off of the `doc`.

Parameters:

`doc`
:   document ([Doc])

Returns:

- `doc` without trailing blanks ([Doc])

*Since: 2.18*

### concat {#pandoc.layout.concat}

`concat (docs[, sep])`

Concatenates a list of `Doc`s.

Parameters:

`docs`
:   list of Docs ([\`{Doc,\...}\`]{unknown-type="`{Doc,...}`"})

`sep`
:   separator (default: none) ([Doc])

Returns:

- concatenated doc ([Doc])

*Since: 2.18*

### double_quotes {#pandoc.layout.double_quotes}

`double_quotes (doc)`

Wraps a `Doc` in double quotes.

Parameters:

`doc`
:   document ([Doc])

Returns:

- `doc` enclosed by `"` chars ([Doc])

*Since: 2.18*

### flush {#pandoc.layout.flush}

`flush (doc)`

Makes a `Doc` flush against the left margin.

Parameters:

`doc`
:   document ([Doc])

Returns:

- flushed `doc` ([Doc])

*Since: 2.18*

### hang {#pandoc.layout.hang}

`hang (doc, ind, start)`

Creates a hanging indent.

Parameters:

`doc`
:   document ([Doc])

`ind`
:   indentation width ([integer]{unknown-type="integer"})

`start`
:   document ([Doc])

Returns:

- `doc` prefixed by `start` on the first line, subsequent lines
  indented by `ind` spaces. ([Doc])

*Since: 2.18*

### inside {#pandoc.layout.inside}

`inside (contents, start, end)`

Encloses a [Doc] inside a start and end [Doc].

Parameters:

`contents`
:   document ([Doc])

`start`
:   document ([Doc])

`end`
:   document ([Doc])

Returns:

- enclosed contents ([Doc])

*Since: 2.18*

### lblock {#pandoc.layout.lblock}

`lblock (doc, width)`

Creates a block with the given width and content, aligned to the
left.

Parameters:

`doc`
:   document ([Doc])

`width`
:   block width in chars ([integer]{unknown-type="integer"})

Returns:

- doc put into block with max `width` chars per line. ([Doc])

*Since: 2.18*

### literal {#pandoc.layout.literal}

`literal (text)`

Creates a `Doc` from a string.

Parameters:

`text`
:   literal value (string)

Returns:

- doc contatining just the literal string ([Doc])

*Since: 2.18*

### nest {#pandoc.layout.nest}

`nest (doc, ind)`

Indents a `Doc` by the specified number of spaces.

Parameters:

`doc`
:   document ([Doc])

`ind`
:   indentation size ([integer]{unknown-type="integer"})

Returns:

- `doc` indented by `ind` spaces ([Doc])

*Since: 2.18*

### nestle {#pandoc.layout.nestle}

`nestle (doc)`

Removes leading blank lines from a `Doc`.

Parameters:

`doc`
:   document ([Doc])

Returns:

- `doc` with leading blanks removed ([Doc])

*Since: 2.18*

### nowrap {#pandoc.layout.nowrap}

`nowrap (doc)`

Makes a `Doc` non-reflowable.

Parameters:

`doc`
:   document ([Doc])

Returns:

- same as input, but non-reflowable ([Doc])

*Since: 2.18*

### parens {#pandoc.layout.parens}

`parens (doc)`

Puts the `doc` in parentheses.

Parameters:

`doc`
:   document ([Doc])

Returns:

- doc enclosed by (). ([Doc])

*Since: 2.18*

### prefixed {#pandoc.layout.prefixed}

`prefixed (doc, prefix)`

Uses the specified string as a prefix for every line of the inside
document (except the first, if not at the beginning of the line).

Parameters:

`doc`
:   document ([Doc])

`prefix`
:   prefix for each line (string)

Returns:

- prefixed `doc` ([Doc])

*Since: 2.18*

### quotes {#pandoc.layout.quotes}

`quotes (doc)`

Wraps a `Doc` in single quotes.

Parameters:

`doc`
:   document ([Doc])

Returns:

- doc enclosed in `'`. ([Doc])

*Since: 2.18*

### rblock {#pandoc.layout.rblock}

`rblock (doc, width)`

Creates a block with the given width and content, aligned to the
right.

Parameters:

`doc`
:   document ([Doc])

`width`
:   block width in chars ([integer]{unknown-type="integer"})

Returns:

- doc, right aligned in a block with max `width` chars per line.
  ([Doc])

*Since: 2.18*

### vfill {#pandoc.layout.vfill}

`vfill (border)`

An expandable border that, when placed next to a box, expands to
the height of the box. Strings cycle through the list provided.

Parameters:

`border`
:   vertically expanded characters (string)

Returns:

- automatically expanding border Doc ([Doc])

*Since: 2.18*

### render {#pandoc.layout.render}

`render (doc[, colwidth[, style]])`

Render a [Doc]. The text is reflowed on breakable spaces to match
the given line length. Text is not reflowed if the line line
length parameter is omitted or nil.

Parameters:

`doc`
:   document ([Doc])

`colwidth`
:   Maximum number of characters per line. A value of `nil`, the
    default, means that the text is not reflown.
    ([integer]{unknown-type="integer"})

`style`
:   Whether to generate plain text or ANSI terminal output. Must
    be either `'plain'` or `'ansi'`. Defaults to `'plain'`.
    (string)

Returns:

- rendered doc (string)

*Since: 2.18*

### is_empty {#pandoc.layout.is_empty}

`is_empty (doc)`

Checks whether a doc is empty.

Parameters:

`doc`
:   document ([Doc])

Returns:

- `true` iff `doc` is the empty document, `false` otherwise.
  (boolean)

*Since: 2.18*

### height {#pandoc.layout.height}

`height (doc)`

Returns the height of a block or other Doc.

Parameters:

`doc`
:   document ([Doc])

Returns:

- doc height ([integer]{unknown-type="integer"}\|string)

*Since: 2.18*

### min_offset {#pandoc.layout.min_offset}

`min_offset (doc)`

Returns the minimal width of a [Doc] when reflowed at breakable
spaces.

Parameters:

`doc`
:   document ([Doc])

Returns:

- minimal possible width
  ([integer]{unknown-type="integer"}\|string)

*Since: 2.18*

### offset {#pandoc.layout.offset}

`offset (doc)`

Returns the width of a [Doc] as number of characters.

Parameters:

`doc`
:   document ([Doc])

Returns:

- doc width ([integer]{unknown-type="integer"}\|string)

*Since: 2.18*

### real_length {#pandoc.layout.real_length}

`real_length (str)`

Returns the real length of a string in a monospace font: 0 for a
combining character, 1 for a regular character, 2 for an East
Asian wide character.

Parameters:

`str`
:   UTF-8 string to measure (string)

Returns:

- text length ([integer]{unknown-type="integer"}\|string)

*Since: 2.18*

### update_column {#pandoc.layout.update_column}

`update_column (doc, i)`

Returns the column that would be occupied by the last laid out
character.

Parameters:

`doc`
:   document ([Doc])

`i`
:   start column ([integer]{unknown-type="integer"})

Returns:

- column number ([integer]{unknown-type="integer"}\|string)

*Since: 2.18*

### bold {#pandoc.layout.bold}

`bold (doc)`

Puts a [Doc] in boldface.

Parameters:

`doc`
:   document ([Doc])

Returns:

- bolded Doc ([Doc])

*Since: 3.4.1*

### italic {#pandoc.layout.italic}

`italic (doc)`

Puts a [Doc] in italics.

Parameters:

`doc`
:   document ([Doc])

Returns:

- styled Doc ([Doc])

*Since: 3.4.1*

### underlined {#pandoc.layout.underlined}

`underlined (doc)`

Underlines a [Doc].

Parameters:

`doc`
:   document ([Doc])

Returns:

- styled Doc ([Doc])

*Since: 3.4.1*

### strikeout {#pandoc.layout.strikeout}

`strikeout (doc)`

Puts a line through the [Doc].

Parameters:

`doc`
:   document ([Doc])

Returns:

- styled Doc ([Doc])

*Since: 3.4.1*

### fg {#pandoc.layout.fg}

`fg (doc, color)`

Set the foreground color.

Parameters:

`doc`
:   document ([Doc])

`color`
:   One of 'black', 'red', 'green', 'yellow', 'blue', 'magenta'
    'cyan', or 'white'. (string)

Returns:

- styled Doc ([Doc])

*Since: 3.4.1*

### bg {#pandoc.layout.bg}

`bg (doc, color)`

Set the background color.

Parameters:

`doc`
:   document ([Doc])

`color`
:   One of 'black', 'red', 'green', 'yellow', 'blue', 'magenta'
    'cyan', or 'white'. (string)

Returns:

- styled Doc ([Doc])

*Since: 3.4.1*

## Types {#pandoc.layout-types}

### Doc {#type-pandoc.Doc}

See the description [above][Doc].

<!-- END: AUTOGENERATED CONTENT -->


<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.scaffolding -->

# Module pandoc.scaffolding

Scaffolding for custom writers.

## Fields {#pandoc.scaffolding-fields}

### Writer {#pandoc.scaffolding.Writer}

An object to be used as a `Writer` function; the construct handles
most of the boilerplate, expecting only render functions for all
AST elements (table)
<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.text -->

# Module pandoc.text

UTF-8 aware text manipulation functions, implemented in Haskell.

The text module can also be loaded under the name `text`, although
this is discouraged and deprecated.

``` lua
-- uppercase all regular text in a document:
function Str (s)
  s.text = pandoc.text.upper(s.text)
  return s
end
```

## Functions {#pandoc.text-functions}

### fromencoding {#pandoc.text.fromencoding}

`fromencoding (s[, encoding])`

Converts a string to UTF-8. The `encoding` parameter specifies the
encoding of the input string. On Windows, that parameter defaults
to the current ANSI code page; on other platforms the function
will try to use the file system's encoding.

The set of known encodings is system dependent, but includes at
least `UTF-8`, `UTF-16BE`, `UTF-16LE`, `UTF-32BE`, and `UTF-32LE`.
Note that the default code page on Windows is available through
`CP0`.

Parameters:

`s`
:   string to be converted (string)

`encoding`
:   target encoding (string)

Returns:

- UTF-8 string (string)

*Since: 3.0*

### len {#pandoc.text.len}

`len (s)`

Returns the length of a UTF-8 string, i.e., the number of
characters.

Parameters:

`s`
:   UTF-8 encoded string (string)

Returns:

- length ([integer]{unknown-type="integer"}\|string)

*Since: 2.0.3*

### lower {#pandoc.text.lower}

`lower (s)`

Returns a copy of a UTF-8 string, converted to lowercase.

Parameters:

`s`
:   UTF-8 string to convert to lowercase (string)

Returns:

- Lowercase copy of `s` (string)

*Since: 2.0.3*

### reverse {#pandoc.text.reverse}

`reverse (s)`

Returns a copy of a UTF-8 string, with characters reversed.

Parameters:

`s`
:   UTF-8 string to revert (string)

Returns:

- Reversed `s` (string)

*Since: 2.0.3*

### sub {#pandoc.text.sub}

`sub (s, i[, j])`

Returns a substring of a UTF-8 string, using Lua's string indexing
rules.

Parameters:

`s`
:   UTF-8 string (string)

`i`
:   substring start position ([integer]{unknown-type="integer"})

`j`
:   substring end position ([integer]{unknown-type="integer"})

Returns:

- text substring (string)

*Since: 2.0.3*

### subscript {#pandoc.text.subscript}

`subscript (input)`

Tries to convert the string into a Unicode subscript version of
the string. Returns `nil` if not all characters of the input can
be mapped to a subscript Unicode character. Supported characters
include numbers, parentheses, and plus/minus.

Parameters:

`input`
:   string to convert to subscript characters (string)

Returns:

- Subscript version of the input, or `nil` if not all characters
  could be converted. (string\|nil)

*Since: 3.8*

### superscript {#pandoc.text.superscript}

`superscript (input)`

Tries to convert the string into a Unicode superscript version of
the string. Returns `nil` if not all characters of the input can
be mapped to a superscript Unicode character. Supported characters
include numbers, parentheses, and plus/minus.

Parameters:

`input`
:   string to convert to superscript characters (string)

Returns:

- Superscript version of the input, or `nil` if not all characters
  could be converted. (string\|nil)

*Since: 3.8*

### toencoding {#pandoc.text.toencoding}

`toencoding (s[, enc])`

Converts a UTF-8 string to a different encoding. The `encoding`
parameter defaults to the current ANSI code page on Windows; on
other platforms it will try to guess the file system's encoding.

The set of known encodings is system dependent, but includes at
least `UTF-8`, `UTF-16BE`, `UTF-16LE`, `UTF-32BE`, and `UTF-32LE`.
Note that the default code page on Windows is available through
`CP0`.

Parameters:

`s`
:   UTF-8 string (string)

`enc`
:   target encoding (string)

Returns:

- re-encoded string (string)

*Since: 3.0*

### upper {#pandoc.text.upper}

`upper (s)`

Returns a copy of a UTF-8 string, converted to uppercase.

Parameters:

`s`
:   UTF-8 string to convert to uppercase (string)

Returns:

- Uppercase copy of `s` (string)

*Since: 2.0.3*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.template -->

# Module pandoc.template

Handle pandoc templates.

## Functions {#pandoc.template-functions}

### apply {#pandoc.template.apply}

`apply (template, context)`

Applies a context with variable assignments to a template,
returning the rendered template. The `context` parameter must be a
table with variable names as keys and [Doc], string, boolean, or
table as values, where the table can be either be a list of the
aforementioned types, or a nested context.

Parameters:

`template`
:   template to apply ([Template])

`context`
:   variable values (table)

Returns:

- rendered template ([Doc])

*Since: 3.0*

### compile {#pandoc.template.compile}

`compile (template[, templates_path])`

Compiles a template string into a [Template] object usable by
pandoc.

If the `templates_path` parameter is specified, then it should be
the file path associated with the template. It is used when
checking for partials. Partials will be taken only from the
default data files if this parameter is omitted.

An error is raised if compilation fails.

Parameters:

`template`
:   template string (string)

`templates_path`
:   parameter to determine a default path and extension for
    partials; uses the data files templates path by default.
    (string)

Returns:

- compiled template ([Template])

*Since: 2.17*

### default {#pandoc.template.default}

`default ([writer])`

Returns the default template for a given writer as a string. An
error is thrown if no such template can be found.

Parameters:

`writer`
:   name of the writer for which the template should be retrieved;
    defaults to the global `FORMAT`. (string)

Returns:

- raw template (string)

*Since: 2.17*

### get {#pandoc.template.get}

`get (filename)`

Retrieve text for a template.

This function first checks the resource paths for a file of this
name; if none is found, the `templates` directory in the user data
directory is checked. Returns the content of the file, or throws
an error if no file is found.

Parameters:

`filename`
:   name of the template (string)

Returns:

- content of template file (string)

*Since: 3.2.1*

### meta_to_context {#pandoc.template.meta_to_context}

`meta_to_context (meta, blocks_writer, inlines_writer)`

Creates template context from the document's [Meta] data, using
the given functions to convert [Blocks] and [Inlines] to [Doc]
values.

Parameters:

`meta`
:   document metadata ([Meta])

`blocks_writer`
:   converter from [Blocks] to [Doc] values (function)

`inlines_writer`
:   converter from [Inlines] to [Doc] values (function)

Returns:

- template context (table)

*Since: 3.0*

## Types {#pandoc.template-types}

### Template {#type-pandoc.Template}

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.types -->

# Module pandoc.types

Constructors for types that are not part of the pandoc AST.

## Functions {#pandoc.types-functions}

### Version {#pandoc.types.Version}

`Version (version_specifier)`

Parameters:

`version_specifier`
:   A version string like `'2.7.3'`, a Lua number like `2.0`, a
    list of integers like `{2,7,3}`, or a Version object.
    (string\|number\|{[integer]{unknown-type="integer"},\...}\|[Version])

Returns:

- New Version object. ([Version])

*Since: 2.7.3*

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: AUTOGENERATED CONTENT for module pandoc.zip -->

# Module pandoc.zip

Functions to create, modify, and extract files from zip archives.

The module can be called as a function, in which case it behaves
like the `zip` function described below.

Zip options are optional; when defined, they must be a table with
any of the following keys:

- `recursive`: recurse directories when set to `true`;
- `verbose`: print info messages to stdout;
- `destination`: the value specifies the directory in which to
  extract;
- `location`: value is used as path name, defining where files are
  placed.
- `preserve_symlinks`: Boolean value, controlling whether symbolic
  links are preserved as such. This option is ignored on Windows.

## Functions {#pandoc.zip-functions}

### Archive {#pandoc.zip.Archive}

`Archive ([bytestring_or_entries])`

Reads an *Archive* structure from a raw zip archive or a list of
Entry items; throws an error if the given string cannot be decoded
into an archive.

Parameters:

`bytestring_or_entries`
:   binary archive data or list of entries; defaults to an empty
    list (string\|{[zip.Entry],\...})

Returns:

- new Archive ([zip.Archive])

*Since: 3.0*

### Entry {#pandoc.zip.Entry}

`Entry (path, contents[, modtime])`

Generates a ZipEntry from a filepath, uncompressed content, and
the file's modification time.

Parameters:

`path`
:   file path in archive (string)

`contents`
:   uncompressed contents (string)

`modtime`
:   modification time ([integer]{unknown-type="integer"})

Returns:

- a new zip archive entry ([zip.Entry])

*Since: 3.0*

### read_entry {#pandoc.zip.read_entry}

`read_entry (filepath[, opts])`

Generates a ZipEntry from a file or directory.

Parameters:

`filepath`
:    (string)

`opts`
:   zip options (table)

Returns:

- a new zip archive entry ([zip.Entry])

*Since: 3.0*

### zip {#pandoc.zip.zip}

`zip (filepaths[, opts])`

Package and compress the given files into a new Archive.

Parameters:

`filepaths`
:   list of files from which the archive is created.
    ({string,\...})

`opts`
:   zip options (table)

Returns:

- a new archive ([zip.Archive])

*Since: 3.0*

## Types {#pandoc.zip-types}

### zip.Archive {#type-pandoc.zip.Archive}

#### Properties {#type-pandoc.zip.Archive-properties}

##### entries {#type-pandoc.zip.Archive.entries}

Files in this zip archive ({[zip.Entry],\...})

#### Methods {#type-pandoc.zip.Archive-methods}

##### bytestring {#pandoc.zip.Archive.bytestring}

`bytestring (self)`

Returns the raw binary string representation of the archive.

Parameters:

`self`
:    ([zip.Archive])

Returns:

- bytes of the archive (string)

##### extract {#pandoc.zip.Archive.extract}

`extract (self[, opts])`

Extract all files from this archive, creating directories as
needed. Note that the last-modified time is set correctly only in
POSIX, not in Windows. This function fails if encrypted entries
are present.

Parameters:

`self`
:    ([zip.Archive])

`opts`
:   zip options (table)

### zip.Entry {#type-pandoc.zip.Entry}

#### Properties {#type-pandoc.zip.Entry-properties}

##### modtime {#type-pandoc.zip.Entry.modtime}

Modification time (seconds since unix epoch)
([integer]{unknown-type="integer"})

##### path {#type-pandoc.zip.Entry.path}

Relative path, using `/` as separator ([zip.Entry])

#### Methods {#type-pandoc.zip.Entry-methods}

##### contents {#pandoc.zip.Entry.contents}

`contents (self[, password])`

Get the uncompressed contents of a zip entry. If `password` is
given, then that password is used to decrypt the contents. An
error is throws if decrypting fails.

Parameters:

`self`
:    ([zip.Entry])

`password`
:   password for entry (string)

Returns:

- binary contents (string)

##### symlink {#pandoc.zip.Entry.symlink}

`symlink (self)`

Returns the target if the Entry represents a symbolic link, and
`nil` otherwise. Always returns `nil` on Windows.

Parameters:

`self`
:    ([zip.Entry])

Returns:

- link target if entry represents a symbolic link (string\|nil)

<!-- END: AUTOGENERATED CONTENT -->

<!-- BEGIN: GENERATED REFERENCE LINKS -->

  [Blocks]: #type-blocks
  [Meta]: #type-meta
  [Pandoc]: #type-pandoc
  [Inlines]: #type-inlines
  [MetaValue]: #type-metavalue
  [Block]: #type-block
  [Attr]: #type-attr
  [Figure]: #type-figure
  [Caption]: #type-caption
  [ListAttributes]: #type-listattributes
  [ColSpec]: #type-colspec
  [TableHead]: #type-tablehead
  [TableBody]: #type-tablebody
  [TableFoot]: #type-tablefoot
  [Inline]: #type-inline
  [Span]: #type-span
  [Str]: #type-str
  [AttributeList]: #type-attributes
  [Alignment]: #type-alignment
  [Cell]: #type-cell
  [Row]: #type-row
  [SimpleTable]: #type-simpletable
  [Table]: #type-table
  [Version]: #type-version
  [`list`]: #pandoc.mediabag.list
  [WriterOptions]: #type-writeroptions
  [null]: #pandoc.json.null
  [this blog post]: http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html
  [ChunkedDoc]: #type-chunkeddoc
  [XDG Base Directory Specification]: https://specifications.freedesktop.org/basedir-spec/latest/
  [Doc]: #type-doc
  [Template]: #type-template
  [zip.Entry]: #type-pandoc.zip.Entry
  [zip.Archive]: #type-pandoc.zip.Archive
