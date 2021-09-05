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
interpreter (version 5.3) and a Lua library for creating pandoc
filters is built into the pandoc executable. Pandoc data types
are marshaled to Lua directly, avoiding the overhead of writing
JSON to stdout and reading it from stdin.

Here is an example of a Lua filter that converts strong emphasis
to small caps:

``` lua
return {
  {
    Strong = function (elem)
      return pandoc.SmallCaps(elem.c)
    end,
  }
}
```

or equivalently,

``` lua
function Strong(elem)
  return pandoc.SmallCaps(elem.c)
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

Pandoc expects each Lua file to return a list of filters. The
filters in that list are called sequentially, each on the result
of the previous filter. If there is no value returned by the
filter script, then pandoc will try to generate a single filter
by collecting all top-level functions whose names correspond to
those of pandoc elements (e.g., `Str`, `Para`, `Meta`, or
`Pandoc`). (That is why the two examples above are equivalent.)

For each filter, the document is traversed and each element
subjected to the filter. Elements for which the filter contains
an entry (i.e. a function of the same name) are passed to Lua
element filtering function. In other words, filter entries will
be called for each corresponding element in the document,
getting the respective element as input.

The return of a filter function must be one of the following:

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
    [Inlines] for each call.

[`Blocks (blocks)`]{#blocks-filter}
:   If present in a filter, this function will be called on all
    lists of block elements, like the content of a [MetaBlocks]
    meta element block, on each item of a list, and the main
    content of the [Pandoc] document. The `blocks` argument
    passed to the function will be a [List] of [Blocks] for each
    call.

These filter functions are special in that the result must either
be nil, in which case the list is left unchanged, or must be a
list of the correct type, i.e., the same type as the input
argument. Single elements are **not** allowed as return values,
as a single element in this context usually hints at a bug.

See ["Remove spaces before normal citations"][Inlines filter
example] for an example.

This functionality has been added in pandoc 2.9.2.

[Inlines filter example]: #remove-spaces-before-citations

## Execution Order

Element filter functions within a filter set are called in a
fixed order, skipping any which are not present:

  1. functions for [*Inline* elements](#type-inline),
  2. the [`Inlines`](#inlines-filter) filter function,
  2. functions for [*Block* elements](#type-block) ,
  2. the [`Blocks`](#inlines-filter) filter function,
  3. the [`Meta`](#type-meta) filter function, and last
  4. the [`Pandoc`](#type-pandoc) filter function.

It is still possible to force a different order by explicitly
returning multiple filter sets. For example, if the filter for
*Meta* is to be run before that for *Str*, one can write

``` lua
-- ... filter definitions ...

return {
  { Meta = Meta },  -- (1)
  { Str = Str }     -- (2)
}
```

Filter sets are applied in the order in which they are returned.
All functions in set (1) are thus run before those in (2),
causing the filter function for *Meta* to be run before the
filtering of *Str* elements is started.

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
functions defined in the [`text` module](#module-text) to the
default `string` module, prefixed with the string `uc_`.

``` lua
for name, fn in pairs(require 'text') do
  string['uc_' .. name] = fn
end
```

This makes it possible to apply these functions on strings using
colon syntax (`mystring:uc_upper()`).

# Debugging Lua filters

It is possible to use a debugging interface to halt execution and
step through a Lua filter line by line as it is run inside Pandoc.
This is accomplished using the remote-debugging interface of the
package [`mobdebug`](https://github.com/pkulchenko/MobDebug).
Although mobdebug can be run from the terminal, it is more useful
run within the donation-ware Lua editor and IDE,
[ZeroBrane](https://studio.zerobrane.com/). ZeroBrane offers a
REPL console and UI to step-through and view all variables and
state.

If you already have Lua 5.3 installed, you can add
[`mobdebug`](https://luarocks.org/modules/paulclinger/mobdebug)
and its dependency
[`luasocket`](https://luarocks.org/modules/luasocket/luasocket)
using [`luarocks`](https://luarocks.org), which should then be
available on the path. ZeroBrane also includes both of these in
its package, so if you don't want to install Lua separately, you
should add/modify your `LUA_PATH` and `LUA_CPATH` to include the
correct locations; [see detailed instructions
here](https://studio.zerobrane.com/doc-remote-debugging).

# Examples

The following filters are presented as examples. A repository of
useful Lua filters (which may also serve as good examples) is
available at <https://github.com/pandoc/lua-filters>.

## Macro substitution

The following filter converts the string `{{helloworld}}` into
emphasized text "Hello, World".

``` lua
return {
  {
    Str = function (elem)
      if elem.text == "{{helloworld}}" then
        return pandoc.Emph {pandoc.Str "Hello, World"}
      else
        return elem
      end
    end,
  }
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

function get_vars (meta)
  for k, v in pairs(meta) do
    if type(v) == 'table' and v.t == 'MetaInlines' then
      vars["%" .. k .. "%"] = {table.unpack(v)}
    end
  end
end

function replace (el)
  if vars[el.text] then
    return pandoc.Span(vars[el.text])
  else
    return el
  end
end

return {{Meta = get_vars}, {Str = replace}}
```

If the contents of file `occupations.md` is

``` markdown
---
name: Samuel Q. Smith
occupation: Professor of Phrenology
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
<dd><p><span>Professor of Phrenology</span></p>
</dd>
</dl>
```

## Modifying pandoc's `MANUAL.txt` for man pages

This is the filter we use when converting `MANUAL.txt` to man
pages. It converts level-1 headers to uppercase (using
`walk_block` to transform inline elements inside headers),
removes footnotes, and replaces links with regular text.

``` lua
-- we use preloaded text to get a UTF-8 aware 'upper' function
local text = require('text')

function Header(el)
    if el.level == 1 then
      return pandoc.walk_block(el, {
        Str = function(el)
            return pandoc.Str(text.upper(el.text))
        end })
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
    pandoc.walk_block(pandoc.Div(el.blocks), wordcount)
    print(words .. " words in body")
    os.exit(0)
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
`--self-contained`.

``` lua
-- Pandoc filter to process code blocks with class "abc" containing
-- ABC notation into images.
--
-- * Assumes that abcm2ps and ImageMagick's convert are in the path.
-- * For textual output formats, use --extract-media=abc-images
-- * For HTML formats, you may alternatively use --self-contained

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
[`pandoc.Pandoc`](#pandoc.pandoc) constructor. Object equality is
determined via [`pandoc.utils.equals`].

`blocks`
:   document content ([List] of [Blocks])

`meta`
:   document meta information ([Meta] object)

## Meta {#type-meta}

Meta information on a document; string-indexed collection of
[MetaValues].

Values of this type can be created with the
[`pandoc.Meta`](#pandoc.meta) constructor. Object equality is
determined via [`pandoc.utils.equals`].

## MetaValue {#type-metavalue}

Document meta information items.

Object equality is determined via [`pandoc.utils.equals`].

### MetaBlocks {#type-metablocks}

A list of blocks usable as meta value ([List] of [Blocks]).

Fields:

`tag`, `t`
:   the literal `MetaBlocks` (string)

### MetaBool {#type-metabool}

Alias for Lua boolean, i.e. the values `true` and `false`.

### MetaInlines {#type-metainlines}

List of inlines used in metadata ([List] of [Inlines])

Values of this type can be created with the
[`pandoc.MetaInlines`](#pandoc.metainlines) constructor.

Fields:

`tag`, `t`
:   the literal `MetaInlines` (string)

### MetaList {#type-metalist}

A list of other metadata values ([List] of [MetaValues]).

Values of this type can be created with the
[`pandoc.MetaList`](#pandoc.metalist) constructor.

Fields:

`tag`, `t`
:   the literal `MetaList` (string)

All methods available for [List]s can be used on this type as
well.

### MetaMap {#type-metamap}

A string-indexed map of meta-values. (table).

Values of this type can be created with the
[`pandoc.MetaMap`](#pandoc.metamap) constructor.

Fields:

`tag`, `t`
:   the literal `MetaMap` (string)

*Note*: The fields will be shadowed if the map contains a field
with the same name as those listed.

### MetaString {#type-metastring}

Plain Lua string value (string).

## Block {#type-block}

Object equality is determined via [`pandoc.utils.equals`].

### BlockQuote {#type-blockquote}

A block quote element.

Values of this type can be created with the
[`pandoc.BlockQuote`](#pandoc.blockquote) constructor.

Fields:

`content`:
:   block content ([List] of [Blocks])

`tag`, `t`
:   the literal `BlockQuote` (string)

### BulletList {#type-bulletlist}

A bullet list.

Values of this type can be created with the
[`pandoc.BulletList`](#pandoc.bulletlist) constructor.

Fields:

`content`
:   list items ([List] of [List] of [Blocks])

`tag`, `t`
:   the literal `BulletList` (string)

### CodeBlock {#type-codeblock}

Block of code.

Values of this type can be created with the
[`pandoc.CodeBlock`](#pandoc.codeblock) constructor.

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
[`pandoc.DefinitionList`](#pandoc.definitionlist) constructor.

Fields:

`content`
:   list of items

`tag`, `t`
:   the literal `DefinitionList` (string)

### Div {#type-div}

Generic block container with attributes.

Values of this type can be created with the
[`pandoc.Div`](#pandoc.div) constructor.

Fields:

`content`
:   block content ([List] of [Blocks])

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

### Header {#type-header}

Creates a header element.

Values of this type can be created with the
[`pandoc.Header`](#pandoc.header) constructor.

Fields:

`level`
:   header level (integer)

`content`
:   inline content ([List] of [Inlines])

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
[`pandoc.HorizontalRule`](#pandoc.horizontalrule) constructor.

Fields:

`tag`, `t`
:   the literal `HorizontalRule` (string)

### LineBlock {#type-lineblock}

A line block, i.e. a list of lines, each separated from the next
by a newline.

Values of this type can be created with the
[`pandoc.LineBlock`](#pandoc.lineblock) constructor.

Fields:

`content`
:   inline content

`tag`, `t`
:   the literal `LineBlock` (string)

### Null {#type-null}

A null element; this element never produces any output in the
target format.

Values of this type can be created with the
[`pandoc.Null`](#pandoc.null) constructor.

`tag`, `t`
:   the literal `Null` (string)

### OrderedList {#type-orderedlist}

An ordered list.

Values of this type can be created with the
[`pandoc.OrderedList`](#pandoc.orderedlist) constructor.

Fields:

`content`
:   list items ([List] of [List] of [Blocks])

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
[`pandoc.Para`](#pandoc.para) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Para` (string)

### Plain {#type-plain}

Plain text, not a paragraph.

Values of this type can be created with the
[`pandoc.Plain`](#pandoc.plain) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Plain` (string)

### RawBlock {#type-rawblock}

Raw content of a specified format.

Values of this type can be created with the
[`pandoc.RawBlock`](#pandoc.rawblock) constructor.

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
[`pandoc.Table`](#pandoc.table) constructor.

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

## Inline {#type-inline}

Object equality is determined via [`pandoc.utils.equals`].

### Cite {#type-cite}

Citation.

Values of this type can be created with the
[`pandoc.Cite`](#pandoc.cite) constructor.

Fields:

`content`
:   ([List] of [Inlines])

`citations`
:   citation entries ([List] of [Citations])

`tag`, `t`
:   the literal `Cite` (string)

### Code {#type-code}

Inline code

Values of this type can be created with the
[`pandoc.Code`](#pandoc.code) constructor.

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
[`pandoc.Emph`](#pandoc.emph) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Emph` (string)

### Image {#type-image}

Image: alt text (list of inlines), target

Values of this type can be created with the
[`pandoc.Image`](#pandoc.image) constructor.

Fields:

`caption`
:   text used to describe the image ([List] of [Inlines])

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
[`pandoc.LineBreak`](#pandoc.linebreak) constructor.

Fields:

`tag`, `t`
:   the literal `LineBreak` (string)

### Link {#type-link}

Hyperlink: alt text (list of inlines), target

Values of this type can be created with the
[`pandoc.Link`](#pandoc.link) constructor.

Fields:

`attr`
:   attributes ([Attr])

`content`
:   text for this link ([List] of [Inlines])

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
[`pandoc.Math`](#pandoc.math) constructor.

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
[`pandoc.Note`](#pandoc.note) constructor.

Fields:

`content`
:   ([List] of [Blocks])

`tag`, `t`
:   the literal `Note` (string)

### Quoted {#type-quoted}

Quoted text

Values of this type can be created with the
[`pandoc.Quoted`](#pandoc.quoted) constructor.

Fields:

`quotetype`
:   type of quotes to be used; one of `SingleQuote` or
    `DoubleQuote` (string)

`content`
:   quoted text ([List] of [Inlines])

`tag`, `t`
:   the literal `Quoted` (string)

### RawInline {#type-rawinline}

Raw inline

Values of this type can be created with the
[`pandoc.RawInline`](#pandoc.rawinline) constructor.

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
[`pandoc.SmallCaps`](#pandoc.smallcaps) constructor.

Fields:

`content`
:   ([List] of [Inlines])

`tag`, `t`
:   the literal `SmallCaps` (string)

### SoftBreak {#type-softbreak}

Soft line break

Values of this type can be created with the
[`pandoc.SoftBreak`](#pandoc.softbreak) constructor.

Fields:

`tag`, `t`
:   the literal `SoftBreak` (string)

### Space {#type-space}

Inter-word space

Values of this type can be created with the
[`pandoc.Space`](#pandoc.space) constructor.

Fields:

`tag`, `t`
:   the literal `Space` (string)

### Span {#type-span}

Generic inline container with attributes

Values of this type can be created with the
[`pandoc.Span`](#pandoc.span) constructor.

Fields:

`attr`
:   attributes ([Attr])

`content`
:   wrapped content ([List] of [Inlines])

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
[`pandoc.Str`](#pandoc.str) constructor.

Fields:

`text`
:   content (string)

`tag`, `t`
:   the literal `Str` (string)

### Strikeout {#type-strikeout}

Strikeout text

Values of this type can be created with the
[`pandoc.Strikeout`](#pandoc.strikeout) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Strikeout` (string)

### Strong {#type-strong}

Strongly emphasized text

Values of this type can be created with the
[`pandoc.Strong`](#pandoc.strong) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Strong` (string)

### Subscript {#type-subscript}

Subscripted text

Values of this type can be created with the
[`pandoc.Subscript`](#pandoc.subscript) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Subscript` (string)

### Superscript {#type-superscript}

Superscripted text

Values of this type can be created with the
[`pandoc.Superscript`](#pandoc.superscript) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Superscript` (string)

### Underline {#type-underline}

Underlined text

Values of this type can be created with the
[`pandoc.Underline`](#pandoc.underline) constructor.

Fields:

`content`
:   inline content ([List] of [Inlines])

`tag`, `t`
:   the literal `Underline` (string)


## Element components

### Attr {#type-attr}

A set of element attributes. Values of this type can be created
with the [`pandoc.Attr`](#pandoc.attr) constructor. For
convenience, it is usually not necessary to construct the value
directly if it is part of an element, and it is sufficient to
pass an HTML-like table. E.g., to create a span with identifier
"text" and classes "a" and "b", one can write:

    local span = pandoc.Span('text', {id = 'text', class = 'a b'})

This also works when using the `attr` setter:

    local span = pandoc.Span 'text'
    span.attr = {id = 'text', class = 'a b', other_attribute = '1'}

Object equality is determined via [`pandoc.utils.equals`].

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

### Caption {#type-caption}

The caption of a table, with an optional short caption.

Fields:

`long`
:   long caption (list of [Blocks])

`short`
:   short caption (list of [Inlines])

### Cell {#type-cell}

A table cell.

Fields:

`attr`
:   cell attributes

`alignment`
:   individual cell alignment ([Alignment]).

`contents`
:   cell contents (list of [Blocks]).

`col_span`
:   number of columns occupied by the cell; the height of the cell
    (integer).

`row_span`
:   number of rows occupied by the cell; the height of the cell
    (integer).

### Citation {#type-citation}

Single citation entry

Values of this type can be created with the
[`pandoc.Citation`](#pandoc.citation) constructor.

Object equality is determined via [`pandoc.utils.equals`].

Fields:

`id`
:   citation identifier, e.g., a bibtex key (string)

`mode`
:   citation mode, one of `AuthorInText`, `SuppressAuthor`, or
    `NormalCitation` (string)

`prefix`
:   citation prefix ([List] of [Inlines])

`suffix`
:   citation suffix ([List] of [Inlines])

`note_num`
:   note number (integer)

`hash`
:   hash (integer)

### ColSpec {#type-colspec}

Column alignment and width specification for a single table
column.

This is a pair with the following components:

1. cell alignment ([Alignment]).
2. table column width, as a fraction of the total table width
   (number).

### ListAttributes {#type-listattributes}

List attributes

Values of this type can be created with the
[`pandoc.ListAttributes`](#pandoc.listattributes) constructor.

Object equality is determined via [`pandoc.utils.equals`].

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

Tuple fields:

1. row attributes
2. row cells (list of [Cells])

### TableBody {#type-tablebody}

A body of a table, with an intermediate head and the specified
number of row header columns.

Fields:

`attr`
:   table body attributes ([Attr])

`body`
:   table body rows (list of [Rows])

`head`
:   intermediate head (list of [Rows])

`row_head_columns`
:   number of columns taken up by the row head of each row of a
    [TableBody]. The row body takes up the remaining columns.

### TableFoot {#type-tablefoot}

The foot of a table.

This is a pair with the following components:

1. attributes
2. foot rows ([Rows])

### TableHead {#type-tablehead}

The head of a table.

This is a pair with the following components:

1. attributes
2. head rows ([Rows])

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
    (string)

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
:   track changes setting for docx; one of `AcceptChanges`,
    `RejectChanges`, and `AllChanges` (string)

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
:   A list of log messages in reverse order ([List] of
    [LogMessage]s)

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

`caption`:
:   [List] of [Inlines]

`aligns`:
:   column alignments ([List] of [Alignments](#type-alignment))

`widths`:
:   column widths; a  ([List] of numbers)

`headers`:
:   table header row ([List] of lists of [Blocks])

`rows`:
:   table rows ([List] of rows, where a row is a list of lists of
    [Blocks])

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
[`pandoc.types.Version`](#pandoc.types.version) constructor.

### `must_be_at_least`

`must_be_at_least(actual, expected [, error_message])`

Raise an error message if the actual version is older than the
expected version; does nothing if actual is equal to or newer
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

[Alignment]: #type-alignment
[Attr]: #type-attr
[Attributes]: #type-attributes
[Block]: #type-block
[Blocks]: #type-block
[Caption]: #type-caption
[Cells]: #type-cell
[Citation]: #type-citation
[Citations]: #type-citation
[ColSpec]: #type-colspec
[CommonState]: #type-commonstate
[Image]: #type-image
[Inline]: #type-inline
[Inlines]: #type-inline
[List]: #type-list
[ListAttributes]: #type-listattributes
[Meta]: #type-meta
[MetaBlocks]: #type-metablocks
[MetaValue]: #type-metavalue
[MetaValues]: #type-metavalue
[LogMessage]: #type-logmessage
[Pandoc]: #type-pandoc
[Para]: #type-para
[Rows]: #type-row
[SimpleTable]: #type-simpletable
[Table]: #type-table
[TableBody]: #type-tablebody
[TableFoot]: #type-tablefoot
[TableHead]: #type-tablehead
[Version]: #type-version
[`pandoc.utils.equals`]: #pandoc.utils.equals

# Module text

UTF-8 aware text manipulation functions, implemented in Haskell.
The module is made available as part of the `pandoc` module via
`pandoc.text`. The text module can also be loaded explicitly:

``` lua
-- uppercase all regular text in a document:
text = require 'text'
function Str (s)
  s.text = text.upper(s.text)
  return s
end
```

### lower {#text.lower}

`lower (s)`

Returns a copy of a UTF-8 string, converted to lowercase.

### upper {#text.upper}

`upper (s)`

Returns a copy of a UTF-8 string, converted to uppercase.

### reverse {#text.reverse}

`reverse (s)`

Returns a copy of a UTF-8 string, with characters reversed.

### len {#text.len}

`len (s)`

Returns the length of a UTF-8 string.

### sub {#text.sub}

`sub (s)`

Returns a substring of a UTF-8 string, using Lua's string
indexing rules.

# Module pandoc

Lua functions for pandoc scripts; includes constructors for
document tree elements, functions to parse text in a given
format, and functions to filter and modify a subtree.

## Pandoc

[`Pandoc (blocks[, meta])`]{#pandoc.pandoc}

:   A complete pandoc document

    Parameters:

    `blocks`:
    :   document content

    `meta`:
    :   document meta data

    Returns: [Pandoc] object

## Meta

[`Meta (table)`]{#pandoc.meta}

:   Create a new Meta object.

    Parameters:

    `table`:
    :   table containing document meta information

    Returns: [Meta] object

## MetaValue

[`MetaBlocks (blocks)`]{#pandoc.metablocks}

:   Meta blocks

    Parameters:

    `blocks`:
    :   blocks

    Returns: [MetaBlocks] object

[`MetaInlines (inlines)`]{#pandoc.metainlines}

:   Meta inlines

    Parameters:

    `inlines`:
    :   inlines

    Returns: [MetaInlines] object

[`MetaList (meta_values)`]{#pandoc.metalist}

:   Meta list

    Parameters:

    `meta_values`:
    :   list of meta values

    Returns: [MetaList] object

[`MetaMap (key_value_map)`]{#pandoc.metamap}

:   Meta map

    Parameters:

    `key_value_map`:
    :   a string-indexed map of meta values

    Returns: [MetaMap] object

[`MetaString (str)`]{#pandoc.metastring}

:   Creates string to be used in meta data.

    Parameters:

    `str`:
    :   string value

    Returns: [MetaString] object

[`MetaBool (bool)`]{#pandoc.metabool}

:   Creates boolean to be used in meta data.

    Parameters:

    `bool`:
    :   boolean value

    Returns: [MetaBool] object

## Blocks

[`BlockQuote (content)`]{#pandoc.blockquote}

:   Creates a block quote element

    Parameters:

    `content`:
    :   block content

    Returns: [BlockQuote] object

[`BulletList (items)`]{#pandoc.bulletlist}

:   Creates a bullet list.

    Parameters:

    `items`:
    :   list items

    Returns: [BulletList] object

[`CodeBlock (text[, attr])`]{#pandoc.codeblock}

:   Creates a code block element

    Parameters:

    `text`:
    :   code string

    `attr`:
    :   element attributes

    Returns: [CodeBlock] object

[`DefinitionList (content)`]{#pandoc.definitionlist}

:   Creates a definition list, containing terms and their
    explanation.

    Parameters:

    `content`:
    :   list of items

    Returns: [DefinitionList] object

[`Div (content[, attr])`]{#pandoc.div}

:   Creates a div element

    Parameters:

    `content`:
    :   block content

    `attr`:
    :   element attributes

    Returns: [Div] object

[`Header (level, content[, attr])`]{#pandoc.header}

:   Creates a header element.

    Parameters:

    `level`:
    :   header level

    `content`:
    :   inline content

    `attr`:
    :   element attributes

    Returns: [Header] object

[`HorizontalRule ()`]{#pandoc.horizontalrule}

:   Creates a horizontal rule.

    Returns: [HorizontalRule] object

[`LineBlock (content)`]{#pandoc.lineblock}

:   Creates a line block element.

    Parameters:

    `content`:
    :   inline content

    Returns: [LineBlock] object

[`Null ()`]{#pandoc.null}

:   Creates a null element.

    Returns: [Null] object

[`OrderedList (items[, listAttributes])`]{#pandoc.orderedlist}

:   Creates an ordered list.

    Parameters:

    `items`:
    :   list items

    `listAttributes`:
    :   list parameters

    Returns: [OrderedList](#type-orderedlist) object

[`Para (content)`]{#pandoc.para}

:   Creates a para element.

    Parameters:

    `content`:
    :   inline content

    Returns: [Para](#type-para) object

[`Plain (content)`]{#pandoc.plain}

:   Creates a plain element.

    Parameters:

    `content`:
    :   inline content

    Returns: [Plain](#type-plain) object

[`RawBlock (format, text)`]{#pandoc.rawblock}

:   Creates a raw content block of the specified format.

    Parameters:

    `format`:
    :   format of content

    `text`:
    :   string content

    Returns: [RawBlock](#type-rawblock) object

[`Table (caption, colspecs, head, bodies, foot[, attr])`]{#pandoc.table}

:   Creates a table element.

    Parameters:

    `caption`:
    :   table [caption](#type-caption)

    `colspecs`:
    :   column alignments and widths (list of [ColSpec](#type-colspec)s)

    `head`:
    :   [table head](#type-tablehead)

    `bodies`:
    :   [table bodies](#type-tablebody)

    `foot`:
    :   [table foot](#type-tablefoot)

    `attr`:
    :   element attributes

    Returns: [Table](#type-table) object

## Inline

[`Cite (content, citations)`]{#pandoc.cite}

:   Creates a Cite inline element

    Parameters:

    `content`:
    :   List of inlines

    `citations`:
    :   List of citations

    Returns: [Cite](#type-cite) object

[`Code (text[, attr])`]{#pandoc.code}

:   Creates a Code inline element

    Parameters:

    `text`:
    :   code string

    `attr`:
    :   additional attributes

    Returns: [Code](#type-code) object

[`Emph (content)`]{#pandoc.emph}

:   Creates an inline element representing emphasized text.

    Parameters:

    `content`:
    :   inline content

    Returns: [Emph](#type-emph) object

[`Image (caption, src[, title[, attr]])`]{#pandoc.image}

:   Creates a Image inline element

    Parameters:

    `caption`:
    :   text used to describe the image

    `src`:
    :   path to the image file

    `title`:
    :   brief image description

    `attr`:
    :   additional attributes

    Returns: [Image](#type-image) object

[`LineBreak ()`]{#pandoc.linebreak}

:   Create a LineBreak inline element

    Returns: [LineBreak](#type-linebreak) object

[`Link (content, target[, title[, attr]])`]{#pandoc.link}

:   Creates a link inline element, usually a hyperlink.

    Parameters:

    `content`:
    :   text for this link

    `target`:
    :   the link target

    `title`:
    :   brief link description

    `attr`:
    :   additional attributes

    Returns: [Link](#type-link) object

[`Math (mathtype, text)`]{#pandoc.math}

:   Creates a Math element, either inline or displayed.

    Parameters:

    `mathtype`:
    :   rendering specifier

    `text`:
    :   Math content

    Returns: [Math](#type-math) object

[`DisplayMath (text)`]{#pandoc.displaymath}

:   Creates a math element of type "DisplayMath" (DEPRECATED).

    Parameters:

    `text`:
    :   Math content

    Returns: [Math](#type-math) object

[`InlineMath (text)`]{#pandoc.inlinemath}

:   Creates a math element of type "InlineMath" (DEPRECATED).

    Parameters:

    `text`:
    :   Math content

    Returns: [Math](#type-math) object

[`Note (content)`]{#pandoc.note}

:   Creates a Note inline element

    Parameters:

    `content`:
    :   footnote block content

    Returns: [Note](#type-note) object

[`Quoted (quotetype, content)`]{#pandoc.quoted}

:   Creates a Quoted inline element given the quote type and
    quoted content.

    Parameters:

    `quotetype`:
    :   type of quotes to be used

    `content`:
    :   inline content

    Returns: [Quoted](#type-quoted) object

[`SingleQuoted (content)`]{#pandoc.singlequoted}

:   Creates a single-quoted inline element (DEPRECATED).

    Parameters:

    `content`:
    :   inline content

    Returns: [Quoted](#type-quoted)

[`DoubleQuoted (content)`]{#pandoc.doublequoted}

:   Creates a single-quoted inline element (DEPRECATED).

    Parameters:

    `content`:
    :   inline content

    Returns: [Quoted](#type-quoted)

[`RawInline (format, text)`]{#pandoc.rawinline}

:   Creates a raw inline element

    Parameters:

    `format`:
    :   format of the contents

    `text`:
    :   string content

    Returns: [RawInline](#type-rawinline) object

[`SmallCaps (content)`]{#pandoc.smallcaps}

:   Creates text rendered in small caps

    Parameters:

    `content`:
    :   inline content

    Returns: [SmallCaps](#type-smallcaps) object

[`SoftBreak ()`]{#pandoc.softbreak}

:   Creates a SoftBreak inline element.

    Returns: [SoftBreak](#type-softbreak) object

[`Space ()`]{#pandoc.space}

:   Create a Space inline element

    Returns: [Space](#type-space) object

[`Span (content[, attr])`]{#pandoc.span}

:   Creates a Span inline element

    Parameters:

    `content`:
    :   inline content

    `attr`:
    :   additional attributes

    Returns: [Span](#type-span) object

[`Str (text)`]{#pandoc.str}

:   Creates a Str inline element

    Parameters:

    `text`:
    :   content

    Returns: [Str](#type-str) object

[`Strikeout (content)`]{#pandoc.strikeout}

:   Creates text which is struck out.

    Parameters:

    `content`:
    :   inline content

    Returns: [Strikeout](#type-strikeout) object

[`Strong (content)`]{#pandoc.strong}

:   Creates a Strong element, whose text is usually displayed in
    a bold font.

    Parameters:

    `content`:
    :   inline content

    Returns: [Strong](#type-strong) object

[`Subscript (content)`]{#pandoc.subscript}

:   Creates a Subscript inline element

    Parameters:

    `content`:
    :   inline content

    Returns: [Subscript](#type-subscript) object

[`Superscript (content)`]{#pandoc.superscript}

:   Creates a Superscript inline element

    Parameters:

    `content`:
    :   inline content

    Returns: [Superscript](#type-superscript) object

[`Underline (content)`]{#pandoc.underline}

:   Creates an Underline inline element

    Parameters:

    `content`:
    :   inline content

    Returns: [Underline](#type-underline) object

## Element components

[`Attr ([identifier[, classes[, attributes]]])`]{#pandoc.attr}

:   Create a new set of attributes (Attr).

    Parameters:

    `identifier`:
    :   element identifier

    `classes`:
    :   element classes

    `attributes`:
    :   table containing string keys and values

    Returns: [Attr](#type-attr) object

[`Citation (id, mode[, prefix[, suffix[, note_num[, hash]]]])`]{#pandoc.citation}

:   Creates a single citation.

    Parameters:

    `id`:
    :   citation identifier (like a bibtex key)

    `mode`:
    :   citation mode

    `prefix`:
    :   citation prefix

    `suffix`:
    :   citation suffix

    `note_num`:
    :   note number

    `hash`:
    :   hash number

    Returns: [Citation](#type-citation) object

[`ListAttributes ([start[, style[, delimiter]]])`]{#pandoc.listattributes}

:   Creates a set of list attributes.

    Parameters:

    `start`:
    :   number of the first list item

    `style`:
    :   style used for list numbering

    `delimiter`:
    :   delimiter of list numbers

    Returns: [ListAttributes](#type-listattributes) object

## Legacy types

[`SimpleTable (caption, aligns, widths, headers, rows)`]{#pandoc.simpletable}

:   Creates a simple table resembling the old (pre pandoc 2.10)
    table type.

    Parameters:

    `caption`:
    :   [List] of [Inlines]

    `aligns`:
    :   column alignments ([List] of [Alignments](#type-alignment))

    `widths`:
    :   column widths; a  ([List] of numbers)

    `headers`:
    :   table header row ([List] of lists of [Blocks])

    `rows`:
    :   table rows ([List] of rows, where a row is a list of lists
        of [Blocks])

    Returns: [SimpleTable] object

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
    (DEPRECATED).

## Helper functions

### pipe {#pandoc.pipe}

`pipe (command, args, input)`

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

### walk\_block {#pandoc.walk_block}

`walk_block (element, filter)`

Apply a filter inside a block element, walking its contents.

Parameters:

`element`:
:   the block element

`filter`:
:   a Lua filter (table of functions) to be applied within the
    block element

Returns: the transformed block element

### walk\_inline {#pandoc.walk_inline}

`walk_inline (element, filter)`

Apply a filter inside an inline element, walking its contents.

Parameters:

`element`:
:   the inline element

`filter`:
:   a Lua filter (table of functions) to be applied within the
    inline element

Returns: the transformed inline element

### read {#pandoc.read}

`read (markup[, format])`

Parse the given string into a Pandoc document.

Parameters:

`markup`:
:   the markup to be parsed

`format`:
:   format specification, defaults to `"markdown"`.

Returns: pandoc document

Usage:

    local org_markup = "/emphasis/"  -- Input to be read
    local document = pandoc.read(org_markup, "org")
    -- Get the first block of the document
    local block = document.blocks[1]
    -- The inline element in that block is an `Emph`
    assert(block.content[1].t == "Emph")

# Module pandoc.utils

This module exposes internal pandoc functions and utility
functions.

The module is loaded as part of the `pandoc` module and
available as `pandoc.utils`. In versions up-to and including
pandoc 2.6, this module had to be loaded explicitly. Example:

    pandoc.utils = require 'pandoc.utils'

Use the above for backwards compatibility.

### blocks\_to\_inlines {#pandoc.utils.blocks_to_inlines}

`blocks_to_inlines (blocks[, sep])`

Squash a list of blocks into a list of inlines.

Parameters:

`blocks`:
:   List of [Blocks](#type-block) to be flattened.

`sep`:
:   List of [Inlines](#type-inline) inserted as separator between
    two consecutive blocks; defaults to
    `{ pandoc.Space(), pandoc.Str'¶', pandoc.Space()}`.

Returns:

-   [List](#type-list) of [Inlines](#type-inline)

Usage:

    local blocks = {
      pandoc.Para{ pandoc.Str 'Paragraph1' },
      pandoc.Para{ pandoc.Emph 'Paragraph2' }
    }
    local inlines = pandoc.utils.blocks_to_inlines(blocks)
    -- inlines = {
    --   pandoc.Str 'Paragraph1',
    --   pandoc.Space(), pandoc.Str'¶', pandoc.Space(),
    --   pandoc.Emph{ pandoc.Str 'Paragraph2' }
    -- }

### equals {#pandoc.utils.equals}

`equals (element1, element2)`

Test equality of AST elements. Elements in Lua are considered
equal if and only if the objects obtained by unmarshaling are
equal.

Parameters:

`element1`, `element2`:
:   Objects to be compared. Acceptable input types are [Pandoc],
    [Meta], [MetaValue], [Block], [Inline], [Attr],
    [ListAttributes], and [Citation].

Returns:

-   Whether the two objects represent the same element (boolean)

### from\_simple\_table {#pandoc.utils.from_simple_table}

`from_simple_table (table)`

Creates a [Table] block element from a [SimpleTable]. This is
useful for dealing with legacy code which was written for pandoc
versions older than 2.10.

Returns:

-   table block element ([Table])

Usage:

    local simple = pandoc.SimpleTable(table)
    -- modify, using pre pandoc 2.10 methods
    simple.caption = pandoc.SmallCaps(simple.caption)
    -- create normal table block again
    table = pandoc.utils.from_simple_table(simple)

### make\_sections {#pandoc.utils.make_sections}

`make_sections (number_sections, base_level, blocks)`

Converts list of [Blocks](#type-block) into sections.
`Div`s will be created beginning at each `Header`
and containing following content until the next `Header`
of comparable level.  If `number_sections` is true,
a `number` attribute will be added to each `Header`
containing the section number. If `base_level` is
non-null, `Header` levels will be reorganized so
that there are no gaps, and so that the base level
is the level specified.

Returns:

-   List of [Blocks](#type-block).

Usage:

    local blocks = {
      pandoc.Header(2, pandoc.Str 'first'),
      pandoc.Header(2, pandoc.Str 'second'),
    }
    local newblocks = pandoc.utils.make_sections(true, 1, blocks)

### run\_json\_filter {#pandoc.utils.run_json_filter}

`run_json_filter (doc, filter[, args])`

Filter the given doc by passing it through the a JSON filter.

Parameters:

`doc`:
:   the Pandoc document to filter

`filter`:
:   filter to run

`args`:
:   list of arguments passed to the filter. Defaults to
    `{FORMAT}`.

Returns:

-   ([Pandoc](#type-pandoc)) Filtered document

Usage:

    -- Assumes `some_blocks` contains blocks for which a
    -- separate literature section is required.
    local sub_doc = pandoc.Pandoc(some_blocks, metadata)
    sub_doc_with_bib = pandoc.utils.run_json_filter(
      sub_doc,
      'pandoc-citeproc'
    )
    some_blocks = sub_doc.blocks -- some blocks with bib

### normalize\_date {#pandoc.utils.normalize_date}

`normalize_date (date_string)`

Parse a date and convert (if possible) to "YYYY-MM-DD" format.
We limit years to the range 1601-9999 (ISO 8601 accepts greater
than or equal to 1583, but MS Word only accepts dates starting
1601).

Returns:

-   A date string, or nil when the conversion failed.

### sha1 {#pandoc.utils.sha1}

`sha1 (contents)`

Returns the SHA1 has of the contents.

Returns:

-   SHA1 hash of the contents.

Usage:

    local fp = pandoc.utils.sha1("foobar")

### stringify {#pandoc.utils.stringify}

`stringify (element)`

Converts the given element (Pandoc, Meta, Block, or Inline) into
a string with all formatting removed.

Returns:

-   A plain string representation of the given element.

Usage:

    local inline = pandoc.Emph{pandoc.Str 'Moin'}
    -- outputs "Moin"
    print(pandoc.utils.stringify(inline))

### to\_roman\_numeral {#pandoc.utils.to_roman_numeral}

`to_roman_numeral (integer)`

Converts an integer \< 4000 to uppercase roman numeral.

Returns:

-   A roman numeral string.

Usage:

    local to_roman_numeral = pandoc.utils.to_roman_numeral
    local pandoc_birth_year = to_roman_numeral(2006)
    -- pandoc_birth_year == 'MMVI'

### to\_simple\_table {#pandoc.utils.to_simple_table}

`to_simple_table (table)`

Creates a [SimpleTable] out of a [Table] block.

Returns:

-   a simple table object ([SimpleTable])

Usage:

    local simple = pandoc.utils.to_simple_table(table)
    -- modify, using pre pandoc 2.10 methods
    simple.caption = pandoc.SmallCaps(simple.caption)
    -- create normal table block again
    table = pandoc.utils.from_simple_table(simple)

# Module pandoc.mediabag

The `pandoc.mediabag` module allows accessing pandoc's media
storage. The "media bag" is used when pandoc is called with the
`--extract-media` or (for HTML only) `--self-contained` option.

The module is loaded as part of module `pandoc` and can either
be accessed via the `pandoc.mediabag` field, or explicitly
required, e.g.:

    local mb = require 'pandoc.mediabag'

### delete {#pandoc.mediabag.delete}

`delete (filepath)`

Removes a single entry from the media bag.

Parameters:

`filepath`:
:   filename of the item to be deleted. The media bag will be
    left unchanged if no entry with the given filename exists.

### empty {#pandoc.mediabag.empty}

`empty ()`

Clear-out the media bag, deleting all items.

### insert {#pandoc.mediabag.insert}

`insert (filepath, mime_type, contents)`

Adds a new entry to pandoc's media bag. Replaces any existing
mediabag entry with the same `filepath`.

Parameters:

`filepath`:
:   filename and path relative to the output folder.

`mime_type`:
:   the file's MIME type; use `nil` if unknown or unavailable.

`contents`:
:   the binary contents of the file.

Usage:

    local fp = "media/hello.txt"
    local mt = "text/plain"
    local contents = "Hello, World!"
    pandoc.mediabag.insert(fp, mt, contents)

### items {#pandoc.mediabag.items}

`items ()`

Returns an iterator triple to be used with Lua's generic `for`
statement. The iterator returns the filepath, MIME type, and
content of a media bag item on each invocation. Items are
processed one-by-one to avoid excessive memory use.

This function should be used only when full access to all items,
including their contents, is required. For all other cases,
[`list`](#pandoc.mediabag.list) should be preferred.

Returns:

-   The iterator function; must be called with the iterator
    state and the current iterator value.
-   Iterator state -- an opaque value to be passed to the
    iterator function.
-   Initial iterator value.

Usage:

    for fp, mt, contents in pandoc.mediabag.items() do
      -- print(fp, mt, contents)
    end

### list {#pandoc.mediabag.list}

`list ()`

Get a summary of the current media bag contents.

Returns: A list of elements summarizing each entry in the media
bag. The summary item contains the keys `path`, `type`, and
`length`, giving the filepath, MIME type, and length of contents
in bytes, respectively.

Usage:

    -- calculate the size of the media bag.
    local mb_items = pandoc.mediabag.list()
    local sum = 0
    for i = 1, #mb_items do
        sum = sum + mb_items[i].length
    end
    print(sum)

### lookup {#pandoc.mediabag.lookup}

`lookup (filepath)`

Lookup a media item in the media bag, and return its MIME type
and contents.

Parameters:

`filepath`:
:   name of the file to look up.

Returns:

-   the entry's MIME type, or nil if the file was not found.
-   contents of the file, or nil if the file was not found.

Usage:

    local filename = "media/diagram.png"
    local mt, contents = pandoc.mediabag.lookup(filename)

### fetch {#pandoc.mediabag.fetch}

`fetch (source)`

Fetches the given source from a URL or local file. Returns two
values: the contents of the file and the MIME type (or an empty
string).

The function will first try to retrieve `source` from the
mediabag; if that fails, it will try to download it or read it
from the local file system while respecting pandoc's "resource
path" setting.

Parameters:

`source`:
:   path to a resource; either a local file path or URI

Returns:

-   the entries MIME type, or nil if the file was not found.
-   contents of the file, or nil if the file was not found.

Usage:

    local diagram_url = "https://pandoc.org/diagram.jpg"
    local mt, contents = pandoc.mediabag.fetch(diagram_url)

# Module pandoc.List

This module defines pandoc's list type. It comes with useful
methods and convenience functions.

## Constructor

[`pandoc.List([table])`]{#pandoc.list}

:   Create a new List. If the optional argument `table` is given,
    set the metatable of that value to `pandoc.List`. This is an
    alias for [`pandoc.List:new([table])`](#pandoc.list:new).

## Metamethods

[`pandoc.List:__concat (list)`]{#pandoc.list:__concat}

:   Concatenates two lists.

    Parameters:

    `list`:
    :   second list concatenated to the first

    Returns: a new list containing all elements from list1 and
    list2

## Methods

[`pandoc.List:clone ()`]{#pandoc.list:clone}

:   Returns a (shallow) copy of the list.

[`pandoc.List:extend (list)`]{#pandoc.list:extend}

:   Adds the given list to the end of this list.

    Parameters:

    `list`:
    :   list to appended

[`pandoc.List:find (needle, init)`]{#pandoc.list:find}

:   Returns the value and index of the first occurrence of the
    given item.

    Parameters:

    `needle`:
    :   item to search for

    `init`:
    :   index at which the search is started

    Returns: first item equal to the needle, or nil if no such
    item exists.

[`pandoc.List:find_if (pred, init)`]{#pandoc.list:find_if}

:   Returns the value and index of the first element for which
    the predicate holds true.

    Parameters:

    `pred`:
    :   the predicate function

    `init`:
    :   index at which the search is started

    Returns: first item for which \`test\` succeeds, or nil if
    no such item exists.

[`pandoc.List:filter (pred)`]{#pandoc.list:filter}

:   Returns a new list containing all items satisfying a given
    condition.

    Parameters:

    `pred`:
    :   condition items must satisfy.

    Returns: a new list containing all items for which \`test\`
    was true.

[`pandoc.List:includes (needle, init)`]{#pandoc.list:includes}

:   Checks if the list has an item equal to the given needle.

    Parameters:

    `needle`:
    :   item to search for

    `init`:
    :   index at which the search is started

    Returns: true if a list item is equal to the needle, false
    otherwise

[`pandoc.List:insert ([pos], value)`]{#pandoc.list:insert}

:   Inserts element `value` at position `pos` in list, shifting
    elements to the next-greater index if necessary.

    This function is identical to
    [`table.insert`](https://www.lua.org/manual/5.3/manual.html#6.6).

    Parameters:

    `pos`:
    :   index of the new value; defaults to length of the list + 1

    `value`:
    :   value to insert into the list

[`pandoc.List:map (fn)`]{#pandoc.list:map}

:   Returns a copy of the current list by applying the given
    function to all elements.

    Parameters:

    `fn`:
    :   function which is applied to all list items.

[`pandoc.List:new([table])`]{#pandoc.list:new}

:   Create a new List. If the optional argument `table` is given,
    set the metatable of that value to `pandoc.List`.

    Parameters:

    `table`:
    :   table which should be treatable as a list; defaults to an
        empty table

    Returns: the updated input value

[`pandoc.List:remove ([pos])`]{#pandoc.list:remove}

:    Removes the element at position `pos`, returning the value
     of the removed element.

     This function is identical to
     [`table.remove`](https://www.lua.org/manual/5.3/manual.html#6.6).

    Parameters:

    `pos`:
    :   position of the list value that will be removed; defaults
        to the index of the last element

    Returns: the removed element

[`pandoc.List:sort ([comp])`]{#pandoc.list:sort}

:   Sorts list elements in a given order, in-place. If `comp` is
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
    [`table.sort`](https://www.lua.org/manual/5.3/manual.html#6.6).

    Parameters:

    `comp`:
    :   Comparison function as described above.

# Module pandoc.path

Module for file path manipulations.

## Static Fields {#pandoc.path-fields}

### separator {#pandoc.path.separator}

The character that separates directories.

### search_path_separator {#pandoc.path.search_path_separator}

The character that is used to separate the entries in the `PATH`
environment variable.

## Functions {#pandoc.path-functions}

### directory (filepath) {#pandoc.path.directory}

Gets the directory name, i.e., removes the last directory
separator and everything after from the given path.

Parameters:

filepath
:   path (string)

Returns:

-   The filepath up to the last directory separator. (string)

### filename (filepath) {#pandoc.path.filename}

Get the file name.

Parameters:

filepath
:   path (string)

Returns:

-   File name part of the input path. (string)

### is_absolute (filepath) {#pandoc.path.is_absolute}

Checks whether a path is absolute, i.e. not fixed to a root.

Parameters:

filepath
:   path (string)

Returns:

-   `true` if `filepath` is an absolute path, `false` otherwise.
    (boolean)

### is_relative (filepath) {#pandoc.path.is_relative}

Checks whether a path is relative or fixed to a root.

Parameters:

filepath
:   path (string)

Returns:

-   `true` if `filepath` is a relative path, `false` otherwise.
    (boolean)

### join (filepaths) {#pandoc.path.join}

Join path elements back together by the directory separator.

Parameters:

filepaths
:   path components (list of strings)

Returns:

-   The joined path. (string)

### make_relative (path, root[, unsafe]) {#pandoc.path.make_relative}

Contract a filename, based on a relative path. Note that the
resulting path will usually not introduce `..` paths, as the
presence of symlinks means `../b` may not reach `a/b` if it starts
from `a/c`. For a worked example see [this blog
post](https://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html).

Set `unsafe` to a truthy value to a allow `..` in paths.

Parameters:

path
:   path to be made relative (string)

root
:   root path (string)

unsafe
:   whether to allow `..` in the result. (boolean)

Returns:

-   contracted filename (string)

### normalize (filepath) {#pandoc.path.normalize}

Normalizes a path.

-   `//` makes sense only as part of a (Windows) network drive;
    elsewhere, multiple slashes are reduced to a single
    `path.separator` (platform dependent).
-   `/` becomes `path.separator` (platform dependent)
-   `./` -\> ''
-   an empty path becomes `.`

Parameters:

filepath
:   path (string)

Returns:

-   The normalized path. (string)

### split (filepath) {#pandoc.path.split}

Splits a path by the directory separator.

Parameters:

filepath
:   path (string)

Returns:

-   List of all path components. (list of strings)

### split_extension (filepath) {#pandoc.path.split_extension}

Splits the last extension from a file path and returns the parts. The
extension, if present, includes the leading separator; if the path has
no extension, then the empty string is returned as the extension.

Parameters:

filepath
:   path (string)

Returns:

-   filepath without extension (string)

-   extension or empty string (string)

### split_search_path (search_path) {#pandoc.path.split_search_path}

Takes a string and splits it on the `search_path_separator` character.
Blank items are ignored on Windows, and converted to `.` on Posix. On
Windows path elements are stripped of quotes.

Parameters:

search_path
:   platform-specific search path (string)

Returns:

-   list of directories in search path (list of strings)

# Module pandoc.system

Access to system information and functionality.

## Static Fields

### arch {#pandoc.system.arch}

The machine architecture on which the program is running.

### os {#pandoc.system.os}

The operating system on which the program is running.

## Functions

### environment {#pandoc.system.environment}

`environment ()`

Retrieve the entire environment as a string-indexed table.

Returns:

-   A table mapping environment variables names to their string
    value (table).

### get\_working\_directory {#pandoc.system.get_working_directory}

`get_working_directory ()`

Obtain the current working directory as an absolute path.

Returns:

-   The current working directory (string).

### with\_environment {#pandoc.system.with_environment}

`with_environment (environment, callback)`

Run an action within a custom environment. Only the environment
variables given by `environment` will be set, when `callback` is
called. The original environment is restored after this function
finishes, even if an error occurs while running the callback
action.

Parameters:

`environment`
:   Environment variables and their values to be set before
    running `callback`. (table with string keys and string
    values)

`callback`
:   Action to execute in the custom environment (function)

Returns:

-   The result(s) of the call to `callback`

### with\_temporary\_directory {#pandoc.system.with_temporary_directory}

`with_temporary_directory ([parent_dir,] templ, callback)`

Create and use a temporary directory inside the given directory.
The directory is deleted after the callback returns.

Parameters:

`parent_dir`
:   Parent directory to create the directory in (string). If
    this parameter is omitted, the system's canonical temporary
    directory is used.

`templ`
:   Directory name template (string).

`callback`
:   Function which takes the name of the temporary directory as
    its first argument (function).

Returns:

-   The result of the call to `callback`.

### with\_working\_directory {#pandoc.system.with_working_directory}

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

-   The result(s) of the call to `callback`

# Module pandoc.types

Constructors for types which are not part of the pandoc AST.

### Version {#pandoc.types.version}

`Version (version_specifier)`

Creates a Version object.

Parameters:

`version_specifier`:
:   Version specifier: this can be a version string like
    `'2.7.3'`, a list of integers like `{2, 7, 3}`, a single
    integer, or a [Version].

Returns:

-   A new [Version] object.
