---
author:
- Albert Krewinkel
- John MacFarlane
date: 'December 6, 2017'
title: Pandoc Lua Filters
---

# Introduction

Pandoc has long supported filters, which allow the pandoc
abstract syntax tree (AST) to be manipulated between the parsing
and the writing phase. [Traditional pandoc
filters](filters.html) accept a JSON representation of the
pandoc AST and produce an altered JSON representation of the
AST. They may be written in any programming language, and
invoked from pandoc using the `--filter` option.

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

Starting with pandoc 2.0, we have made it possible to write
filters in lua without any external dependencies at all. A lua
interpreter and a lua library for creating pandoc filters is
built into the pandoc executable. Pandoc data types are
marshalled to lua directly, avoiding the overhead of writing
JSON to stdout and reading it from stdin.

Here is an example of a lua filter that converts strong emphasis
to small caps:

``` {.lua}
return {
  {
    Strong = function (elem)
      return pandoc.SmallCaps(elem.c)
    end,
  }
}
```

or equivalently,

``` {.lua}
function Strong(elem)
  return pandoc.SmallCaps(elem.c)
end
```

This says: walk the AST, and when you find a Strong element,
replace it with a SmallCaps element with the same content.

To run it, save it in a file, say `smallcaps.lua`, and invoke
pandoc with `--lua-filter=smallcaps.lua`.

Here's a quick performance comparison, using a version of the
pandoc manual, MANUAL.txt, and versions of the same filter
written in compiled Haskell (`smallcaps`) and interpreted Python
(`smallcaps.py`):

  Command                                               Time
  -------------------------------------------------- -------
  `pandoc MANUAL.txt`                                  1.01s
  `pandoc MANUAL.txt --filter ./smallcaps`             1.36s
  `pandoc MANUAL.txt --filter ./smallcaps.py`          1.40s
  `pandoc MANUAL.txt --lua-filter ./smallcaps.lua`     1.03s

As you can see, the lua filter avoids the substantial overhead
associated with marshalling to and from JSON over a pipe.

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
`--filter` and lua filters specified via `--lua-filter`) in the
order they appear on the command line.

Pandoc expects each lua file to return a list of filters. The
filters in that list are called sequentially, each on the result
of the previous filter. If there is no value returned by the
filter script, then pandoc will try to generate a single filter
by collecting all top-level functions whose names correspond to
those of pandoc elements (e.g., `Str`, `Para`, `Meta`, or
`Pandoc`). (That is why the two examples above are equivalent.)

For each filter, the document is traversed and each element
subjected to the filter. Elements for which the filter contains
an entry (i.e. a function of the same name) are passed to lua
element filtering function. In other words, filter entries will
be called for each corresponding element in the document,
getting the respective element as input.

The return of a filter function must one of the following:

-   nil: this means that the object should remain unchanged.
-   a pandoc object: this must be of the same type as the input
    and will replace the original object.
-   a list of pandoc objects: these will replace the original
    object; the list is merged with the neighbors of the original
    objects (spliced into the list the original object belongs
    to); returning an empty list deletes the object.

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
:   Contains the pandoc version as a numerically indexed table,
    most significant number first. E.g., for pandoc 2.1.1, the
    value of the variable is a table `{2, 1, 1}`. Use
    `table.concat(PANDOC_VERSION, '.')` to produce a version
    string. This variable is also set in custom writers.

`PANDOC_API_VERSION`
:   Contains the version of the pandoc-types API against which
    pandoc was compiled. It is given as a numerically indexed
    table, most significant number first. E.g., if pandoc was
    compiled against pandoc-types 1.17.3, then the value of the
    variable will be a table `{1, 17, 3}`. Use
    `table.concat(PANDOC_API_VERSION, '.')` to produce a version
    string from this table. This variable is also set in custom
    writers.

`PANDOC_SCRIPT_FILE`
:   The name used to involve the filter. This value can be used
    to find files relative to the script file. This variable is
    also set in custom writers.

`PANDOC_STATE`
:   The state shared by all readers and writers. It is used by
    pandoc to collect and pass information. The value of this
    variable is of type [CommonState](#type-ref-CommonState) and
    is read-only.

# Pandoc Module

The `pandoc` lua module is loaded into the filter's lua
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
to use and can be read back from the lua environment.
Internally, pandoc uses these functions to create the lua
objects which are passed to element filter functions. This means
that elements created via this module will behave exactly as
those elements accessible through the filter function parameter.

## Exposed pandoc functionality

Some pandoc functions have been made available in lua:

-   [`walk_block`](#walk_block) and
    [`walk_inline`](#walk_inline) allow filters to be applied
    inside specific block or inline elements;
-   [`read`](#read) allows filters to parse strings into pandoc
    documents;
-   [`pipe`](#pipe) runs an external command with input from and
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
when added to `init.lua`. The snippet adds all unicode-aware
functions defined in the [`text` module] to the default `string`
module, prefixed with the string `uc_`.

``` {.lua}
for name, fn in pairs(require 'text') do
  string['uc_' .. name] = fn
end
```

This makes it possible to apply these functions on strings using
colon syntax (`mystring:uc_upper()`).

[`text` module]: #module-text

# Examples

The following filters are presented as examples.
A repository of useful lua filters (which may also serve
as good examples) is available at
<https://github.com/pandoc/lua-filters>.

## Macro substitution.

The following filter converts the string `{{helloworld}}` into
emphasized text "Hello, World".

``` {.lua}
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

## Default metadata file

This filter causes metadata defined in an external file
(`metadata-file.yaml`) to be used as default values in a
document's metadata:

``` {.lua}
-- read metadata file into string
local metafile = io.open('metadata-file.yaml', 'r')
local content = metafile:read("*a")
metafile:close()
-- get metadata
local default_meta = pandoc.read(content, "markdown").meta

return {
  {
    Meta = function(meta)
      -- use default metadata field if it hasn't been defined yet.
      for k, v in pairs(default_meta) do
        if meta[k] == nil then
          meta[k] = v
        end
      end
      return meta
    end,
  }
```

## Setting the date in the metadata

This filter sets the date in the document's metadata to the
current date:

``` {.lua}
function Meta(m)
  m.date = os.date("%B %e, %Y")
  return m
end
```

## Extracting information about links

This filter prints a table of all the URLs linked to in the
document, together with the number of links to that URL.

``` {.lua}
links = {}

function Link (el)
  if links[el.target] then
    links[el.target] = links[el.target] + 1
  else
    links[el.target] = 1
  end
  return el
end

function Doc (blocks, meta)
  function strCell(str)
    return {pandoc.Plain{pandoc.Str(str)}}
  end
  local caption = {pandoc.Str "Link", pandoc.Space(), pandoc.Str "count"}
  local aligns = {pandoc.AlignDefault, pandoc.AlignLeft}
  local widths = {0.8, 0.2}
  local headers = {strCell "Target", strCell "Count"}
  local rows = {}
  for link, count in pairs(links) do
    rows[#rows + 1] = {strCell(link), strCell(count)}
  end
  return pandoc.Doc(
    {pandoc.Table(caption, aligns, widths, headers, rows)},
    meta
  )
end
```

## Replacing placeholders with their metadata value

Lua filter functions are run in the order

> *Inlines → Blocks → Meta → Pandoc*.

Passing information from a higher level (e.g., metadata) to a
lower level (e.g., inlines) is still possible by using two
filters living in the same file:

``` {.lua}
local vars = {}

function get_vars (meta)
  for k, v in pairs(meta) do
    if v.t == 'MetaInlines' then
      vars["$" .. k .. "$"] = {table.unpack(v)}
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

``` {.markdown}
---
name: Samuel Q. Smith
occupation: Professor of Phrenology
---

Name

: \$name\$

Occupation

: \$occupation\$
```

then running `pandoc --lua-filter=meta-vars.lua occupations.md`
will output:

``` {.html}
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

``` {.lua}
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

``` {.lua}
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

``` {.lua}
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
<http://abcnotation.com>.)

Images are added to the mediabag. For output to binary formats,
pandoc will use images in the mediabag. For textual formats, use
`--extract-media` to specify a directory where the files in the
mediabag will be written, or (for HTML only) use
`--self-contained`.

``` {.lua}
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

## Building images with tikz

This filter converts raw LaTeX tikz environments into images. It
works with both PDF and HTML output. The tikz code is compiled
to an image using `pdflatex`, and the image is converted (if
necessary) from pdf to png format using ImageMagick's `convert`,
so both of these must be in the system path. Converted images
are cached in the working directory and given filenames based on
a hash of the source, so that they need not be regenerated each
time the document is built. (A more sophisticated version of
this might put these in a special cache directory.)

``` {.lua}
local function tikz2image(src, filetype, outfile)
    local tmp = os.tmpname()
    local tmpdir = string.match(tmp, "^(.*[\\/])") or "."
    local f = io.open(tmp .. ".tex", 'w')
    f:write("\\documentclass{standalone}\n\\usepackage{tikz}\n\\begin{document}\n")
    f:write(src)
    f:write("\n\\end{document}\n")
    f:close()
    os.execute("pdflatex -output-directory " .. tmpdir  .. " " .. tmp)
    if filetype == 'pdf' then
        os.rename(tmp .. ".pdf", outfile)
    else
        os.execute("convert " .. tmp .. ".pdf " .. outfile)
    end
    os.remove(tmp .. ".tex")
    os.remove(tmp .. ".pdf")
    os.remove(tmp .. ".log")
    os.remove(tmp .. ".aux")
end

extension_for = {
    html = 'png',
    html4 = 'png',
    html5 = 'png',
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

function RawBlock(el)
    local filetype = extension_for[FORMAT] or "png"
    local fname = pandoc.sha1(el.text) .. "." .. filetype
    if not file_exists(fname) then
        tikz2image(el.text, filetype, fname)
    end
    return pandoc.Para({pandoc.Image({}, fname)})
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
filters. See the [pandoc module](#module-pandoc}) for functions
to create these objects.

## Pandoc {#type-ref-pandoc}

Pandoc document

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

`blocks`
:   document content ([List] of [Block]s)

`meta`
:   document meta information ([Meta] object)


## Meta {#type-ref-meta}

Meta information on a document; string-indexed collection of
[MetaValue]s.

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

## MetaValue {#type-ref-MetaValue}

Document meta information items.

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

### MetaBlocks {#type-ref-MetaBlocks}

A list of blocks usable as meta value ([List] of [Block]s)

Fields:

`tag`, `t`
:   the literal `MetaBlocks` (string)

### MetaBool {#type-ref-MetaBool}

Plain Lua boolean value (boolean)

### MetaInlines {#type-ref-MetaInlines}

List of inlines used in metadata ([List] of [Inline]s)

Fields:

`tag`, `t`
:   the literal `MetaInlines` (string)

### MetaList {#type-ref-iMetaList}

A list of other [MetaValue]s. ([List])

Fields:

`tag`, `t`
:   the literal `MetaList` (string)

### MetaMap {#type-ref-MetaMap}

A string-indexed map of meta-values. (table)

Fields:

`tag`, `t`
:   the literal `MetaMap` (string)

*Note*: The fields will be shadowed if the map contains a field
with the same name as those listed.

### MetaString {#type-ref-MetaString}

Plain Lua string value (string)


## Block {#type-ref-Block}

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

### BlockQuote {#type-ref-BlockQuote}

A block quote element

content:
:   block content ([List] of [Block]s)

`tag`, `t`
:   the literal `BlockQuote` (string)

### BulletList {#type-ref-BulletList}

A bullet list

`content`
:   list of items ([List] of [Block]s)

`tag`, `t`
:   the literal `BulletList` (string)

### CodeBlock {#type-ref-CodeBlock}

Block of code.

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

### DefinitionList {#type-ref-DefinitionList}

Definition list, containing terms and their explanation.

`content`
:   list of items

`tag`, `t`
:   the literal `DefinitionList` (string)

### Div {#type-ref-Div}

Generic block container with attributes

`content`
:   block content ([List] of [Block]s)

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Div` (string)

### Header {#type-ref-Header}

Creates a header element.

`level`
:   header level (integer)

`content`
:   inline content ([List] of [Inline]s)

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Header` (string)


### HorizontalRule {#type-ref-HorizontalRule}

A horizontal rule.

`tag`, `t`
:   the literal `HorizontalRule` (string)

### LineBlock {#type-ref-LineBlock}

A line block, i.e. a list of lines, each separated from the next
by a newline.

`content`
:   inline content

`tag`, `t`
:   the literal `LineBlock` (string)

### Null {#type-ref-Null}

A null element; this element never produces any output in the
target format.

`tag`, `t`
:   the literal `Null` (string)

### OrderedList {#type-ref-OrderedList}

An ordered list.

`content`
:   list items ([List] of [Block]s)

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

### Para {#type-ref-Para}

A paragraph

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Para` (string)

### Plain {#type-ref-Plain}

Plain text, not a paragraph

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Plain` (string)

### RawBlock {#type-ref-RawBlock}

Raw content of a specified format.

`format`
:   format of content (string)

`text`
:   raw content (string)

`tag`, `t`
:   the literal `RawBlock` (string)

### Table {#type-ref-Table}

A table.

`caption`
:   table caption ([List] of [Inline]s)

`aligns`
:   column alignments ([List] of [Alignment]s)

`widths`
:   column widths (number)

`headers`
:   header row ([List] of [table cells])

`rows`
:   table rows ([List] of [List]s of [table cells])

`tag`, `t`
:   the literal `Table` (string)

A [table cell]{#table-cell} is a list of blocks.

*[Alignment]{#Alignment}* is a string value indicating the
horizontal alignment of a table column. `AlignLeft`,
`AlignRight`, and `AlignCenter` leads cell content tob be
left-aligned, right-aligned, and centered, respectively. The
default alignment is `AlignDefault` (often equivalent to
centered).

[Alignment]: #type-ref-Alignment
[table cells]: #type-ref-table-cell

## Inline {#type-ref-Inline}

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

### Cite {#type-ref-Cite}
Citation

`content`
:   ([List] of [Inline]s)

`citations`
:   citation entries ([List] of [citations])

`tag`, `t`
:   the literal `Cite` (string)

### Code {#type-ref-Code}
Inline code

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

### Emph {#type-ref-Emph}
Emphasized text

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Emph` (string)

### Image {#type-ref-Image}
Image:  alt text (list of inlines), target

`attr`
:   attributes ([Attr])

`caption`
:   text used to describe the image ([List] of [Inline]s)

`src`
:   path to the image file (string)

`title`
:   brief image description

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Image` (string)

### LineBreak {#type-ref-LineBreak}
Hard line break

`tag`, `t`
:   the literal `LineBreak` (string)

### Link {#type-ref-Link}
Hyperlink: alt text (list of inlines), target

`attr`
:   attributes ([Attr])

`content`
:   text for this link ([List] of [Inline]s)

`target`
:   the link target (string)

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Link` (string)

### Math {#type-ref-Math}
TeX math (literal)

`mathype`
:   specifier determining whether the math content should be
    shown inline (`InlineMath`) or on a separate line
    (`DisplayMath`) (string)

`text`
:   math content (string)

`tag`, `t`
:   the literal `Math` (string)

### Note {#type-ref-Note}
Footnote or endnote

`content`
:   ([List] of [Block]s)

`tag`, `t`
:   the literal `Note` (string)

### Quoted {#type-ref-Quoted}
Quoted text

`quotetype`
:   type of quotes to be used; one of `SingleQuote` or
    `DoubleQuote` (string)

`content`
:   quoted text ([List] of [Inline]s)

`tag`, `t`
:   the literal `Quoted` (string)

### RawInline {#type-ref-RawInline}
Raw inline

`format`
:   the format of the content (string)

`text`
:   raw content (string)

`tag`, `t`
:   the literal `RawInline` (string)

### SmallCaps {#type-ref-SmallCaps}
Small caps text

`content`
:   ([List] of [Inline]s)

`tag`, `t`
:   the literal `SmallCaps` (string)

### SoftBreak {#type-ref-SoftBreak}
Soft line break

`tag`, `t`
:   the literal `SoftBreak` (string)

### Space {#type-ref-Space}
Inter-word space

`tag`, `t`
:   the literal `Space` (string)

### Span {#type-ref-Span}
Generic inline container with attributes

`attr`
:   attributes ([Attr])

`content`
:   wrapped content ([List] of [Inline]s)

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([List] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Span` (string)

### Str {#type-ref-Str}
Text

`text`
:   content (string)

`tag`, `t`
:   the literal `Str` (string)

### Strikeout {#type-ref-Strikeout}
Strikeout text

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Strikeout` (string)

### Strong {#type-ref-Strong}
Strongly emphasized text

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Strong` (string)

### Subscript {#type-ref-Subscript}
Subscripted text

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Subscript` (string)

### Superscript {#type-ref-Superscript}
Superscripted text

`content`
:   inline content ([List] of [Inline]s)

`tag`, `t`
:   the literal `Superscript` (string)

## Element components

### Attr {#type-ref-Attr}

A set of element attributes

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

`identifier`
:   element identifier (string)

`classes`
:   element classes ([List] of strings)

`attributes`
:   collection of key/value pairs ([Attributes])

### Attributes {#type-ref-Attributes}

List of key/value pairs. Values can be accessed by using keys as
indices to the list table.

### Citation {#type-ref-Citation}

Single citation entry

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

`id`
:   citation identifier, e.g., a bibtex key (string)

`mode`
:   citation mode, one of `AuthorInText`, `SuppressAuthor`, or
    `NormalCitation` (string)

`prefix`
:   citation prefix ([List] of [Inline]s)

`suffix`
:   citation suffix ([List] of [Inline]s)

`note_num`
:   note number (integer)

`hash`
:   hash (integer)

### ListAttributes {#type-ref-ListAttributes}
List attributes

Object equality is determined via
[`pandoc.utils.equals`](#utils-equals).

`start`
:   number of the first list item (integer)

`style`
:   style used for list numbers; possible values are `DefaultStyle`,
    `Example`, `Decimal`, `LowerRoman`, `UpperRoman`,
    `LowerAlpha`, and `UpperAlpha` (string)

`delimiter`
:   delimiter of list numbers; one of `DefaultDelim`, `Period`,
    `OneParen`, and `TwoParens` (string)

## Hierarchical Element {#type-ref-Element}

Hierarchical elements can be either *Sec* (sections) or *Blk*
(blocks). *Blk* elements are treated like [Block]s.

### Sec {#type-ref-Sec}

Section elements used to provide hierarchical information on
document contents.

**Objects of this type are read-only.**

`level`
:   header level (integer)

`numbering`
:   section numbering ([list] of integers)

`attr`
:   header attributes ([Attr])

`label`
:   header content ([list] of [Inline]s)

`contents`
:   list of contents in this section ([list] of [hierarchical element]s)

`tag`, `t`
:   constant `Sec` (string)

[hierarchical element]: #Element

## ReaderOptions {#type-ref-ReaderOptions}

Pandoc reader options

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
:   width (i.e. equivalent number of spaces) of tab stops
    (integer)

`track_changes`
:   track changes setting for docx; one of `AcceptChanges`,
    `RejectChanges`, and `AllChanges` (string)

## CommonState {#type-ref-CommonState}

The state used by pandoc to collect information and make it
available to readers and writers.

`input_files`
:   List of input files from command line ([List] of strings)

`output_file`
:   Output file from command line (string or nil)

`log`
:   A list of log messages in reverse order ([List] of [LogMessage]s)

`request_headers`
:   Headers to add for HTTP requests; table with header names as
    keys and header contents as value (table)

`resource_path`
:   Path to search for resources like included images ([List] of
    strings)

`source_url`
:   Absolute URL or directory of first source file (string or
    nil)

`user_data_dir`
:   Directory to search for data files (string or nil)

`trace`
:   Whether tracing messages are issued (boolean)

`verbosity`
:   Verbosity level; one of `INFO`, `WARNING`, `ERROR` (string)

## LogMessage {#type-ref-LogMessage}

A pandoc log message. Object have no fields, but can be converted
to a string via `tostring`.

[Block]: #type-ref-Block
[List]: #module-pandoc.list
[MetaValue]: #type-ref-MetaValue
[Inline]: #type-ref-Inline
[Attr]: #type-ref-Attr
[Attributes]: #type-ref-Attributes
[citations]: #type-ref-Citation
[LogMessage]: #type-ref-LogMessage

# Module text

UTF-8 aware text manipulation functions, implemented in Haskell.
The module is made available as part of the `pandoc` module via
`pandoc.text`. The text module can also be loaded explicitly:

``` {.lua}
-- uppercase all regular text in a document:
text = require 'text'
function Str (s)
  s.text = text.upper(s.text)
  return s
end
```

### lower {#text-lower}

`lower (s)`

Returns a copy of a UTF-8 string, converted to lowercase.

### upper {#text-upper}

`upper (s)`

Returns a copy of a UTF-8 string, converted to uppercase.

### reverse {#text-reverse}

`reverse (s)`

Returns a copy of a UTF-8 string, with characters reversed.

### len {#text-len}

`len (s)`

Returns the length of a UTF-8 string.

### sub {#text-sub}

`sub (s)`

Returns a substring of a UTF-8 string, using Lua's string
indexing rules.

# Module pandoc

Lua functions for pandoc scripts.

## Pandoc Document

### Pandoc {#Pandoc}

`Pandoc (blocks[, meta])`

A complete pandoc document

Parameters:

`blocks`:
:   list of [Blocks]

`meta`:
:   [Meta] value (see below)

## Metadata

### Meta {#Meta}

`Meta (table)`

Create a new [Meta] object.

Parameters:

`table`:
:   table with string keys and [MetaValue] values

## MetaValue

### MetaBlocks {#MetaBlocks}

`MetaBlocks (blocks)`

Block-level metadata content.

Parameters:

`blocks`:
:   list of [Blocks]

### MetaInlines {#MetaInlines}

`MetaInlines (inlines)`

Inline-level metadata content.

Parameters:

`inlines`:
:   list of [Inlines]

### MetaList {#MetaList}

`MetaList (meta_values)`

List of metadata items.

Parameters:

`meta_values`:
:   list of [MetaValues][MetaValue]

### MetaMap {#MetaMap}

`MetaMap (key_value_map)`

Field/value map of metadata items.

Parameters:

`key_value_map`:
:   a table with string keys and [MetaValue] values

### MetaString {#MetaString}

`MetaString (str)`

String metadata content.

Parameters:

`str`:
:   string value

### MetaBool {#MetaBool}

`MetaBool (bool)`

Boolean metadata content.

Parameters:

`bool`:
:   boolean value

## Blocks {#Blocks}

### BlockQuote {#BlockQuote}

`BlockQuote (content)`

Creates a BlockQuote element

Parameters:

`content`:
:   list of [Blocks]

Returns: BlockQuote element

### BulletList {#BulletList}

`BulletList (content)`

Creates a BulletList element

Parameters:

`content`:
:   list of items (where each item is a list of [Blocks])

Returns: BulletList element

### CodeBlock {#CodeBlock}

`CodeBlock (text[, attr])`

Creates a CodeBlock element

Parameters:

`text`:
:   string (the code)

`attr`:
:   [Attr]{#Attr} (code attributes)

Returns: CodeBlock element

### DefinitionList {#DefinitionList}

`DefinitionList (content)`

Creates a DefinitionList element

Parameters:

`content`:
:   list of items (where each item is a two element list,
    where the first element is a list of [Inlines], the
    term, and the second is a list of lists of [Blocks],
    the definitions)

Returns: DefinitionList element

### Div {#Div}

`Div (content[, attr])`

Creates a Div element

Parameters:

`content`:
:   list of [Blocks]

`attr`:
:   [Attr]{#Attr} (Div attributes)

Returns: Div element

### Header {#Header}

`Header (level, content[, attr])`

Creates a Header element.

Parameters:

`level`:
:   Header level (integer)

`content`:
:   list of [Inlines] (header title)

`attr`:
:   [Attr]{#Attr} (header attributes)

Returns: Header element

### HorizontalRule {#HorizontalRule}

`HorizontalRule ()`

Creates a HorizontalRule element.

Returns: HorizontalRule element

### LineBlock {#LineBlock}

`LineBlock (content)`

Creates a LineBlock element.

Parameters:

`content`:
:   list of lines (where each line is a list of [Inlines])

Returns: LineBlock element

### Null {#Null}

`Null ()`

Creates a Null block element.

Returns: Null element

### OrderedList {#OrderedList}

`OrderedList (items[, listAttributes])`

Creates an OrderedList element.

Parameters:

`items`:
:   list of items (where each item is a list of [Blocks])

`listAttributes`:
:   [ListAttributes]{#ListAttributes}

Returns: OrderedList element

### Para {#Para}

`Para (content)`

Creates a Para element.

Parameters:

`content`:
:   list of [Inlines]

Returns: Para element

### Plain {#Plain}

`Plain (content)`

Creates a Plain element.

Parameters:

`content`:
:   list of [Inlines]

Returns: Plain element

### RawBlock {#RawBlock}

`RawBlock (format, text)`

Creates a RawBlock of the specified format.

Parameters:

`format`:
:   string (format of content, e.g. 'latex')

`text`:
:   string content

Returns: RawBlock element

### Table {#Table}

`Table (caption, aligns, widths, headers, rows)`

Creates a Table element.

Parameters:

`caption`:
:   table caption (list of [Inlines])

`aligns`:
:   alignments (a list of
    `pandoc.AlignDefault`, `pandoc.AlignLeft`, `pandoc.AlignRight`,
    `pandoc.AlignCenter`, one for each column)

`widths`:
:   column widths (a list of floats, one for each column,
    denoting the fraction of the textwidth needed for the
    column, 0.5 = half width; OR an empty list for a
    simple table where cells need not wrap)

`headers`:
:   header row (a list of cells, each cell a list of [Blocks])

`rows`:
:   table rows (a list of rows, each row a list of cells,
    each cell a list of [Blocks])

Returns: Table element

## Inlines {#Inlines}

### Cite {#Cite}

`Cite (content, citations)`

Creates a Cite element

Parameters:

`content`:
:   list of [Inlines]

`citations`:
:   list of [Citation]s

Returns: citations element

### Code {#Code}

`Code (text[, attr])`

Creates a Code inline element

Parameters:

`text`:
:   string (the code)

`attr`:
:   [Attr]{#Attr} (code attributes)

Returns: Code element

### Emph {#Emph}

`Emph (content)`

Creates an Emph inline element

Parameters:

`content`:
:   list of [Inlines]

Returns: Emph element

### Image {#Image}

`Image (alt, src[, title[, attr]])`

Creates a Image inline element

Parameters:

`alt`:
:   list of [Inlines]: alt text (or, for implicit figures,
    caption)

`src`:
:   string: path to the image file

`title`:
:   string: title attribute

`attr`:
:   [Attr]{#Attr}: additional image attributes

Returns: Image element

### LineBreak {#LineBreak}

`LineBreak ()`

Create a LineBreak inline element

Returns: linebreak element

### Link {#Link}

`Link (content, target[, title[, attr]])`

Creates a Link inline element

Parameters:

`content`:
:   list of [Inlines]: the linked text

`target`:
:   string: the link target

`title`:
:   string: the title attribute

`attr`:
:   [Attr]{#Attr}: additional link attributes

Returns: image element

### Math {#Math}

`Math (mathtype, text)`

Creates a Math inline element, either inline or displayed.

Parameters:

`mathtype`:
:   either `pandoc.InlineMath` or `pandoc.DisplayMath`

`text`:
:   string: raw tex math

Returns: Math element

### DisplayMath {#DisplayMath}

`DisplayMath (text)`

Creates a DisplayMath element (DEPRECATED, use `Math`).

Parameters:

`text`:
:   string: raw tex math

Returns: Math element

### InlineMath {#InlineMath}

`InlineMath (text)`

Creates an InlineMath inline element (DEPRECATED, use
[Math]{#Math}).

Parameters:

`text`:
:   string: raw tex math

Returns: Math element

### Note {#Note}

`Note (content)`

Creates a Note inline element

Parameters:

`content`:
:   list of [Blocks] (content of footnote)

### Quoted {#Quoted}

`Quoted (quotetype, content)`

Creates a Quoted inline element

Parameters:

`quotetype`:
:   either `pandoc.DoubleQuote` or `pandoc.SingleQuote`

`content`:
:   list of [Inlines]

Returns: Quoted element

### SingleQuoted {#SingleQuoted}

`SingleQuoted (content)`

Creates a single-quoted inline element (DEPRECATED, use [Quoted]{#Quoted}).

Parameters:

`content`:
:   list of [Inlines]

Returns: Quoted element

### DoubleQuoted {#DoubleQuoted}

`DoubleQuoted (content)`

Creates a double-quoted inline element (DEPRECATED, use [Quoted]{#Quoted}).

Parameters:

`content`:
:   list of [Inlines]

Returns: Quoted element

### RawInline {#RawInline}

`RawInline (format, text)`

Creates a RawInline inline element

Parameters:

`format`:
:   string (format of the contents)

`text`:
:   string (content)

Returns: RawInline element

### Smallcaps {#SmallCaps}

`SmallCaps (content)`

Creates text rendered in small caps

Parameters:

`content`:
:   list of [Inlines]

Returns: SmallCaps element

### SoftBreak {#SoftBreak}

`SoftBreak ()`

Creates a SoftBreak inline element.

Returns: SoftBreak element

### Space {#Space}

`Space ()`

Create a Space inline element

Returns: Space element

### Span {#Span}

`Span (content[, attr])`

Creates a Span inline element

Parameters:

`content`:
:   list of [Inlines]

`attr`:
:   [Attr]{#Attr}: span attributes

Returns: Span element

### Str {#Str}

`Str (text)`

Creates a Str inline element

Parameters:

`text`:
:   string

Returns: String element

### Strikeout {#Strikeout}

`Strikeout (content)`

Creates a Strikeout inline element

Parameters:

`content`:
:   list of [Inlines]

Returns: Strikeout element

### Strong {#Strong}

`Strong (content)`

Creates a Strong inline element.

Parameters:

`content`:
:   list of [Inlines]

Returns: Strong element

### Subscript {#Subscript}

`Subscript (content)`

Creates a Subscript inline element

Parameters:

`content`:
:   list of [Inlines]

Returns: Subscript element

### Superscript {#Superscript}

`Superscript (content)`

Creates a Superscript inline element

Parameters:

`content`:
:   list of [Inlines]

Returns: Superscript element

## Element components

### Attr {#Attr}

`Attr ([identifier[, classes[, attributes]]])`

Create a new set of attributes (Attr).

Parameters:

`identifier`:
:   string: element identifier

`classes`:
:   list of strings: classes

`attributes`:
:   table containing string keys and values

Returns: Attr

### Citation {#Citation}

`Citation (id, mode[, prefix[, suffix[, note_num[, hash]]]])`

Creates a single Citation.

Parameters:

`id`:
:   string citation identifier (like a bibtex key)

`mode`:
:   `pandoc.AuthorInText`, `pandoc.SuppressAuthor`, or
    `pandoc.NormalCitation`

`prefix`:
:   list of [Inlines] for citation prefix

`suffix`:
:   list of [Inlines] for citation suffix

`note_num`:
:   int: note number

`hash`:
:   int: hash number

### ListAttributes {#ListAttributes}

`ListAttributes ([start[, style[, delimiter]]])`

Creates a set of list attributes

Parameters:

`start`:
:   int: number of the first list item (default: 1)

`style`:
:   `pandoc.DefaultStyle` (default), `pandoc.Decimal`,
    `pandoc.LowerRoman`, `pandoc.UpperRoman`, `pandoc.LowerAlpha`,
    or `pandoc.UpperAlpha`

`delimiter`:
:   `pandoc.DefaultDelim` (default), `pandoc.Period`,
    `pandoc.OneParen`, `pandoc.TwoParens`

Returns: list attributes table

## Helper functions

### pipe {#pipe}

`pipe (command, args, input)`

Runs command with arguments, passing it some input, and
returns the output.

Returns:

-   Output of command.

Raises:

-   A table containing the keys `command`, `error_code`, and
    `output` is thrown if the command exits with a non-zero
    error code.

Usage:

    local output = pandoc.pipe("sed", {"-e","s/a/b/"}, "abc")

### walk_block {#walk_block}

`walk_block (element, filter)`

Apply a filter inside a block element, walking its contents.

Parameters:

`element`:
:   the block element

`filter`:
:   a lua filter (table of functions) to be applied within
    the block element

Returns: the transformed block element

### walk_inline {#walk_inline}

`walk_inline (element, filter)`

Apply a filter inside an inline element, walking its
contents.

Parameters:

`element`:
:   the inline element

`filter`:
:   a lua filter (table of functions) to be applied within
    the inline element

Returns: the transformed inline element

### read {#read}

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

The module is loaded as part of the `pandoc` module and available
as `pandoc.utils`. In versions up-to and including pandoc 2.6,
this module had to be loaded explicitly. Example:

    local utils = require 'pandoc.utils'

Use this for backwards compatibility.

### blocks\_to\_inlines {#utils-blocks_to_inlines}

`blocks_to_inlines (blocks[, sep])`

Squash a list of blocks into a list of inlines.

Parameters:

`blocks`:
:   List of [Blocks] to be flattened.

`sep`:
:   List of [Inlines] inserted as separator between two
    consecutive blocks; defaults to `{ pandoc.Space(),
    pandoc.Str'¶', pandoc.Space()}`.

Returns:

-   List of [Inlines]

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

### equals {#utils-equals}

`equals (element1, element2)`

Test equality of AST elements. Elements in Lua are considered
equal if and only if the objects obtained by unmarshaling are
equal.

Parameters:

`element1`, `element2`:
:   Objects to be compared. Acceptable input types are
    [Pandoc](#type-ref-pandoc), [Meta](#type-ref-meta),
    [MetaValue](#type-ref-MetaValue),
    [Block](#type-ref-Block), [Inline](#type-ref-Inline),
    [Attr](#type-ref-Attr),
    [ListAttributes](#type-ref-ListAttributes), and
    [Citation](#type-ref-Citation).

Returns:

-   Whether the two objects represent the same element
    (boolean)

### hierarchicalize {#utils-hierarchicalize}

`hierarchicalize (blocks)`

Convert list of [Blocks] into an hierarchical list. An
hierarchical elements is either a normal block (but no
Header), or a `Sec` element. The latter has the following
fields:

-   level: level in the document hierarchy;
-   numbering: list of integers of length `level`,
    specifying the absolute position of the section in the
    document;
-   attr: section attributes (see [Attr](#Attr));
-   contents: nested list of hierarchical elements.

Returns:

-   List of hierarchical elements.

Usage:

    local blocks = {
      pandoc.Header(2, pandoc.Str 'first'),
      pandoc.Header(2, pandoc.Str 'second'),
    }
    local elements = pandoc.utils.hierarchicalize(blocks)
    print(table.concat(elements[1].numbering, '.')) -- 0.1
    print(table.concat(elements[2].numbering, '.')) -- 0.2

### run\_json\_filter {#utils-run_json_filter}

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

-   ([Pandoc](#Pandoc)) Filtered document

Usage:

    -- Assumes `some_blocks` contains blocks for which a
    -- separate literature section is required.
    local sub_doc = pandoc.Pandoc(some_blocks, metadata)
    sub_doc_with_bib = pandoc.utils.run_json_filter(
      sub_doc,
      'pandoc-citeproc'
    )
    some_blocks = sub_doc.blocks -- some blocks with bib

### normalize_date {#utils-normalize_date}

`normalize_date (date_string)`

Parse a date and convert (if possible) to "YYYY-MM-DD"
format. We limit years to the range 1601-9999 (ISO 8601
accepts greater than or equal to 1583, but MS Word only
accepts dates starting 1601).

Returns:

-   A date string, or nil when the conversion failed.

### sha1 {#utils-sha1}

`sha1 (contents)`

Returns the SHA1 has of the contents.

Returns:

-   SHA1 hash of the contents.

Usage:

    local fp = pandoc.utils.sha1("foobar")

### stringify {#utils-stringify}

`stringify (element)`

Converts the given element (Pandoc, Meta, Block, or Inline)
into a string with all formatting removed.

Returns:

-   A plain string representation of the given element.

Usage:

    local inline = pandoc.Emph{pandoc.Str 'Moin'}
    -- outputs "Moin"
    print(pandoc.utils.stringify(inline))

### to\_roman\_numeral {#utils-to_roman_numeral}

`to_roman_numeral (integer)`

Converts an integer \< 4000 to uppercase roman numeral.

Returns:

-   A roman numeral string.

Usage:

    local to_roman_numeral = pandoc.utils.to_roman_numeral
    local pandoc_birth_year = to_roman_numeral(2006)
    -- pandoc_birth_year == 'MMVI'

# Module pandoc.mediabag

The `pandoc.mediabag` module allows accessing pandoc's media
storage. The "media bag" is used when pandoc is called with the
`--extract-media` or `--standalone`/`-s` option.

The module is loaded as part of module `pandoc` and can either be
accessed via the `pandoc.mediabag` field, or explicitly required,
e.g.:


    local mb = require 'pandoc.mediabag'

### insert {#mediabag-insert}

`insert (filepath, mime_type, contents)`

Adds a new entry to pandoc's media bag.

Parameters:

`filepath`:
:   filename and path relative to the output folder.

`mime_type`:
:   the file's MIME type

`contents`:
:   the binary contents of the file.

Usage:

    local fp = "media/hello.txt"
    local mt = "text/plain"
    local contents = "Hello, World!"
    pandoc.mediabag(fp, mt, contents)

### list {#mediabag-list}

`list ()`

Get a summary of the current media bag contents.

Returns: A list of elements summarizing each entry in the
media bag. The summary item contains the keys `path`,
`type`, and `length`, giving the filepath, MIME type, and
length of contents in bytes, respectively.

Usage:

    -- calculate the size of the media bag.
    local mb_items = pandoc.mediabag.list()
    local sum = 0
    for i = 1, #mb_items:
        sum = sum + mb_items[i].length
    end
    print(sum)

### lookup {#mediabag-lookup}

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

### fetch {#mediabag-fetch}

`fetch (source, base_url)`

Fetches the given source from a URL or local file. Returns
two values: the contents of the file and the MIME type (or
an empty string).

Returns:

-   the entries MIME type, or nil if the file was not found.
-   contents of the file, or nil if the file was not found.

Usage:

    local diagram_url = "https://pandoc.org/diagram.jpg"
    local contents = pandoc.mediabag.fetch(diagram_url, ".")


# Module pandoc.List

Pandoc's List type and helper methods.

This module is loaded and available as `pandoc.List`. Older
versions up-to and including pandoc 2.6 require the module to be
loaded explicitly. Example:

    local List = require 'pandoc.List'

The above remains the recommended method to use this module; it
provides the List type under an idiomatic name and is fully
backwards compatible.

## Metamethods

### concat {#pandoc.List:__concat}

`pandoc.List:__concat (list)`

Concatenates two lists.

Parameters:

`list`:
:   second list concatenated to the first

Returns: a new list containing all elements from list1 and
list2

## Methods

### clone {#pandoc.List:clone}

`pandoc.List:clone ()` {#pandoc.List:clone}

Returns a (shallow) copy of the list.

### includes {#pandoc.List:includes}

`pandoc.List:includes (needle, init)`

Checks if the list has an item equal to the given needle.

Parameters:

`needle`:
:   item to search for

`init`:
:   index at which the search is started

Returns: true if a list item is equal to the needle, false
otherwise

### find {#pandoc.List:find}

`pandoc.List:find (needle, init)`

Returns the value and index of the first occurrence of the
given item.

Parameters:

`needle`:
:   item to search for

`init`:
:   index at which the search is started

Returns: first item equal to the needle, or nil if no such
item exists.

### find_if {#pandoc.List:find_if}

`pandoc.List:find_if (pred, init)`

Returns the value and index of the first element for which
the predicate holds true.

Parameters:

`pred`:
:   the predicate function

`init`:
:   index at which the search is started

Returns: first item for which \`test\` succeeds, or nil if
no such item exists.

### extend {#pandoc.List:extend}

`pandoc.List:extend (list)`

Adds the given list to the end of this list.

Parameters:

`list`:
:   list to appended

### map {#pandoc.List:map}

`pandoc.List:map (fn)`

Returns a copy of the current list by applying the given
function to all elements.

Parameters:

`fn`:
:   function which is applied to all list items.

### filter {#pandoc.List:filter}

`pandoc.List:filter (pred)`

Returns a new list containing all items satisfying a given
condition.

Parameters:

`pred`:
:   condition items must satisfy.

Returns: a new list containing all items for which \`test\`
was true.
