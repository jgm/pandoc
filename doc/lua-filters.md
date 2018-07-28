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
and the writing phase. Traditional pandoc filters accept a JSON
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

The way the Lua interpreter is set-up can be controlled by
placing a file `init.lua` in pandoc's data directory. The
default init file loads the `pandoc` and `pandoc.mediabag`
modules:

``` {.lua}
pandoc = require 'pandoc'
pandoc.mediabag = require 'pandoc.mediabag'
```

A common use-case would be to add code to load additional
modules or to alter default modules. E.g., the following snippet
adds all unicode-aware functions defined in the [`text`
module](#module-text) to the default `string` module, prefixed
with the string `uc_`.

``` {.lua}
for name, fn in pairs(require 'text') do
  string['uc_' .. name] = fn
end
```

This makes it possible to apply these functions on strings using
colon syntax (`mystring:uc_upper()`).

# Examples

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

# Module text

UTF-8 aware text manipulation functions, implemented in Haskell.
These are available to any lua filter. However, the module must
be explicitly loaded:

``` {.lua}
-- uppercase all regular text in a document:
text = require 'text'
function Str (s)
  s.text = text.upper(s.text)
  return s
end
```

[`lower (s)`]{#text-lower}

:   Returns a copy of a UTF-8 string, converted to lowercase.

[`upper (s)`]{#text-upper}

:   Returns a copy of a UTF-8 string, converted to uppercase.

[`reverse (s)`]{#text-reverse}

:   Returns a copy of a UTF-8 string, with characters reversed.

[`len (s)`]{#text-len}

:   Returns the length of a UTF-8 string.

[`sub (s)`]{#text-sub}

:   Returns a substring of a UTF-8 string, using lua's string
    indexing rules.

# Module pandoc

Lua functions for pandoc scripts.

## Pandoc Document

[`Pandoc (blocks[, meta])`]{#Pandoc}

:   A complete pandoc document

    Parameters:

    `blocks`:
    :   document content

    `meta`:
    :   document meta data

## Meta

[`Meta (table)`]{#Meta}

:   Create a new Meta object.

    Parameters:

    `table`:
    :   table containing document meta information

## MetaValue

[`MetaBlocks (blocks)`]{#MetaBlocks}

:   Meta blocks

    Parameters:

    `blocks`:
    :   blocks

[`MetaInlines (inlines)`]{#MetaInlines}

:   Meta inlines

    Parameters:

    `inlines`:
    :   inlines

[`MetaList (meta_values)`]{#MetaList}

:   Meta list

    Parameters:

    `meta_values`:
    :   list of meta values

[`MetaMap (key_value_map)`]{#MetaMap}

:   Meta map

    Parameters:

    `key_value_map`:
    :   a string-indexed map of meta values

[`MetaString (str)`]{#MetaString}

:   Creates string to be used in meta data.

    Parameters:

    `str`:
    :   string value

[`MetaBool (bool)`]{#MetaBool}

:   Creates boolean to be used in meta data.

    Parameters:

    `bool`:
    :   boolean value

## Blocks

[`Block`]{#Block}

:   Block elements

[`BlockQuote (content)`]{#BlockQuote}

:   Creates a block quote element

    Parameters:

    `content`:
    :   block content

    Returns: block quote element

[`BulletList (content)`]{#BulletList}

:   Creates a bullet (i.e.

    Parameters:

    `content`:
    :   list of items

    Returns: bullet list element

[`CodeBlock (text[, attr])`]{#CodeBlock}

:   Creates a code block element

    Parameters:

    `text`:
    :   code string

    `attr`:
    :   element attributes

    Returns: code block element

[`DefinitionList (content)`]{#DefinitionList}

:   Creates a definition list, containing terms and their
    explanation.

    Parameters:

    `content`:
    :   list of items

    Returns: definition list element

[`Div (content[, attr])`]{#Div}

:   Creates a div element

    Parameters:

    `content`:
    :   block content

    `attr`:
    :   element attributes

    Returns: div element

[`Header (level, content[, attr])`]{#Header}

:   Creates a header element.

    Parameters:

    `level`:
    :   header level

    `content`:
    :   inline content

    `attr`:
    :   element attributes

    Returns: header element

[`HorizontalRule ()`]{#HorizontalRule}

:   Creates a horizontal rule.

    Returns: horizontal rule

[`LineBlock (content)`]{#LineBlock}

:   Creates a line block element.

    Parameters:

    `content`:
    :   inline content

    Returns: line block element

[`Null ()`]{#Null}

:   Creates a null element.

    Returns: null element

[`OrderedList (items[, listAttributes])`]{#OrderedList}

:   Creates an ordered list.

    Parameters:

    `items`:
    :   list items

    `listAttributes`:
    :   list parameters

    Returns: ordered list element

[`Para (content)`]{#Para}

:   Creates a para element.

    Parameters:

    `content`:
    :   inline content

    Returns: paragraph element

[`Plain (content)`]{#Plain}

:   Creates a plain element.

    Parameters:

    `content`:
    :   inline content

    Returns: plain element

[`RawBlock (format, text)`]{#RawBlock}

:   Creates a raw content block of the specified format.

    Parameters:

    `format`:
    :   format of content

    `text`:
    :   string content

    Returns: raw block element

[`Table (caption, aligns, widths, headers, rows)`]{#Table}

:   Creates a table element.

    Parameters:

    `caption`:
    :   table caption

    `aligns`:
    :   alignments

    `widths`:
    :   column widths

    `headers`:
    :   header row

    `rows`:
    :   table rows

    Returns: table element

## Inline

[`Inline`]{#Inline}

:   Inline element class

[`Cite (content, citations)`]{#Cite}

:   Creates a Cite inline element

    Parameters:

    `content`:
    :   List of inlines

    `citations`:
    :   List of citations

    Returns: citations element

[`Code (text[, attr])`]{#Code}

:   Creates a Code inline element

    Parameters:

    `text`:
    :   brief image description

    `attr`:
    :   additional attributes

    Returns: code element

[`Emph (content)`]{#Emph}

:   Creates an inline element representing emphasised text.

    Parameters:

    `content`:
    :   inline content

    Returns: emphasis element

[`Image (caption, src[, title[, attr]])`]{#Image}

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

    Returns: image element

[`LineBreak ()`]{#LineBreak}

:   Create a LineBreak inline element

    Returns: linebreak element

[`Link (content, target[, title[, attr]])`]{#Link}

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

    Returns: image element

[`Math (mathtype, text)`]{#Math}

:   Creates a Math element, either inline or displayed.

    Parameters:

    `mathtype`:
    :   rendering specifier

    `text`:
    :   Math content

    Returns: Math element

[`DisplayMath (text)`]{#DisplayMath}

:   Creates a DisplayMath element (DEPRECATED).

    Parameters:

    `text`:
    :   Math content

    Returns: Math element

[`InlineMath (text)`]{#InlineMath}

:   Creates an InlineMath inline element (DEPRECATED).

    Parameters:

    `text`:
    :   Math content

    Returns: Math element

[`Note (content)`]{#Note}

:   Creates a Note inline element

    Parameters:

    `content`:
    :   footnote block content

[`Quoted (quotetype, content)`]{#Quoted}

:   Creates a Quoted inline element given the quote type and
    quoted content.

    Parameters:

    `quotetype`:
    :   type of quotes to be used

    `content`:
    :   inline content

    Returns: quoted element

[`SingleQuoted (content)`]{#SingleQuoted}

:   Creates a single-quoted inline element (DEPRECATED).

    Parameters:

    `content`:
    :   inline content

    Returns: quoted element

    See also: [Quoted](#Quoted)

[`DoubleQuoted (content)`]{#DoubleQuoted}

:   Creates a single-quoted inline element (DEPRECATED).

    Parameters:

    `content`:
    :   inline content

    Returns: quoted element

    See also: [Quoted](#Quoted)

[`RawInline (format, text)`]{#RawInline}

:   Creates a RawInline inline element

    Parameters:

    `format`:
    :   format of the contents

    `text`:
    :   string content

    Returns: raw inline element

[`SmallCaps (content)`]{#SmallCaps}

:   Creates text rendered in small caps

    Parameters:

    `content`:
    :   inline content

    Returns: smallcaps element

[`SoftBreak ()`]{#SoftBreak}

:   Creates a SoftBreak inline element.

    Returns: softbreak element

[`Space ()`]{#Space}

:   Create a Space inline element

    Returns: space element

[`Span (content[, attr])`]{#Span}

:   Creates a Span inline element

    Parameters:

    `content`:
    :   inline content

    `attr`:
    :   additional attributes

    Returns: span element

[`Str (text)`]{#Str}

:   Creates a Str inline element

    Parameters:

    `text`:
    :   content

    Returns: string element

[`Strikeout (content)`]{#Strikeout}

:   Creates text which is striked out.

    Parameters:

    `content`:
    :   inline content

    Returns: strikeout element

[`Strong (content)`]{#Strong}

:   Creates a Strong element, whose text is usually displayed in
    a bold font.

    Parameters:

    `content`:
    :   inline content

    Returns: strong element

[`Subscript (content)`]{#Subscript}

:   Creates a Subscript inline element

    Parameters:

    `content`:
    :   inline content

    Returns: subscript element

[`Superscript (content)`]{#Superscript}

:   Creates a Superscript inline element

    Parameters:

    `content`:
    :   inline content

    Returns: strong element

## Element components

[`Attr ([identifier[, classes[, attributes]]])`]{#Attr}

:   Create a new set of attributes (Attr).

    Parameters:

    `identifier`:
    :   element identifier

    `classes`:
    :   element classes

    `attributes`:
    :   table containing string keys and values

    Returns: element attributes

[`Citation (id, mode[, prefix[, suffix[, note_num[, hash]]]])`]{#Citation}

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

## Constants

[`AuthorInText`]{#AuthorInText}

:   Author name is mentioned in the text.

    See also: [Citation](#Citation)

[`SuppressAuthor`]{#SuppressAuthor}

:   Author name is suppressed.

    See also: [Citation](#Citation)

[`NormalCitation`]{#NormalCitation}

:   Default citation style is used.

    See also: [Citation](#Citation)

[`AlignLeft`]{#AlignLeft}

:   Table cells aligned left.

    See also: [Table](#Table)

[`AlignRight`]{#AlignRight}

:   Table cells right-aligned.

    See also: [Table](#Table)

[`AlignCenter`]{#AlignCenter}

:   Table cell content is centered.

    See also: [Table](#Table)

[`AlignDefault`]{#AlignDefault}

:   Table cells are alignment is unaltered.

    See also: [Table](#Table)

[`DefaultDelim`]{#DefaultDelim}

:   Default list number delimiters are used.

    See also: [OrderedList](#OrderedList)

[`Period`]{#Period}

:   List numbers are delimited by a period.

    See also: [OrderedList](#OrderedList)

[`OneParen`]{#OneParen}

:   List numbers are delimited by a single parenthesis.

    See also: [OrderedList](#OrderedList)

[`TwoParens`]{#TwoParens}

:   List numbers are delimited by a double parentheses.

    See also: [OrderedList](#OrderedList)

[`DefaultStyle`]{#DefaultStyle}

:   List are numbered in the default style

    See also: [OrderedList](#OrderedList)

[`Example`]{#Example}

:   List items are numbered as examples.

    See also: [OrderedList](#OrderedList)

[`Decimal`]{#Decimal}

:   List are numbered using decimal integers.

    See also: [OrderedList](#OrderedList)

[`LowerRoman`]{#LowerRoman}

:   List are numbered using lower-case roman numerals.

    See also: [OrderedList](#OrderedList)

[`UpperRoman`]{#UpperRoman}

:   List are numbered using upper-case roman numerals

    See also: [OrderedList](#OrderedList)

[`LowerAlpha`]{#LowerAlpha}

:   List are numbered using lower-case alphabetic characters.

    See also: [OrderedList](#OrderedList)

[`UpperAlpha`]{#UpperAlpha}

:   List are numbered using upper-case alphabetic characters.

    See also: [OrderedList](#OrderedList)

## Helper functions

[`pipe (command, args, input)`]{#pipe}

:   Runs command with arguments, passing it some input, and
    returns the output.

    Returns:

    -   Output of command.

    Raises:

    -   A table containing the keys `command`, `error_code`, and
        `output` is thrown if the command exits with a non-zero
        error code.

    Usage:

        local output = pandoc.pipe("sed", {"-e","s/a/b/"}, "abc")

[`walk_block (element, filter)`]{#walk_block}

:   Apply a filter inside a block element, walking its contents.

    Parameters:

    `element`:
    :   the block element

    `filter`:
    :   a lua filter (table of functions) to be applied within
        the block element

    Returns: the transformed block element

[`walk_inline (element, filter)`]{#walk_inline}

:   Apply a filter inside an inline element, walking its
    contents.

    Parameters:

    `element`:
    :   the inline element

    `filter`:
    :   a lua filter (table of functions) to be applied within
        the inline element

    Returns: the transformed inline element

[`read (markup[, format])`]{#read}

:   Parse the given string into a Pandoc document.

    Parameters:

    `markup`:
    :   the markup to be parsed

    `format`:
    :   format specification, defaults to \"markdown\".

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

[`blocks_to_inlines (blocks[, sep])`]{#utils-blocks_to_inlines}

:   Squash a list of blocks into a list of inlines.

    Parameters:

    `blocks`:
    :   List of blocks to be flattened.

    `sep`:
    :   List of inlines inserted as separator between two
        consecutive blocks; defaults to `{ pandoc.Space(),
        pandoc.Str'¶', pandoc.Space()}`.

    Returns:

    -   ({[Inline][#Inline]}) List of inlines

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

[`hierarchicalize (blocks)`]{#utils-hierarchicalize}

:   Convert list of blocks into an hierarchical list. An
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

    -   List of hierarchical elements

    Usage:

        local blocks = {
          pandoc.Header(2, pandoc.Str 'first'),
          pandoc.Header(2, pandoc.Str 'second'),
        }
        local elements = pandoc.utils.hierarchicalize(blocks)
        print(table.concat(elements[1].numbering, '.')) -- 0.1
        print(table.concat(elements[2].numbering, '.')) -- 0.2

[`run_json_filter (doc, filter[, args])`]{#utils-run_json_filter}

:   Filter the given doc by passing it through the a JSON filter.

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

[`normalize_date (date_string)`]{#utils-normalize_date}

:   Parse a date and convert (if possible) to "YYYY-MM-DD"
    format. We limit years to the range 1601-9999 (ISO 8601
    accepts greater than or equal to 1583, but MS Word only
    accepts dates starting 1601).

    Returns:

    -   A date string, or nil when the conversion failed.

[`sha1 (contents)`]{#utils-sha1}

:   Returns the SHA1 has of the contents.

    Returns:

    -   SHA1 hash of the contents.

    Usage:

        local fp = pandoc.utils.sha1("foobar")

[`stringify (element)`]{#utils-stringify}

:   Converts the given element (Pandoc, Meta, Block, or Inline)
    into a string with all formatting removed.

    Returns:

    -   A plain string representation of the given element.

    Usage:

        local inline = pandoc.Emph{pandoc.Str 'Moin'}
        -- outputs "Moin"
        print(pandoc.utils.stringify(inline))

[`to_roman_numeral (integer)`]{#utils-to_roman_numeral}

:   Converts an integer \< 4000 to uppercase roman numeral.

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

[`insert (filepath, mime_type, contents)`]{#mediabag-insert}

:   Adds a new entry to pandoc's media bag.

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

[`list ()`]{#mediabag-list}

:   Get a summary of the current media bag contents.

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

[`lookup (filepath)`]{#mediabag-lookup}

:   Lookup a media item in the media bag, returning mime type
    and contents.

    Parameters:

    `filepath`:
    :   name of the file to look up.

    Returns:

    -   the entries MIME type, or nil if the file was not found.
    -   contents of the file, or nil if the file was not found.

    Usage:

        local filename = "media/diagram.png"
        local mt, contents = pandoc.mediabag.lookup(filename)

[`fetch (source, base_url)`]{#mediabag-fetch}

:   Fetches the given source from a URL or local file. Returns
    two values: the contents of the file and the mime type (or
    an empty string).

    Returns:

    -   the entries MIME type, or nil if the file was not found.
    -   contents of the file, or nil if the file was not found.

    Usage:

        local diagram_url = "https://pandoc.org/diagram.jpg"
        local contents = pandoc.mediabag.fetch(diagram_url, ".")

# Module pandoc.List

Pandoc\'s List type and helper methods

## Metamethods

[`pandoc.List:__concat (list)`]{#pandoc.List:__concat}

:   Concatenates two lists.

    Parameters:

    `list`:
    :   second list concatenated to the first

    Returns: a new list containing all elements from list1 and
    list2

## Methods

[`pandoc.List:clone ()`]{#pandoc.List:clone}

:   Returns a (shallow) copy of the list.

[`pandoc.List:includes (needle, init)`]{#pandoc.List:includes}

:   Checks if the list has an item equal to the given needle.

    Parameters:

    `needle`:
    :   item to search for

    `init`:
    :   index at which the search is started

    Returns: true if a list item is equal to the needle, false
    otherwise

[`pandoc.List:find (needle, init)`]{#pandoc.List:find}

:   Returns the value and index of the first occurrence of the
    given item.

    Parameters:

    `needle`:
    :   item to search for

    `init`:
    :   index at which the search is started

    Returns: first item equal to the needle, or nil if no such
    item exists.

[`pandoc.List:find_if (pred, init)`]{#pandoc.List:find_if}

:   Returns the value and index of the first element for which
    the predicate holds true.

    Parameters:

    `pred`:
    :   the predicate function

    `init`:
    :   index at which the search is started

    Returns: first item for which \`test\` succeeds, or nil if
    no such item exists.

[`pandoc.List:extend (list)`]{#pandoc.List:extend}

:   Adds the given list to the end of this list.

    Parameters:

    `list`:
    :   list to appended

[`pandoc.List:map (fn)`]{#pandoc.List:map}

:   Returns a copy of the current list by applying the given
    function to all elements.

    Parameters:

    `fn`:
    :   function which is applied to all list items.

[`pandoc.List:filter (pred)`]{#pandoc.List:filter}

:   Returns a new list containing all items satisfying a given
    condition.

    Parameters:

    `pred`:
    :   condition items must satisfy.

    Returns: a new list containing all items for which \`test\`
    was true.
