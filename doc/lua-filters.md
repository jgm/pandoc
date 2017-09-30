% Pandoc Lua Filters
% Albert Krewinkel, John MacFarlane
% August 31, 2017

# Introduction

Pandoc has long supported filters, which allow the pandoc
abstract syntax tree (AST) to be manipulated between the parsing
and the writing phase.  Traditional pandoc filters accept a JSON
representation of the pandoc AST and produce an altered JSON
representation of the AST.  They may be written in any
programming language, and invoked from pandoc using the
`--filter` option.

Although traditional filters are very flexible, they have a
couple of disadvantages.  First, there is some overhead in
writing JSON to stdout and reading it from stdin (twice,
once on each side of the filter).  Second, whether a filter
will work will depend on details of the user's environment.
A filter may require an interpreter for a certain programming
language to be available, as well as a library for manipulating
the pandoc AST in JSON form.  One cannot simply provide a filter
that can be used by anyone who has a certain version of the
pandoc executable.

Starting with pandoc 2.0, we have made it possible to write
filters in lua without any external dependencies at all.
A lua interpreter and a lua library for creating pandoc filters
is built into the pandoc executable.  Pandoc data types
are marshalled to lua directly, avoiding the overhead of writing
JSON to stdout and reading it from stdin.

Here is an example of a lua filter that converts strong emphasis
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

This says:  walk the AST, and when you find a Strong element,
replace it with a SmallCaps element with the same content.

To run it, save it in a file, say `smallcaps.lua`, and invoke
pandoc with `--lua-filter=smallcaps.lua`.

Here's a quick performance comparison, using a version of the
pandoc manual, MANUAL.txt, and versions of the same filter
written in compiled Haskell (`smallcaps`) and interpreted Python
(`smallcaps.py`):

| Command                                          | Time  |
|--------------------------------------------------|------:|
| `pandoc MANUAL.txt`                              | 1.01s |
| `pandoc MANUAL.txt --filter ./smallcaps`         | 1.36s |
| `pandoc MANUAL.txt --filter ./smallcaps.py`      | 1.40s |
| `pandoc MANUAL.txt --lua-filter ./smallcaps.lua` | 1.03s |

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

The `--lua-filter` can be supplied multiple times, causing the
filters to be applied sequentially in the order they were given.
If other, non-Lua filters are given as well (via `--filter`),
then those are executed *after* all Lua filters have been
applied.

Pandoc expects each lua file to return a list of filters. The
filters in that list are called sequentially, each on the result
of the previous filter. If there is no value returned by the
filter script, then pandoc will try to generate a single filter
by collecting all top-level functions whose names correspond to
those of pandoc elements (e.g., `Str`, `Para`, `Meta`, or
`Pandoc`).  (That is why the two examples above are equivalent.)

For each filter, the document is traversed and each element
subjected to the filter. Elements for which the filter contains
an entry (i.e. a function of the same name) are passed to lua
element filtering function.  In other words, filter entries will
be called for each corresponding element in the document,
getting the respective element as input.

The return of a filter function must one of the following:

-   nil: this means that the object should remain unchanged.
-   a pandoc object: this must be of the same type as the input
    and will replace the original object.
-   a list of pandoc objects: these will replace the original
    object; the list is merged with the neighbors of the orignal
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

See [module documentation](pandoc-module.html) for a list of pandoc
elements.

The global `FORMAT` is set to the format of the pandoc writer
being used (`html5`, `latex`, etc.), so the behavior of a filter
can be made conditional on the eventual output format.

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

Some filters will require access to certain functions provided
by pandoc. This is currently limited to the `read` function
which allows to parse strings into pandoc documents from within
the lua filter.

# Examples

## Macro substitution.

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

## Default metadata file

This filter causes metadata defined in an external file
(`metadata-file.yaml`) to be used as default values in
a document's metadata:

``` lua
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

```lua
function Meta(m)
  m.date = os.date("%B %e, %Y")
  return m
end
```

## Extracting information about links

This filter prints a table of all the URLs linked to
in the document, together with the number of links to
that URL.

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

``` lua
local vars = {}

function get_vars (meta)
  for k, v in pairs(meta) do
    if v.t == 'MetaInlines' then
      vars["$" .. k .. "$"] = v
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

: \$name\$

Occupation

: \$occupation\$
```

then running `pandoc --lua-filter=meta-vars.lua occupations.md` will output:

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

    Returns: block quote element

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

    Returns: block quote element

[`Div (content[, attr])`]{#Div}

:   Creates a div element

    Parameters:

    `content`:
    :   block content

    `attr`:
    :   element attributes

    Returns: code block element

[`Header (level, content[, attr])`]{#Header}

:   Creates a block quote element.

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

    Returns: block quote element

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

    Returns:

[`Para (content)`]{#Para}

:   Creates a para element.

    Parameters:

    `content`:
    :   inline content

    Returns: block quote element

[`Plain (content)`]{#Plain}

:   Creates a plain element.

    Parameters:

    `content`:
    :   inline content

    Returns: block quote element

[`RawBlock (format, text)`]{#RawBlock}

:   Creates a raw content block of the specified format.

    Parameters:

    `format`:
    :   format of content

    `text`:
    :   string content

    Returns: block quote element

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

    Returns: block quote element

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

## Helpers

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

## Helper Functions

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

[`global_filter ()`]{#global_filter}

:   Use functions defined in the global namespace to create a
    pandoc filter.

    Returns: A list of filter functions

    Usage:

        -- within a file defining a pandoc filter:
        function Str(text)
          return pandoc.Str(utf8.upper(text))
        end

        return {pandoc.global_filter()}
        -- the above is equivallent to
        -- return {{Str = Str}}

# Submodule mediabag

The submodule `mediabag` allows accessing pandoc's media
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

:   Fetches the given source and inserts it into the media bag
    using a SHA1 hash of the content as filename.

    Usage:

        local diagram_url = "https://pandoc.org/diagram.jpg"
        pandoc.mediabag.fetch(diagram_url, ".")
