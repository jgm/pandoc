% Pandoc Lua Filters
% Albert Krewinkel, John MacFarlane
% August 21, 2017

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

The element function's output must be an element of the same
type as the input. This means a filter function acting on an
inline element must return an inline, and a block element must
remain a block element after filter application. Pandoc will
throw an error if this condition is violated.

Elements without matching functions are left untouched.

See [module documentation](pandoc-module.html) for a list of pandoc
elements.

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
