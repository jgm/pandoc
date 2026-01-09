---
author:
- John MacFarlane
date: 'November 21, 2021'
title: Creating Custom Pandoc Writers in Lua
---

# Introduction

If you need to render a format not already handled by pandoc,
or you want to change how pandoc renders a format,
you can create a custom writer using the [Lua] language.
Pandoc has a built-in Lua interpreter, so you needn't
install any additional software to do this.

[Lua]: https://www.lua.org

A custom writer is a Lua file that defines how to render the
document. Writers must define just a single function, named either
`Writer` or `ByteStringWriter`, which gets passed the document and
writer options, and then handles the conversion of the document,
rendering it into a string. This interface was introduced in
pandoc 2.17.2, with ByteString writers becoming available in
pandoc 3.0.

Pandoc also supports "classic" custom writers, where a Lua
function must be defined for each AST element type. Classic style
writers are *deprecated* and should be replaced with new-style
writers if possible.

# Writers

Custom writers using the new style must contain a global function
named `Writer` or `ByteStringWriter`. Pandoc calls this function
with the document and writer options as arguments, and expects the
function to return a UTF-8 encoded string.

``` lua
function Writer (doc, opts)
  -- ...
end
```

Writers that do not return text but binary data should define a
function with name `ByteStringWriter` instead. The function must
still return a string, but it does not have to be UTF-8 encoded
and can contain arbitrary binary data.

If both `Writer` and `ByteStringWriter` functions are defined,
then only the `Writer` function will be used.

## Format extensions

Writers can be customized through format extensions, such as
`smart`, `citations`, or `hard_line_breaks`. The global
`Extensions` table indicates supported extensions with a
key. Extensions enabled by default are assigned a true value,
while those that are supported but disabled are assigned a false
value.

Example: A writer with the following global table supports the
extensions `smart`, `citations`, and `foobar`, with `smart` enabled and
the others disabled by default:

``` lua
Extensions = {
  smart = true,
  citations = false,
  foobar = false
}
```

The users control extensions as usual, e.g., `pandoc -t
my-writer.lua+citations`. The extensions are accessible through
the writer options' `extensions` field, e.g.:

``` lua
function Writer (doc, opts)
  print(
    'The citations extension is',
    opts.extensions:includes 'citations' and 'enabled' or 'disabled'
  )
  -- ...
end
```

## Default template

The default template of a custom writer is defined by the return
value of the global function `Template`. Pandoc uses the default
template for rendering when the user has not specified a template,
but invoked with the `-s`/`--standalone` flag.

The `Template` global can be left undefined, in which case pandoc
will throw an error when it would otherwise use the default
template.

## Example: modified Markdown writer

Writers have access to all modules described in the [Lua filters
documentation][]. This includes `pandoc.write`, which can be used
to render a document in a format already supported by pandoc. The
document can be modified before this conversion, as demonstrated
in the following short example. It renders a document as GitHub
Flavored Markdown, but always uses fenced code blocks, never
indented code.

``` lua
function Writer (doc, opts)
  local filter = {
    CodeBlock = function (cb)
      -- only modify if code block has no attributes
      if cb.attr == pandoc.Attr() then
        local delimited = '```\n' .. cb.text .. '\n```'
        return pandoc.RawBlock('markdown', delimited)
      end
    end
  }
  return pandoc.write(doc:walk(filter), 'gfm', opts)
end

Template = pandoc.template.default 'gfm'
```

[Lua filters documentation]: https://pandoc.org/lua-filters.html

## Reducing boilerplate with `pandoc.scaffolding.Writer`

The `pandoc.scaffolding.Writer` structure is a custom writer scaffold
that serves to avoid common boilerplate code when defining a custom
writer. The object can be used as a function and allows to skip details
like metadata and template handling, requiring only the render functions
for each AST element type.

The value of `pandoc.scaffolding.Writer` is a function that should
usually be assigned to the global `Writer`:

``` lua
Writer = pandoc.scaffolding.Writer
```

The render functions for Block and Inline values can then be added
to `Writer.Block` and `Writer.Inline`, respectively. The functions
are passed the element and the WriterOptions.

``` lua
Writer.Inline.Str = function (str)
  return str.text
end
Writer.Inline.SoftBreak = function (_, opts)
  return opts.wrap_text == "wrap-preserve"
    and cr
    or space
end
Writer.Inline.LineBreak = cr

Writer.Block.Para = function (para)
  return {Writer.Inlines(para.content), pandoc.layout.blankline}
end
```

The render functions must return a string, a pandoc.layout *Doc*
element, or a list of such elements. In the latter case, the
values are concatenated as if they were passed to
`pandoc.layout.concat`. If the value does not depend on the input,
a constant can be used as well.

The tables `Writer.Block` and `Writer.Inline` can be used as
functions; they apply the right render function for an element of
the respective type. E.g., `Writer.Block(pandoc.Para 'x')` will
delegate to the `Writer.Para` render function and will return the
result of that call.

Similarly, the functions `Writer.Blocks` and `Writer.Inlines` can
be used to render lists of elements, and `Writer.Pandoc` renders
the document's blocks. The function `Writer.Blocks` can take a
separator as an optional second argument, e.g.,
`Writer.Blocks(blks, pandoc.layout.cr)`; the default block
separator is `pandoc.layout.blankline`.

All predefined functions can be overwritten when needed.

The resulting Writer uses the render functions to handle metadata
values and converts them to template variables. The template is
applied automatically if one is given.

# Classic style

A writer using the classic style defines rendering functions for
each element of the pandoc AST. Note that this style is
*deprecated* and may be removed in later versions.

For example,

``` lua
function Para(s)
  return "<paragraph>" .. s .. "</paragraph>"
end
```

## Template variables

New template variables can be added, or existing ones
modified, by returning a second value from function `Doc`.

For example, the following will add the current date in
variable `date`, unless `date` is already defined as either a
metadata value or a variable:

``` lua
function Doc (body, meta, vars)
  vars.date = vars.date or meta.date or os.date '%B %e, %Y'
  return body, vars
end
```

## Changes in pandoc 3.0

Custom writers were reworked in pandoc 3.0. For technical reasons,
the global variables `PANDOC_DOCUMENT` and `PANDOC_WRITER_OPTIONS`
are set to the empty document and default values, respectively.
The old behavior can be restored by adding the following snippet,
which turns a classic into a new style writer.

``` lua
function Writer (doc, opts)
  PANDOC_DOCUMENT = doc
  PANDOC_WRITER_OPTIONS = opts
  loadfile(PANDOC_SCRIPT_FILE)()
  return pandoc.write_classic(doc, opts)
end
```
