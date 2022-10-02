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
document. Writers must define just a single function named
`Writer`, which gets passed the document and writer options, and
then handles the conversion of the document, rendering it into a
string. This interface was introduced in pandoc 2.17.2.

Pandoc also supports "classic" custom writers, where a Lua
function must be defined for each AST element type. Classic style
writers are *deprecated* and should be replaced with new-style
writers if possible.

# Writers

Custom writers using the new style must contain a global function
named `Writer`. Pandoc calls this function with the document and
writer options as arguments, and expects the function to return a
UTF-8 encoded string.

``` lua
function Writer (doc, opts)
  -- ...
end
```

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
```

[Lua filters documentation]: https://pandoc.org/lua-filters.html

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

The best way to go about creating a classic custom writer is to
modify the example that comes with pandoc. To get the example,
you can do

```
pandoc --print-default-data-file sample.lua > sample.lua
```

## A custom HTML writer

`sample.lua` is a full-features HTML writer, with explanatory
comments. To use it, just use the path to the custom writer as
the writer name:

```
pandoc -t sample.lua myfile.md
```

`sample.lua` defines all the functions needed by any custom
writer, so you can design your own custom writer by modifying
the functions in `sample.lua` according to your needs.

``` {.lua include="sample.lua"}
```

## Template variables

New template variables can be added, or existing ones
modified, by returning a second value from function `Doc`.

For example, the following will add the current date in
variable `date`, unless `date` is already defined as either a
metadata value or a variable:

``` lua
function Doc (body, meta, vars)
  vars.date = vars.date or meta.data or os.date '%B %e, %Y'
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
