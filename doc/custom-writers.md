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
document. Two styles of custom writers are supported: classic
custom writers must define rendering functions for each AST
element. New style writers, available since pandoc 2.17.2, must
define just a single function `Writer`, which gets passed the
document and writer options, and then does all rendering.

# Classic style

A writer using the classic style defines rendering functions for
each element of the pandoc AST.

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

# New style

Custom writers using the new style must contain a global function
named `Writer`. Pandoc calls this function with the document and
writer options as arguments, and expects the function to return a
string.

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
