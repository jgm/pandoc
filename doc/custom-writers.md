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

A custom writer is a Lua file that defines functions for
rendering each element of a pandoc AST.

For example,

``` lua
function Para(s)
  return "<paragraph>" .. s .. "</paragraph>"
end
```

The best way to go about creating a custom writer is to modify
the example that comes with pandoc.  To get the example, you
can do

```
pandoc --print-default-data-file sample.lua > sample.lua
```

`sample.lua` is a full-features HTML writer, with explanatory
comments. To use it, just use the path to the custom writer as
the writer name:

```
pandoc -t sample.lua myfile.md
```

`sample.lua` defines all the functions needed by any custom
writer, so you can design your own custom writer by modifying
the functions in `sample.lua` according to your needs.

