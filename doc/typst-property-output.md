---
title: Typst property output
author: Gordon Woodhull
---

Pandoc Typst property output
============================

In addition to the output of structural properties built into Pandoc's Typst Writer, the Writer can also output non-structural Typst properties. This is enabled by setting attributes with keys of the form `typst:prop` or `typst:text:prop` on supported elements.


Typst properties
----------------

[Typst](https://typst.app/) allows specification of visual and layout properties as parameters to elements

```typst
#block(fill=orange)[Hello]
```

and set-rules

```typst
#set text(fill=blue); Hello
```

The parameter values are [Typst code](https://typst.app/docs/reference/syntax/#modes) that can use any features of the Typst language.

Pandoc Typst property output
----------------------------

For the set of supported Pandoc elements, the Pandoc Typst Writer will output attributes as parameters to corresponding Typst elements or set-text rules. 

The Typst Writer looks for attributes with keys of the form `typst:prop` or `typst:text:prop` and assumes the values are raw Typst code.

`prop` is the name of the property to set.

For example, `pandoc -f html -t typst` with HTML input

```html
<div typst:inset="10pt">foo</div>
```

produces Typst output

```typst
#block(inset: 10pt)[
foo
]
```

and with HTML input

```html
<div typst:text:fill="purple">foo</div>
```

it produces Typst output

```typst
#block[
#set text(fill: purple); foo
]
```

The Typst Writer does not check the validity of `prop` or the value. Since Typst is a statically typed language, improper property names or values usually result in compilation failure.

Supported elements
------------------

The following Pandoc AST elements are currently supported. More may be supported in the future.

- [Span](https://pandoc.org/lua-filters.html#type-span)

  `typst:text:prop`

  : The content is wrapped in a Typst [text element](https://typst.app/docs/reference/text/text/) with the specified properties set.

- [Div](https://pandoc.org/lua-filters.html#type-div)

  `typst:prop`

  : The `prop` is output as a parameter to the Typst [block element](https://typst.app/docs/reference/layout/block/).

  `typst:text:prop`

  : The `prop` is output as a parameter to a set-text rule at the start of the block content.

- [Table](https://pandoc.org/lua-filters.html#type-table)

  `typst:prop`

  : The `prop` is output as a parameter to the Typst [table element](https://typst.app/docs/reference/model/table/).

  `typst:text:prop`

  : The table is wrapped in a Typst [text element](https://typst.app/docs/reference/text/text/) with `prop` as one of its parameters.

  `typst:no-figure`

  : By default, Pandoc will wrap the table in a Typst [figure element](https://typst.app/docs/reference/model/figure/). If this attribute is set, only the table element itself will be emitted. This avoids Typst's crossreference counter of kind `table` from being incremented.

  `typst:figure:kind`

  : If this attribute is set, Pandoc will wrap the table in a Typst [figure element](https://typst.app/docs/reference/model/figure/) with the specified `kind` attribute. This is useful for tables that should be cross-referenced as something other than `Table ...` in the document. Typst will use the `kind` attribute to increment the corresponding counter: `raw` and `image`.

- Table [Cell](https://pandoc.org/lua-filters.html#type-cell)

  `typst:prop`

  : The `prop` is output as a parameter to the Typst table [cell element](https://typst.app/docs/reference/model/table/#definitions-cell).

  `typst:text:prop`

  : The `prop` is output as a parameter to a set-text rule at the start of the cell content.


Lua filter example
------------------

Here is a minimal example of a Lua filter which translates the CSS [color property](https://developer.mozilla.org/en-US/docs/Web/CSS/color) on a span element to the equivalent [fill parameter](https://typst.app/docs/reference/text/text/#parameters-fill) on a Typst text element.

```lua
function styleToTable(style)
  if not style then return nil end
  local ret = {}
  for clause in style:gmatch('([^;]+)') do
    k,v = clause:match("([%w-]+)%s*:%s*(.*)$")
    ret[k] = v
  end
  return ret
end

function Span(span)
  local style = styleToTable(span.attributes['style'])
  if not style then return end
  if style['color'] then
    span.attributes['typst:text:fill'] = style['color']
  end
  return span
end
```

Given the HTML input

```html
<p>Here is some <span style="color:orange">orange text</span>.</p>
```

the command

```sh
pandoc -f html -t typst --lua-filter ./typst-property-example.lua
```

will produce the Typst output

```typst
Here is some #text(fill: orange)[orange text].
```

Of course, this simple filter will only work for Typst's [predefined colors](https://typst.app/docs/reference/visualize/color/#predefined-colors). A more complete filter would need to translate the value as well.
