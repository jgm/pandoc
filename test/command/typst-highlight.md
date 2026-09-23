Highlighted text should be read as a `mark` span, round-tripping with
the Typst writer.

```
% pandoc -f typst -t native
#highlight[hello]
^D
[ Para [ Span ( "" , [ "mark" ] , [] ) [ Str "hello" ] ] ]
```

```
% pandoc -f markdown -t typst
[hello]{.mark}
^D
#highlight[hello]
```

```
% pandoc -f typst -t typst
#highlight[hello]
^D
#highlight[hello]
```

`highlight` may also wrap multiple paragraphs. A pandoc inline cannot
span paragraphs, so the body is split at paragraph breaks and each
paragraph is read as a `mark` span. The same holds for other
inline-styling elements, such as `emph`.

```
% pandoc -f typst -t native
Before.

#highlight[
  Para one.

  Para two.
]
^D
[ Para [ Str "Before." ]
, Para
    [ Span
        ( "" , [ "mark" ] , [] )
        [ SoftBreak , Str "Para" , Space , Str "one." ]
    ]
, Para
    [ Span
        ( "" , [ "mark" ] , [] ) [ Str "Para" , Space , Str "two." ]
    ]
]
```

```
% pandoc -f typst -t typst
Before.

#highlight[
  Para one.

  Para two.
]
^D
Before.

#highlight[ Para one.]

#highlight[Para two.]
```

A `mark` div, e.g. from a `::: mark` block in Markdown, is written by
the Typst writer as a `#highlight[...]`.

```
% pandoc -f markdown -t typst
::: mark
Para one.

Para two.
:::
^D
#highlight[
  Para one.

  Para two.
]
```

A `mark` div is followed by a blank line, so the highlight cannot
swallow the next paragraph on re-reading.

```
% pandoc -f markdown -t typst
::: mark
Para one.
:::

After.
^D
#highlight[
  Para one.
]

After.
```

A `mark` div around a heading is kept; the heading is rendered inside
the highlight.

```
% pandoc -f markdown -t typst
::: mark
## Head

body
:::
^D
#highlight[
  == Head
  <head>
  body
]
```

Highlight bodies may contain math, inline or display, without breaking
the reader.

```
% pandoc -f typst -t native
#highlight[$ a = b $]
^D
[ Para
    [ Span ( "" , [ "mark" ] , [] ) [ Math DisplayMath "a = b" ]
    ]
]
```

```
% pandoc -f typst -t native
#highlight[
  Para.

  $ c = d $
]
^D
[ Para
    [ Span ( "" , [ "mark" ] , [] ) [ SoftBreak , Str "Para." ]
    ]
, Para
    [ Span ( "" , [ "mark" ] , [] ) [ Math DisplayMath "c = d" ]
    ]
]
```
