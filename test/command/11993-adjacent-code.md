Adjacent inline elements that would run together in the output
must be separated, so they read back as they were written (#11993).

Back-to-back code spans would render as a single code span, so
we insert a zero-width space to keep them apart:

```
% pandoc -f native -t markdown
[Para [Code ("",[],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`​`b)`
```

No separator is needed when the first code span has attributes:

```
% pandoc -f native -t markdown
[Para [Code ("",["x"],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`{.x}`b)`
```

In formats that don't support attributes on inline code, such as gfm,
we'll need the zero-width space again:

```
% pandoc -f native -t gfm
[Para [Code ("",["x"],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`​`b)`
```

The same problem arises for emphasis-like elements, whose delimiter
runs would join; emphasis, strong, strikeout, subscript, and superscript
spans get the same treatment:

```
% pandoc -f native -t markdown
[Para [Emph [Str "hi"], Emph [Str "hi"]]]
^D
*hi*​*hi*
```

```
% pandoc -f native -t gfm
[Para [Emph [Str "hi"], Emph [Str "hi"]]]
^D
*hi*​*hi*
```

```
% pandoc -f native -t markdown
[Para [Strong [Str "hi"], Strong [Str "hi"]]]
^D
**hi**​**hi**
```

```
% pandoc -f native -t markdown
[Para [Strikeout [Str "hi"], Strikeout [Str "hi"]]]
^D
~~hi~~​~~hi~~
```

```
% pandoc -f native -t markdown
[Para [Subscript [Str "hi"], Subscript [Str "hi"]]]
^D
~hi~​~hi~
```

```
% pandoc -f native -t markdown
[Para [Superscript [Str "hi"], Superscript [Str "hi"]]]
^D
^hi^​^hi^
```

Reading a separated pair back yields the two elements with a
zero-width space between them:

```
% pandoc -f markdown -t native
`a(`​`b)`{.y}
^D
[ Para
    [ Code ( "" , [] , [] ) "a("
    , Str "\8203"
    , Code ( "" , [ "y" ] , [] ) "b)"
    ]
]
```
