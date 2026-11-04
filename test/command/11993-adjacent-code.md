Adjacent inline elements that would run together in the output
must be separated, so they read back as they were written (#11993).

Back-to-back code spans read back as a single span containing
backticks; a zero-width space keeps them apart:

```
% pandoc -f native -t markdown
[Para [Code ("",[],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`​`b)`
```

A zero-width space reads back as a separate `Str`, and attributes on
either span survive:

```
% pandoc -f native -t markdown
[Para [Code ("",[],[]) "a(", Code ("",["y"],[]) "b)"]]
^D
`a(`​`b)`{.y}
```

No separator is needed where the writer already emits one, i.e. the
attribute block after a code span that has attributes:

```
% pandoc -f native -t markdown
[Para [Code ("",["x"],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`{.x}`b)`
```

The same applies to variants without attribute support, such as gfm:

```
% pandoc -f native -t gfm
[Para [Code ("",[],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`​`b)`
```

The same problem arises for emphasis-like elements, whose delimiter
runs would join; emphasis, strong, strikeout and subscript spans get
the same treatment:

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
