Adjacent code inlines must not be written as back-to-back code
spans, which read back as a single span containing backticks
(#11993). Markdown has no way to write two adjacent code spans, so
the spans are merged.

```
% pandoc -f native -t gfm
[Para [Code ("",[],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(b)`
```

```
% pandoc -f native -t markdown
[Para [Code ("",[],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(b)`
```

If the first span's attributes are written after it, the spans are
already separated and no merge is needed:

```
% pandoc -f native -t markdown
[Para [Code ("",["x"],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(`{.x}`b)`
```

The gfm writer writes no code attributes, so it always merges:

```
% pandoc -f native -t gfm
[Para [Code ("",["x"],[]) "a(", Code ("",[],[]) "b)"]]
^D
`a(b)`
```
