Link titles and destinations must be escaped so that links read
back (#11994).

A title containing `"` must have it escaped, or gfm and commonmark
readers do not recognize the link at all:

```
% pandoc -f native -t gfm
[Para [Link ("",[],[]) [Str "x"] ("https://example.test","Opens the \"spec\"")]]
^D
[x](https://example.test "Opens the \"spec\"")
```

Readers decode character references in link destinations, so `&para;`
is not a literal `&` there:

```
% pandoc -f gfm -t native
[x](https://example.test/?q=one&para;=two)
^D
[ Para
    [ Link
        ( "" , [] , [] )
        [ Str "x" ]
        ( "https://example.test/?q=one\182=two" , "" )
    ]
]
```

The writer therefore escapes the `&` when it begins a character
reference, so the destination survives a round trip:

```
% pandoc -f native -t gfm
[Para [Link ("",[],[]) [Str "x"] ("https://example.test/?q=one&para;=two","")]]
^D
[x](https://example.test/?q=one\&para;=two)
```

Reference link definitions write titles through the same code path:

```
% pandoc -f native -t markdown --reference-links
[Para [Link ("",[],[]) [Str "x"] ("https://example.test","Opens the \"spec\"")]]
^D
[x]

  [x]: https://example.test "Opens the \"spec\""
```
