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

A destination containing something that looks like a character
reference must have the `&` escaped, since readers decode character
references in destinations. The writer's own output does not show
the difference; reading it back does:

```
% pandoc -f native -t gfm | pandoc -f gfm -t native
[Para [Link ("",[],[]) [Str "x"] ("https://example.test/?q=one&para;=two","")]]
^D
[ Para
    [ Link
        ( "" , [] , [] )
        [ Str "x" ]
        ( "https://example.test/?q=one&para;=two" , "" )
    ]
]
```

Reference link definitions write titles through the same code path:

```
% pandoc -f native -t markdown --reference-links
[Para [Link ("",[],[]) [Str "x"] ("https://example.test","Opens the \"spec\"")]]
^D
[x]

  [x]: https://example.test "Opens the \"spec\""
```
