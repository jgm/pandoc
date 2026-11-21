`\path` from the url package typesets its argument verbatim, like
`\nolinkurl`.

```
% pandoc -f latex -t native
See \path{src/a_b.c} and \href{https://github.com/a/b}{\path{a/b}}; also \path{50%off}.
^D
[ Para
    [ Str "See"
    , Space
    , Code ( "" , [] , [] ) "src/a_b.c"
    , Space
    , Str "and"
    , Space
    , Link
        ( "" , [] , [] )
        [ Code ( "" , [] , [] ) "a/b" ]
        ( "https://github.com/a/b" , "" )
    , Str ";"
    , Space
    , Str "also"
    , Space
    , Code ( "" , [] , [] ) "50%off"
    , Str "."
    ]
]
```
