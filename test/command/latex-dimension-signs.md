As in TeX, a dimension may have several signs, followed by spaces.

```
% pandoc -f latex -t native
a\vskip +0.1in b\vskip - 3.7cm c\hskip -+-2pt d
^D
[ Para
    [ Str "a"
    , Space
    , Str "b"
    , Space
    , Str "c"
    , Space
    , Str "d"
    ]
]
```
