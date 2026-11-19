\strut produces nothing, and doesn't end the paragraph.

```
% pandoc -f latex -t native
\textbf{\strut X} and \emph{a\strut b}.
^D
[ Para
    [ Strong [ Str "X" ]
    , Space
    , Str "and"
    , Space
    , Emph [ Str "ab" ]
    , Str "."
    ]
]
```

```
% pandoc -f latex -t native
a \strut b
^D
[ Para [ Str "a" , Space , Str "b" ] ]
```
