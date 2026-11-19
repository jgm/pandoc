Definitions of active characters are read and ignored.

```
% pandoc -f latex -t native
\def~{\hphantom{0}}
\renewcommand{~}{\,}
\gdef:{\mathrel{\mathop\ordinarycolon}}
\let~\relax
Ratio 1:2 and a~b.
^D
[ Para
    [ Str "Ratio"
    , Space
    , Str "1:2"
    , Space
    , Str "and"
    , Space
    , Str "a\160b."
    ]
]
```
