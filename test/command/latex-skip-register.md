A length register can be the amount of a skip.

```
% pandoc -f latex -t native
a\hskip\parindent b \hskip-\parindent c \vskip 0.5\baselineskip plus 1pt d
\vskip .5\baselineskip e
^D
[ Para
    [ Str "ab"
    , Space
    , Str "c"
    , Space
    , Str "d"
    , SoftBreak
    , Str "e"
    ]
]
```

```
% pandoc -f latex+raw_tex -t native
First paragraph.
\vskip\baselineskip
Second paragraph.
^D
[ Para
    [ Str "First"
    , Space
    , Str "paragraph."
    , SoftBreak
    , RawInline (Format "latex") "\\vskip\\baselineskip"
    , SoftBreak
    , Str "Second"
    , Space
    , Str "paragraph."
    ]
]
```
