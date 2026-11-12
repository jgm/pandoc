```
% pandoc -f latex -t native
\def\state#1. {(#1)}
\def\pair#1\and#2{[#1|#2]}
\def\word#1 {<#1>}
\def\swap#1,#2%
{#2 #1}
\state a. b \pair x\and y \word abc
def \swap x,y
^D
[ Para
    [ Str "(a)b"
    , Space
    , Str "[x|y]"
    , Space
    , Str "<abc>def"
    , Space
    , Str "y"
    , Space
    , Str "x"
    ]
]
```

```
% pandoc -f latex -t native
\def\foo\bar
Some \textbf{bold} text.
^D
[ Para
    [ Str "Some"
    , Space
    , Strong [ Str "bold" ]
    , Space
    , Str "text."
    ]
]
```
