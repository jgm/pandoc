Definitions made with \newcolumntype and tcolorbox's \newtcolorbox family
produce no output in the document body, as in the preamble.

```
% pandoc -f latex -t native
\newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}}
\newtcolorbox{mybox}[2][]{title=#2,#1}
\newtcblisting{prompt}[1]{listing only,title=#1}
Text.
^D
[ Para [ Str "Text." ] ]
```

```
% pandoc -f latex+raw_tex -t native
\newcolumntype{C}[1]{>{\centering\arraybackslash}p{#1}}
\newtcolorbox{mybox}[2][]{title=#2,#1}
\newtcblisting{prompt}[1]{listing only,title=#1}
Text.
^D
[ RawBlock
    (Format "latex")
    "\\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}"
, RawBlock
    (Format "latex") "\\newtcolorbox{mybox}[2][]{title=#2,#1}"
, RawBlock
    (Format "latex")
    "\\newtcblisting{prompt}[1]{listing only,title=#1}"
, Para [ Str "Text." ]
]
```
