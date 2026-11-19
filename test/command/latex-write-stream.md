The stream of \write can be a control sequence.

```
% pandoc -f latex -t native
\makeatletter
\immediate\write\@auxout{\string\newlabel{x}{{1}{1}}}
\makeatother
Text.
^D
[ Para [ Str "Text." ] ]
```

```
% pandoc -f latex+raw_tex -t native
\makeatletter
\immediate\write\@auxout{\string\newlabel{x}{{1}{1}}}
\makeatother
Text.
^D
[ RawBlock (Format "latex") "\\makeatletter"
, RawBlock (Format "latex") "\\immediate"
, RawBlock
    (Format "latex")
    "\\write\\@auxout{\\string\\newlabel{x}{{1}{1}}}"
, RawBlock (Format "latex") "\\makeatother"
, Para [ Str "Text." ]
]
```
