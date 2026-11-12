```
% pandoc -f latex -t native
\expandafter\def\csname val-a\endcsname{1}
\expandafter\def\csname val-b\endcsname{2}
\newcommand{\val}[1]{\csname val-#1\endcsname}
Values \val{a} and \val{b}; \csname undefined\endcsname done.
^D
[ Para
    [ Str "Values"
    , Space
    , Str "1"
    , Space
    , Str "and"
    , Space
    , Str "2;"
    , Space
    , Str "done."
    ]
]
```

```
% pandoc -f latex+raw_tex -t latex
A \csname foo-bar\endcsname{} B
^D
A \csname foo-bar\endcsname{} B
```
