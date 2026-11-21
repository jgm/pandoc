`\address` may hold block content, such as several paragraphs or a
`\parbox`.

```
% pandoc -f latex -t native -s
\address{Department of Mathematics

University of Somewhere}
\begin{document}
Text.
\end{document}
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "address"
            , MetaBlocks
                [ Para
                    [ Str "Department"
                    , Space
                    , Str "of"
                    , Space
                    , Str "Mathematics"
                    ]
                , Para
                    [ Str "University"
                    , Space
                    , Str "of"
                    , Space
                    , Str "Somewhere"
                    ]
                ]
            )
          ]
    }
  [ Para [ Str "Text." ] ]
```

```
% pandoc -f latex -t native -s
\address{\parbox{3in}{Dept.\\ Univ.}}
\begin{document}
Text.
\end{document}
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "address"
            , MetaBlocks
                [ Para [ Str "Dept." , LineBreak , Str "Univ." ] ]
            )
          ]
    }
  [ Para [ Str "Text." ] ]
```

In a letter, a block address comes before the letter.

```
% pandoc -f latex -t native
\documentclass{letter}
\address{Dept. of Math

Univ.}
\begin{document}
\begin{letter}{Someone}
Hello.
\end{letter}
\end{document}
^D
[ Para
    [ Str "Dept." , Space , Str "of" , Space , Str "Math" ]
, Para [ Str "Univ." ]
, Para
    [ Span ( "" , [] , [] ) [ Str "Someone" ]
    , SoftBreak
    , Str "Hello."
    ]
]
```

The contents of an `\hbox` or `\mbox` that cannot be read as inlines,
such as a `\parbox`, are read as blocks.

```
% pandoc -f latex -t native -s
\address{\hbox{\parbox{5.7in}{G, \\Dip, \\Poli}}}
\begin{document}
Text.
\end{document}
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "address"
            , MetaBlocks
                [ Para
                    [ Str "G,"
                    , LineBreak
                    , Str "Dip,"
                    , LineBreak
                    , Str "Poli"
                    ]
                ]
            )
          ]
    }
  [ Para [ Str "Text." ] ]
```

```
% pandoc -f latex -t native
\mbox{\parbox{3in}{a\\ b}}

Before \hbox{\parbox{3in}{c\\ d}} after.
^D
[ Para [ Str "a" , LineBreak , Str "b" ]
, Para [ Str "Before" ]
, Para [ Str "c" , LineBreak , Str "d" ]
, Para [ Str "after." ]
]
```
