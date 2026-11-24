Box commands from graphicx and adjustbox keep their contents, at block
and inline level.

```
% pandoc -f latex -t native
\resizebox{\textwidth}{!}{%
\begin{tabular}{ll}
a & b \\
\end{tabular}}
^D
[ Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignLeft , ColWidthDefault )
    , ( AlignLeft , ColWidthDefault )
    ]
    (TableHead ( "" , [] , [] ) [])
    [ TableBody
        ( "" , [] , [] )
        (RowHeadColumns 0)
        []
        [ Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "a" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "b" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```

```
% pandoc -f latex -t native
Text \scalebox{0.5}{X} \rotatebox[origin=c]{90}{Y} \reflectbox{Z}
\resizebox*{1cm}{!}{W} \adjustbox{max width=\linewidth}{V}.
^D
[ Para
    [ Str "Text"
    , Space
    , Str "X"
    , Space
    , Str "Y"
    , Space
    , Str "Z"
    , SoftBreak
    , Str "W"
    , Space
    , Str "V."
    ]
]
```

```
% pandoc -f latex -t native
\begin{adjustbox}{width=\textwidth}
\begin{tabular}{l}
a \\
\end{tabular}
\end{adjustbox}
^D
[ Div
    ( "" , [ "adjustbox" ] , [] )
    [ Table
        ( "" , [] , [] )
        (Caption Nothing [])
        [ ( AlignLeft , ColWidthDefault ) ]
        (TableHead ( "" , [] , [] ) [])
        [ TableBody
            ( "" , [] , [] )
            (RowHeadColumns 0)
            []
            [ Row
                ( "" , [] , [] )
                [ Cell
                    ( "" , [] , [] )
                    AlignDefault
                    (RowSpan 1)
                    (ColSpan 1)
                    [ Plain [ Str "a" ] ]
                ]
            ]
        ]
        (TableFoot ( "" , [] , [] ) [])
    ]
]
```

```
% pandoc -f latex -t native
\begin{figure}
\resizebox{0.5\textwidth}{!}{\includegraphics{plot.png}}
\caption{A plot.}
\end{figure}
^D
[ Figure
    ( "" , [] , [] )
    (Caption
       Nothing [ Plain [ Str "A" , Space , Str "plot." ] ])
    [ Plain [ Image ( "" , [] , [] ) [] ( "plot.png" , "" ) ] ]
]
```

Contents that cannot be read as inlines, such as a `\parbox`, are read
as blocks.

```
% pandoc -f latex -t native
\resizebox{\linewidth}{!}{\parbox{3in}{a\\ b}}
^D
[ Para [ Str "a" , LineBreak , Str "b" ] ]
```

A box is set in horizontal mode, where a blank line does nothing, and
contents that are not inline, such as a tikzpicture, are skipped.

```
% pandoc -f latex -t native
\scalebox{0.7}{

\begin{tikzpicture}
\draw (0,0) -- (1,1);
\end{tikzpicture}
}
Before \scalebox{0.5}{\begin{tikzpicture}\draw (0,0);\end{tikzpicture}}
after, and \scalebox{0.7}{a

b} c.
^D
[ Para
    [ Str "Before"
    , SoftBreak
    , Str "after,"
    , Space
    , Str "and"
    , Space
    , Str "a"
    , Space
    , Str "b"
    , Space
    , Str "c."
    ]
]
```
