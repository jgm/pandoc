The column specification of `\multicolumn` may contain `@{...}`,
`!{...}`, `>{...}` and `<{...}` around the alignment.

```
% pandoc -f latex -t native
\begin{tabular}{@{}lll@{}}
\multicolumn{3}{@{}l@{}}{Group} \\
\multicolumn{2}{>{\bfseries}c}{Both} & \multicolumn{1}{!{\vrule}r<{\%}}{50} \\
a & b & c \\
\end{tabular}
^D
[ Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignLeft , ColWidthDefault )
    , ( AlignLeft , ColWidthDefault )
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
                AlignLeft
                (RowSpan 1)
                (ColSpan 3)
                [ Plain [ Str "Group" ] ]
            ]
        , Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignCenter
                (RowSpan 1)
                (ColSpan 2)
                [ Plain [ Str "Both" ] ]
            , Cell
                ( "" , [] , [] )
                AlignRight
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "50" ] ]
            ]
        , Row
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
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "c" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
