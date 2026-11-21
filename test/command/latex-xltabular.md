`xltabular` is a `longtable` with the width argument and `X` columns of
`tabularx`.

```
% pandoc -f latex -t native
\begin{xltabular}{\linewidth}{lX}
\caption{Packages}\label{tab:packages}\\
\toprule
Name & Purpose \\
\midrule
\endhead
xltabular & Long tables with X columns \\
\end{xltabular}
^D
[ Table
    ( "tab:packages" , [] , [] )
    (Caption Nothing [ Plain [ Str "Packages" ] ])
    [ ( AlignLeft , ColWidthDefault )
    , ( AlignLeft , ColWidthDefault )
    ]
    (TableHead
       ( "" , [] , [] )
       [ Row
           ( "" , [] , [] )
           [ Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "Name" ] ]
           , Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "Purpose" ] ]
           ]
       ])
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
                [ Plain [ Str "xltabular" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain
                    [ Str "Long"
                    , Space
                    , Str "tables"
                    , Space
                    , Str "with"
                    , Space
                    , Str "X"
                    , Space
                    , Str "columns"
                    ]
                ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
