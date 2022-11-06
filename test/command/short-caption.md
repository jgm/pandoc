```
% pandoc -f latex -t native
\begin{table}
\caption[short caption]{long caption}
\begin{tabular}{ll}
hi & hi \\
\end{tabular}
\end{table}
^D
[ Table
    ( "" , [] , [] )
    (Caption
       (Just [ Str "short" , Space , Str "caption" ])
       [ Plain [ Str "long" , Space , Str "caption" ] ])
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
                [ Plain [ Str "hi" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "hi" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
