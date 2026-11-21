A URL in a table cell may contain `&` and `%`.

```
% pandoc -f latex -t native
\begin{tabular}{ll}
a & \url{https://example.com/watch?v=x&vl=en} \\
b & \href{https://example.com/?%3Fq=1&r=2}{link} \\
c & \nolinkurl{https://example.com/?a=1&b=2%20c} \\
\end{tabular}
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
                [ Plain
                    [ Link
                        ( "" , [ "uri" ] , [] )
                        [ Str "https://example.com/watch?v=x&vl=en" ]
                        ( "https://example.com/watch?v=x&vl=en" , "" )
                    ]
                ]
            ]
        , Row
            ( "" , [] , [] )
            [ Cell
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
                [ Plain
                    [ Link
                        ( "" , [] , [] )
                        [ Str "link" ]
                        ( "https://example.com/?%3Fq=1&r=2" , "" )
                    ]
                ]
            ]
        , Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "c" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain
                    [ Code
                        ( "" , [] , [] )
                        "https://example.com/?a=1&b=2%20c"
                    ]
                ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
