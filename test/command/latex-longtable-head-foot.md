The first head, the repeated head, the foot and the last foot of a
`longtable` become the table head and foot rather than body rows. The
caption of the first head is the table's.

```
% pandoc -f latex -t native
\begin{longtable}{ll}
\caption{Results}\label{tab:results}\\
\toprule
A & B \\
\midrule
\endfirsthead
\caption[]{Results (continued)}\\
\toprule
A & B \\
\midrule
\endhead
\midrule
\multicolumn{2}{r}{Continued on next page}\\
\endfoot
\bottomrule
Total & 3 \\
\endlastfoot
a & 1 \\
b & 2 \\
\end{longtable}
^D
[ Table
    ( "tab:results" , [] , [] )
    (Caption Nothing [ Plain [ Str "Results" ] ])
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
               [ Plain [ Str "A" ] ]
           , Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "B" ] ]
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
                [ Plain [ Str "a" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "1" ] ]
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
                [ Plain [ Str "2" ] ]
            ]
        ]
    ]
    (TableFoot
       ( "" , [] , [] )
       [ Row
           ( "" , [] , [] )
           [ Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "Total" ] ]
           , Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "3" ] ]
           ]
       ])
]
```

The head and foot pandoc's LaTeX writer produces are read back.

```
% pandoc -f latex -t native
\begin{longtable}[]{@{}ll@{}}
\toprule\noalign{}
one & two \\
\midrule\noalign{}
\endhead
\bottomrule\noalign{}
\tabularnewline
\caption[]{Repeated caption.}
\endfoot
\bottomrule\noalign{}
\tabularnewline
\caption{Repeated caption.}
\endlastfoot
1 & 2 \\
\end{longtable}
^D
[ Table
    ( "" , [] , [] )
    (Caption
       (Just [])
       [ Plain [ Str "Repeated" , Space , Str "caption." ] ])
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
               [ Plain [ Str "one" ] ]
           , Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "two" ] ]
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
                [ Plain [ Str "1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "2" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
