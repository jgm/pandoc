The argument of `\caption` may contain paragraph breaks, which LaTeX
allows when a short caption is given. It is then read as blocks.

```
% pandoc -f latex -t native
\begin{figure}
\includegraphics{a.png}
\caption[Short]{First paragraph.

Second paragraph.}
\end{figure}
^D
[ Figure
    ( "" , [] , [] )
    (Caption
       (Just [ Str "Short" ])
       [ Para [ Str "First" , Space , Str "paragraph." ]
       , Para [ Str "Second" , Space , Str "paragraph." ]
       ])
    [ Plain [ Image ( "" , [] , [] ) [] ( "a.png" , "" ) ] ]
]
```

```
% pandoc -f latex -t native
\begin{longtable}{l}
\caption[Short]{First paragraph.

Second paragraph.}\\
a \\
\end{longtable}
^D
[ Table
    ( "" , [] , [] )
    (Caption
       (Just [ Str "Short" ])
       [ Para [ Str "First" , Space , Str "paragraph." ]
       , Para [ Str "Second" , Space , Str "paragraph." ]
       ])
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
```

A caption that can be read as inlines is still a single `Plain` block.

```
% pandoc -f latex -t native
\begin{figure}
\includegraphics{a.png}
\caption{One paragraph.}
\end{figure}
^D
[ Figure
    ( "" , [] , [] )
    (Caption
       Nothing [ Plain [ Str "One" , Space , Str "paragraph." ] ])
    [ Plain [ Image ( "" , [] , [] ) [] ( "a.png" , "" ) ] ]
]
```
