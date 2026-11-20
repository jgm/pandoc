cleveref's optional argument to \label is skipped.

```
% pandoc -f latex -t native
\section{Results}\label[lemma]{sec:res}
See \ref{sec:res}.
^D
[ Header 1 ( "sec:res" , [] , [] ) [ Str "Results" ]
, Para
    [ Str "See"
    , Space
    , Link
        ( ""
        , []
        , [ ( "reference-type" , "ref" )
          , ( "reference" , "sec:res" )
          ]
        )
        [ Str "1" ]
        ( "#sec:res" , "" )
    , Str "."
    ]
]
```

```
% pandoc -f latex -t native
\begin{figure}
\includegraphics{a.png}
\caption{A figure.}\label[figure]{fig:a}
\end{figure}
^D
[ Figure
    ( "fig:a" , [] , [] )
    (Caption
       Nothing [ Plain [ Str "A" , Space , Str "figure." ] ])
    [ Plain [ Image ( "" , [] , [] ) [] ( "a.png" , "" ) ] ]
]
```
