```
% pandoc -f latex -t native
\begin{figure}
\includegraphics{a.png}
\caption{First paragraph.

Second paragraph.}
\end{figure}
^D
[ Figure
    ( "" , [] , [] )
    (Caption
       Nothing
       [ Plain
           [ Str "First"
           , Space
           , Str "paragraph."
           , Space
           , Str "Second"
           , Space
           , Str "paragraph."
           ]
       ])
    [ Plain [ Image ( "" , [] , [] ) [] ( "a.png" , "" ) ] ]
]
```
