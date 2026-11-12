```
% pandoc -f latex -t native
\begin{figure}
\includegraphics{a.png}
\caption{Key: {\color{red}\rule{2mm}{2mm}} ours,
\textcolor{gray}{\rule[0pt]{0.9em}{1pt}} baseline.}
\end{figure}

\textbf{\centerline{Title}}

\rule{\linewidth}{0.4pt}
^D
[ Figure
    ( "" , [] , [] )
    (Caption
       Nothing
       [ Plain
           [ Str "Key:"
           , Space
           , Str "ours,"
           , SoftBreak
           , Span ( "" , [] , [ ( "style" , "color: gray" ) ] ) []
           , Space
           , Str "baseline."
           ]
       ])
    [ Plain [ Image ( "" , [] , [] ) [] ( "a.png" , "" ) ] ]
, Para [ Strong [ Str "Title" ] ]
, HorizontalRule
]
```

```
% pandoc -f latex+raw_tex -t native
Swatch \rule{2mm}{2mm} here.
^D
[ Para
    [ Str "Swatch"
    , Space
    , RawInline (Format "latex") "\\rule{2mm}{2mm}"
    , Space
    , Str "here."
    ]
]
```

```
% pandoc -f latex -t native
\noindent\rule{\linewidth}{0.4pt}

Title\\
\rule{0.5\textwidth}{1pt}

Text \rule{1cm}{1pt} more.
^D
[ HorizontalRule
, Para [ Str "Title" , LineBreak ]
, HorizontalRule
, Para [ Str "Text" , Space , Str "more." ]
]
```
