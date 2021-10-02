```
% pandoc --from=latex -t native
\begin{document}
Visible

\include{command/bar-endinput}

Visible
\end{document}
^D
[ Para [ Str "Visible" ]
, Para [ Emph [ Str "hi there" ] ]
, Para [ Str "Visible" ]
]
```
