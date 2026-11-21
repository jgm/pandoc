```
% pandoc -f latex+raw_tex -t native
\begin{tikzpicture}
\node at (0,0) {
  \begin{tikzpicture}[scale=0.4]
  \draw (0,0) -- (1,0);
  \end{tikzpicture}
};
\end{tikzpicture}

After.
^D
[ RawBlock
    (Format "latex")
    "\\begin{tikzpicture}\n\\node at (0,0) {\n  \\begin{tikzpicture}[scale=0.4]\n  \\draw (0,0) -- (1,0);\n  \\end{tikzpicture}\n};\n\\end{tikzpicture}"
, Para [ Str "After." ]
]
```
