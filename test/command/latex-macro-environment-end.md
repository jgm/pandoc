```
% pandoc -f latex -t native
\def\proof{\emph{Proof.} }
\begin{proof}
Trivial.
\end{proof}

After.
^D
[ Para
    [ Emph [ Str "Proof." ] , SoftBreak , Str "Trivial." ]
, Para [ Str "After." ]
]
```
