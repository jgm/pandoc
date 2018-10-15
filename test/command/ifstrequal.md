```
% pandoc -f latex -t native
\ifstrequal{a}{b}{yes}{\emph{no}}
\newcommand{\h}[1]{\ifstrequal{#1}{a}{\'a}{#1}}
\h{a}
\h{b}
^D
[Para [Emph [Str "no"],SoftBreak,Str "\225",SoftBreak,Str "b"]]
```
