```
% pandoc -f latex -t html
\newenvironment{foo}%
{\emph\bgroup}%
{\egroup}

\begin{foo}
hi
\end{foo}
^D
<p><span> <em>hi</em> </span></p>
```
