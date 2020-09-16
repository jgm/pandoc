This inserts an empty `\item[]` when a list occurs at the
beginning of a definition list definition; otherwise the list
may start on the line with the label, which looks terrible.
See https://tex.stackexchange.com/questions/192480/force-itemize-inside-description-onto-a-new-line

```
% pandoc -t latex
Definition
:   1. list
    2. list
^D
\begin{description}
\item[Definition]
\begin{enumerate}
\def\labelenumi{\arabic{enumi}.}
\tightlist
\item[]
\item
  list
\item
  list
\end{enumerate}
\end{description}
```

```
% pandoc -t latex
Definition
:   Foo

    1. list
    2. list
^D
\begin{description}
\item[Definition]
Foo

\begin{enumerate}
\def\labelenumi{\arabic{enumi}.}
\tightlist
\item
  list
\item
  list
\end{enumerate}
\end{description}
```

```
% pandoc -t latex
Definition
:   - list
    - list
^D
\begin{description}
\item[Definition]
\begin{itemize}
\tightlist
\item[]
\item
  list
\item
  list
\end{itemize}
\end{description}
```

