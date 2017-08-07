```
% pandoc -f markdown+latex_macros -t markdown
\newcommand{\my}{\phi}
$\my+\my$
^D
\newcommand{\my}{\phi}
$\phi+\phi$
```

```
% pandoc -f markdown-latex_macros -t markdown
\newcommand{\my}{\phi}
$\my+\my$
^D
\newcommand{\my}{\phi}
$\my+\my$
```

`\let` macros should be expanded at point of
definition, while `\newcommand` macros should be
expanded at point of use:

```
% pandoc -f latex -t latex
\let\a\b
\newcommand{\b}{\emph{ouk}}
\a
^D
\b
```

```
% pandoc -f latex -t latex
\newcommand{\a}{\b}
\newcommand{\b}{\emph{ouk}}
\a
^D
\emph{ouk}
```

