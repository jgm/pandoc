```
% pandoc -f markdown+latex_macros -t markdown
\newcommand{\my}{\phi}
$\my+\my$
^D
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

```
% pandoc -f latex -t latex
\def\BDpos{}
\def\BDneg{-}
\def\beq{\begin{align}}
\def\eeq{\end{align}}
\def\e#1{\emph{#1}}
\def\f#1#2{\emph{#1--#2}}

$5\BDneg 6\BDpos 7$

\beq
x &= y\\
\eeq

\e{hi}

\f{hi}{ok}
^D
\(5-67\)

\[\begin{aligned}
x &= y\\\end{aligned}\]

\emph{hi}

\emph{hi--ok}
```

```
% pandoc -f markdown+latex_macros -t markdown
\newcommand{\my}{\phi}
\begin{equation}
\my+\my
\end{equation}
^D
\begin{equation}
\phi+\phi
\end{equation}
```

```
% pandoc -f markdown-latex_macros -t markdown
\newcommand{\my}{\phi}
\begin{equation}
\my+\my
\end{equation}
^D
\newcommand{\my}{\phi}
\begin{equation}
\my+\my
\end{equation}
```

```
% pandoc -f markdown+latex_macros -t markdown
\newcommand{\my}{\emph{a}}
\my
^D
\emph{a}
```
