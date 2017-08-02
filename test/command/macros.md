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
