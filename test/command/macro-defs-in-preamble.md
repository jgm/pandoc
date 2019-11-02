```
% pandoc -s -f latex-latex_macros -t native
\documentclass[11pt]{article}

\newcommand{\vara}{\alpha}
\newcommand{\varb}{b}

\begin{document}
$\vara \varb$
\end{document}
^D
Pandoc (Meta {unMeta = fromList []})
[RawBlock (Format "latex") "\\newcommand{\\vara}{\\alpha}"
,RawBlock (Format "latex") "\\newcommand{\\varb}{b}"
,Para [Math InlineMath "\\vara \\varb"]]
```
