As in LaTeX, the name in \newcommand may be in more than one pair of braces.

```
% pandoc -f latex -t native
\newcommand{{\ba}}{\mathbf{a}}
$\ba$
^D
[ Para [ Math InlineMath "\\mathbf{a}" ] ]
```
