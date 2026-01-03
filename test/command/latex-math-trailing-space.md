Test that trailing spaces before \end{equation} don't create spurious paragraph breaks:

```
% pandoc -f latex -t latex
\begin{equation}
  a
 \end{equation}
^D
\begin{equation}
  a
\end{equation}
```

Same for align environment:

```
% pandoc -f latex -t latex
\begin{align}
  x &= y \\
 \end{align}
^D
\begin{align}
  x &= y \\
\end{align}
```

Test with multiple trailing spaces:

```
% pandoc -f latex -t latex
\begin{equation}
  a + b
   \end{equation}
^D
\begin{equation}
  a + b
\end{equation}
```
