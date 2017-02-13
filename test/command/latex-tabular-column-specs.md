See https://groups.google.com/forum/#!topic/pandoc-discuss/_VXtqihCyDU.

```
% pandoc -f latex -t native
\begin{tabular}{>{$}l<{$}>{$}l<{$} >{$}l<{$}}
\toprule
& f1 & f2 \\
\midrule
e      & 0.5    &  4   \\
f      & 0.5    & 5,5  \\
\bottomrule
\end{tabular}
^D
[Table [] [AlignLeft,AlignLeft,AlignLeft] [0.0,0.0,0.0]
 [[Plain [Math InlineMath ""]]
 ,[Plain [Math InlineMath "f1"]]
 ,[Plain [Math InlineMath "f2"]]]
 [[[Plain [Math InlineMath "e"]]
  ,[Plain [Math InlineMath "0.5"]]
  ,[Plain [Math InlineMath "4"]]]
 ,[[Plain [Math InlineMath "f"]]
  ,[Plain [Math InlineMath "0.5"]]
  ,[Plain [Math InlineMath "5,5"]]]]]
```
