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
[Table ("",[],[]) (Caption Nothing
 []) [(AlignLeft,Nothing),(AlignLeft,Nothing),(AlignLeft,Nothing)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath ""]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "f1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "f2"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "e"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "0.5"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "4"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "f"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "0.5"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Math InlineMath "5,5"]]]]
 []]
```
