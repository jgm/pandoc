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
 [])
 [(AlignLeft,ColWidthDefault)
 ,(AlignLeft,ColWidthDefault)
 ,(AlignLeft,ColWidthDefault)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Math InlineMath ""]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Math InlineMath "f1"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Math InlineMath "f2"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Math InlineMath "e"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Math InlineMath "0.5"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Math InlineMath "4"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Math InlineMath "f"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Math InlineMath "0.5"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Math InlineMath "5,5"]]]])]
 (TableFoot ("",[],[])
 [])]
```
