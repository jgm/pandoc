```
% pandoc -f latex -t native --quiet
\begin{tabularx}{\linewidth}{|c|c|c|}
\hline
  Column Heading 1
  & Column Heading 2
  & Column Heading 3 \\
\hline
  Cell 1.1
  & Cell 1.2
  & Cell 1.3 \\
\hline
  Cell 2.1
  & Cell 2.2
  & Cell 2.3 \\
\hline
  Cell 3.1
  & Cell 3.2
  & Cell 3.3 \\
\hline
\end{tabularx}
^D
[Table ("",[],[]) (Caption Nothing
 []) [(AlignCenter,Nothing),(AlignCenter,Nothing),(AlignCenter,Nothing)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "3"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.3"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.3"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.3"]]]]
 []]
```

```
% pandoc -f latex -t native --quiet
\begin{tabularx}{\linewidth}{|X|c|p{0.25\linewidth}|}
\hline
  Column Heading 1
  & Column Heading 2
  & Column Heading 3 \\
\hline
  Cell 1.1
  & Cell 1.2
  & Cell 1.3 \\
\hline
  Cell 2.1
  & Cell 2.2
  & Cell 2.3 \\
\hline
  Cell 3.1
  & Cell 3.2
  & Cell 3.3 \\
\hline
\end{tabularx}
^D
[Table ("",[],[]) (Caption Nothing
 []) [(AlignLeft,Nothing),(AlignCenter,Nothing),(AlignLeft,Just 0.25)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "3"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.3"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.3"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.3"]]]]
 []]
```

```
% pandoc -f latex -t native --quiet
\begin{tabularx}{\linewidth}{|b{0.25\linewidth}|c|m{0.25\linewidth}|}
\hline
  Column Heading 1
  & Column Heading 2
  & Column Heading 3 \\
\hline
  Cell 1.1
  & Cell 1.2
  & Cell 1.3 \\
\hline
  Cell 2.1
  & Cell 2.2
  & Cell 2.3 \\
\hline
  Cell 3.1
  & Cell 3.2
  & Cell 3.3 \\
\hline
\end{tabularx}
^D
[Table ("",[],[]) (Caption Nothing
 []) [(AlignLeft,Just 0.25),(AlignCenter,Nothing),(AlignLeft,Just 0.25)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Column",Space,Str "Heading",Space,Str "3"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "1.3"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "2.3"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Cell",Space,Str "3.3"]]]]
 []]
```
