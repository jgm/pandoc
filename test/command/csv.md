```
% pandoc -f csv -t native
Fruit,Price,Quantity
Apple,25 cents,33
"""Navel"" Orange","35 cents",22
,,45
^D
[Table ("",[],[]) (Caption Nothing
 []) [(AlignDefault,Nothing),(AlignDefault,Nothing),(AlignDefault,Nothing)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Fruit"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Price"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Quantity"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Apple"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "25",Space,Str "cents"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "33"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "\"Navel\"",Space,Str "Orange"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "35",Space,Str "cents"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "22"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   []
  ,Cell ("",[],[]) Nothing 1 1
   []
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "45"]]]]
 []]
```
