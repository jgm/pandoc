```
% pandoc -f csv -t native
Fruit,Price,Quantity
Apple,25 cents,33
"""Navel"" Orange","35 cents",22
,,45
^D
[Table ("",[],[]) (Caption Nothing
 [])
 [(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
   [Plain [Str "Fruit"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
   [Plain [Str "Price"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
   [Plain [Str "Quantity"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns {getRowHeadColumns = 0})
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "Apple"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "25",Space,Str "cents"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "33"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "\"Navel\"",Space,Str "Orange"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "35",Space,Str "cents"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "22"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    []
   ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    []
   ,Cell ("",[],[]) AlignDefault (RowSpan {getRowSpan = 1}) (ColSpan {getColSpan = 1})
    [Plain [Str "45"]]]])]
 (TableFoot ("",[],[])
 [])]
```
