```
% pandoc -f rst -t native
.. csv-table:: Test
   :widths: 10, 5, 10
   :header: Flavor,Price,Slogan
   :file: command/3533-rst-csv-tables.csv
^D
[Table ("",[],[]) (Caption Nothing
 [Plain [Str "Test"]])
 [(AlignDefault,ColWidth 0.4)
 ,(AlignDefault,ColWidth 0.2)
 ,(AlignDefault,ColWidth 0.4)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Flavor"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Price"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Slogan"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "Albatross"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "2.99"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "On",Space,Str "a",Space,Str "stick!"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "Crunchy",Space,Str "Frog"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "1.49"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "If",Space,Str "we",Space,Str "took",Space,Str "the",Space,Str "bones",Space,Str "out,",Space,Str "it",Space,Str "wouldn't",Space,Str "be",SoftBreak,Str "crunchy,",Space,Str "now",Space,Str "would",Space,Str "it?"]]]])]
 (TableFoot ("",[],[])
 [])]
```

```
% pandoc -f rst -t native
.. csv-table:: Test
   :header-rows: 1
   :quote: '
   :delim: space

   '' 'a' 'b'
   'cat''s' 3 4
   'dog''s' 2 3
^D
[Table ("",[],[]) (Caption Nothing
 [Plain [Str "Test"]])
 [(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   []
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "a"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "b"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "cat's"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "3"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "4"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "dog's"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "2"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "3"]]]])]
 (TableFoot ("",[],[])
 [])]
```

```
% pandoc -f rst -t native
.. csv-table:: Test
   :escape: \

   "1","\""
^D
[Table ("",[],[]) (Caption Nothing
 [Plain [Str "Test"]])
 [(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)]
 (TableHead ("",[],[])
 [])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "1"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "\""]]]])]
 (TableFoot ("",[],[])
 [])]
```

