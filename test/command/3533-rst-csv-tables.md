```
% pandoc -f rst -t native
.. csv-table:: Test
   :widths: 10, 5, 10
   :header: Flavor,Price,Slogan
   :file: command/3533-rst-csv-tables.csv
^D
[Table ("",[],[]) (Caption Nothing
 [Para [Str "Test"]]) [(AlignDefault,Just 0.4),(AlignDefault,Just 0.2),(AlignDefault,Just 0.4)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Flavor"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Price"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Slogan"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Albatross"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "2.99"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "On",Space,Str "a",Space,Str "stick!"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "Crunchy",Space,Str "Frog"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "1.49"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "If",Space,Str "we",Space,Str "took",Space,Str "the",Space,Str "bones",Space,Str "out,",Space,Str "it",Space,Str "wouldn't",Space,Str "be",SoftBreak,Str "crunchy,",Space,Str "now",Space,Str "would",Space,Str "it?"]]]]
 []]
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
 [Para [Str "Test"]]) [(AlignDefault,Nothing),(AlignDefault,Nothing),(AlignDefault,Nothing)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   []
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "a"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "b"]]]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "cat's"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "3"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "4"]]]
 ,Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "dog's"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "2"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "3"]]]]
 []]
```

```
% pandoc -f rst -t native
.. csv-table:: Test
   :escape: \

   "1","\""
^D
[Table ("",[],[]) (Caption Nothing
 [Para [Str "Test"]]) [(AlignDefault,Nothing),(AlignDefault,Nothing)] 0
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   []
  ,Cell ("",[],[]) Nothing 1 1
   []]]
 [Row ("",[],[])
  [Cell ("",[],[]) Nothing 1 1
   [Plain [Str "1"]]
  ,Cell ("",[],[]) Nothing 1 1
   [Plain [Str "\""]]]]
 []]
```

