```
% pandoc -f rst -t native
.. csv-table:: Test
   :widths: 10, 5, 10
   :header: Flavor,Price,Slogan
   :file: command/3533-rst-csv-tables.csv
^D
[Table [Str "Test"] [AlignDefault,AlignDefault,AlignDefault] [0.4,0.2,0.4]
 [[Plain [Str "Flavor"]]
 ,[Plain [Str "Price"]]
 ,[Plain [Str "Slogan"]]]
 [[[Plain [Str "Albatross"]]
  ,[Plain [Str "2.99"]]
  ,[Plain [Str "On",Space,Str "a",Space,Str "stick!"]]]
 ,[[Plain [Str "Crunchy",Space,Str "Frog"]]
  ,[Plain [Str "1.49"]]
  ,[Plain [Str "If",Space,Str "we",Space,Str "took",Space,Str "the",Space,Str "bones",Space,Str "out,",Space,Str "it",Space,Str "wouldn't",Space,Str "be",SoftBreak,Str "crunchy,",Space,Str "now",Space,Str "would",Space,Str "it?"]]]]]
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
[Table [Str "Test"] [AlignDefault,AlignDefault,AlignDefault] [0.0,0.0,0.0]
 [[]
 ,[Plain [Str "a"]]
 ,[Plain [Str "b"]]]
 [[[Plain [Str "cat's"]]
  ,[Plain [Str "3"]]
  ,[Plain [Str "4"]]]
 ,[[Plain [Str "dog's"]]
  ,[Plain [Str "2"]]
  ,[Plain [Str "3"]]]]]
```

```
% pandoc -f rst -t native
.. csv-table:: Test
   :escape: \

   "1","\""
^D
[Table [Str "Test"] [AlignDefault,AlignDefault] [0.0,0.0]
 [[]
 ,[]]
 [[[Plain [Str "1"]]
  ,[Plain [Str "\""]]]]]
```

