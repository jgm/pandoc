Figure float with caption at the figure level.

```
% pandoc -f native -t texinfo
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
@node Top
@top Top

@float Figure
@image{foo,,,Caption,png}
@caption{Caption}
@end float
```

Float that has no caption and doesn't contain a `SimpleFigure`

```
% pandoc -f native -t texinfo
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Image ("",[],[]) [] ("foo.png", "")]]]

^D
@node Top
@top Top

@float
@image{foo,,,,png}
@end float
```

Table float with caption at the figure level.

```
% pandoc -f native -t texinfo
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]])
[Table ("",[],[]) (Caption Nothing
 [])
 [(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Fruit"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Price"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Quantity"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "Apple"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "25",Space,Str "cents"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "33"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "\"Navel\"",Space,Str "Orange"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "35",Space,Str "cents"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "22"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    []
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    []
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "45"]]]])]
 (TableFoot ("",[],[])
 [])]]

^D
@node Top
@top Top

@float Table
@multitable {"Navel" Orange} {35 cents} {Quantity} 
@headitem 
Fruit
 @tab Price
 @tab Quantity
@item 
Apple
 @tab 25 cents
 @tab 33
@item 
"Navel" Orange
 @tab 35 cents
 @tab 22
@item 
45
@end multitable
@caption{Caption}
@end float
```

Float the isn't a table nor a figure.

```
% pandoc -f native -t texinfo
[Figure ("fig-id",[],[]) (Caption Nothing [Para[ Str "Caption"]]) [Para [Str "Content"]]]

^D
@node Top
@top Top

@float
Content
@caption{Caption}
@end float
```
