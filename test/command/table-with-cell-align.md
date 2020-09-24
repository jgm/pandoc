```
% pandoc -f docbook -t native --quiet
<informaltable frame="all" rowsep="1" colsep="1">
<tgroup cols="16">
<tbody>
<row>
<entry align="center" valign="top"><simpara>1</simpara></entry>
<entry align="left" valign="top"><simpara>2</simpara></entry>
<entry align="right" valign="top"><simpara>3</simpara></entry>
<entry align="justify" valign="top"><simpara>4</simpara></entry>
</row>
</tbody>
</tgroup>
</informaltable>
^D
[Table ("",[],[]) (Caption Nothing
 [])
 [(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault)]
 (TableHead ("",[],[])
 [])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignCenter (RowSpan 1) (ColSpan 1)
    [Para [Str "1"]]
   ,Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1)
    [Para [Str "2"]]
   ,Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1)
    [Para [Str "3"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Para [Str "4"]]]])]
 (TableFoot ("",[],[])
 [])]
```
