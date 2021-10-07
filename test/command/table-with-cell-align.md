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
[ Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignDefault , ColWidthDefault )
    , ( AlignDefault , ColWidthDefault )
    , ( AlignDefault , ColWidthDefault )
    , ( AlignDefault , ColWidthDefault )
    ]
    (TableHead ( "" , [] , [] ) [])
    [ TableBody
        ( "" , [] , [] )
        (RowHeadColumns 0)
        []
        [ Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignCenter
                (RowSpan 1)
                (ColSpan 1)
                [ Para [ Str "1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignLeft
                (RowSpan 1)
                (ColSpan 1)
                [ Para [ Str "2" ] ]
            , Cell
                ( "" , [] , [] )
                AlignRight
                (RowSpan 1)
                (ColSpan 1)
                [ Para [ Str "3" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Para [ Str "4" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
```
% pandoc -f native -t opendocument --quiet
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
^D
<table:table table:name="Table1" table:style-name="Table1">
  <table:table-column table:style-name="Table1.A" />
  <table:table-column table:style-name="Table1.B" />
  <table:table-column table:style-name="Table1.C" />
  <table:table-column table:style-name="Table1.D" />
  <table:table-row>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="P1">1</text:p>
    </table:table-cell>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="Table_20_Contents">2</text:p>
    </table:table-cell>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="P2">3</text:p>
    </table:table-cell>
    <table:table-cell table:style-name="TableRowCell" office:value-type="string">
      <text:p text:style-name="Table_20_Contents">4</text:p>
    </table:table-cell>
  </table:table-row>
</table:table>
```
