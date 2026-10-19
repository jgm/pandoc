```
% pandoc -f xml -t native -
<Table>
<Caption />
<colspecs>
<ColSpec alignment="AlignDefault" col-width="0" />
<ColSpec alignment="AlignDefault" col-width="0" />
<ColSpec alignment="AlignDefault" col-width="0" />
</colspecs>
<TableHead/>
<TableBody row-head-columns="2">
<Row>
<Cell alignment="AlignDefault">
<Plain>BH1,1</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BH1,2</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BH1,3</Plain>
</Cell>
</Row>
<Row>
<Cell alignment="AlignDefault">
<Plain>BH2,1</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BH2,2</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BH2,3</Plain>
</Cell>
</Row>
<body>
<Row>
<Cell alignment="AlignDefault">
<Plain>BB1,1</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BB1,2</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BB1,3</Plain>
</Cell>
</Row>
<Row>
<Cell alignment="AlignDefault">
<Plain>BB2,1</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BB2,2</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BB2,3</Plain>
</Cell>
</Row>
<Row>
<Cell alignment="AlignDefault">
<Plain>BB3,1</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BB3,2</Plain>
</Cell>
<Cell alignment="AlignDefault">
<Plain>BB3,3</Plain>
</Cell>
</Row>
</body>
</TableBody>
<TableFoot/>
</Table>
^D
[ Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignDefault , ColWidthDefault )
    , ( AlignDefault , ColWidthDefault )
    , ( AlignDefault , ColWidthDefault )
    ]
    (TableHead ( "" , [] , [] ) [])
    [ TableBody
        ( "" , [] , [] )
        (RowHeadColumns 2)
        [ Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BH1,1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BH1,2" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BH1,3" ] ]
            ]
        , Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BH2,1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BH2,2" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BH2,3" ] ]
            ]
        ]
        [ Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB1,1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB1,2" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB1,3" ] ]
            ]
        , Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB2,1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB2,2" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB2,3" ] ]
            ]
        , Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB3,1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB3,2" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "BB3,3" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```
