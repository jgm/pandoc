Test that content after a table stays on the same slide.

```
% pandoc -t native
## Slide with Bullets and Table

- First bullet before table
- Second bullet before table

| A | B |
|---|---|
| 1 | 2 |

- Third bullet after table
- Fourth bullet after table
^D
[ Header
    2
    ( "slide-with-bullets-and-table" , [] , [] )
    [ Str "Slide"
    , Space
    , Str "with"
    , Space
    , Str "Bullets"
    , Space
    , Str "and"
    , Space
    , Str "Table"
    ]
, BulletList
    [ [ Plain
          [ Str "First"
          , Space
          , Str "bullet"
          , Space
          , Str "before"
          , Space
          , Str "table"
          ]
      ]
    , [ Plain
          [ Str "Second"
          , Space
          , Str "bullet"
          , Space
          , Str "before"
          , Space
          , Str "table"
          ]
      ]
    ]
, Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignDefault , ColWidthDefault )
    , ( AlignDefault , ColWidthDefault )
    ]
    (TableHead
       ( "" , [] , [] )
       [ Row
           ( "" , [] , [] )
           [ Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "A" ] ]
           , Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Str "B" ] ]
           ]
       ])
    [ TableBody
        ( "" , [] , [] )
        (RowHeadColumns 0)
        []
        [ Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "1" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "2" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
, BulletList
    [ [ Plain
          [ Str "Third"
          , Space
          , Str "bullet"
          , Space
          , Str "after"
          , Space
          , Str "table"
          ]
      ]
    , [ Plain
          [ Str "Fourth"
          , Space
          , Str "bullet"
          , Space
          , Str "after"
          , Space
          , Str "table"
          ]
      ]
    ]
]
```
