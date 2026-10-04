Round trips through the flat OpenDocument format.  The flat format has no
zip structure, so a relative link is not rewritten, and an image is
embedded as base64 in an `office:binary-data` element rather than being
put in a `Pictures/` entry.

```
% pandoc -f markdown -t fodt | pandoc -f fodt -t native
# A *heading*

Inline $x^2$, display:

$$y = 1$$

| a | b |
|---|---|
| 1 | 2 |

1. one
2. two

A footnote[^1] and a [relative link](foo/bar.html).

[^1]: Note text.
^D
[ Header
    1
    ( "a-heading" , [] , [] )
    [ Span ( "anchor" , [] , [] ) []
    , Str "A"
    , Space
    , Emph [ Str "heading" ]
    ]
, Para
    [ Str "Inline"
    , Space
    , Math DisplayMath "x^{2}"
    , Str ","
    , Space
    , Str "display:"
    ]
, Para [ Math DisplayMath "y = 1" ]
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
               [ Plain [ Strong [ Str "a" ] ] ]
           , Cell
               ( "" , [] , [] )
               AlignDefault
               (RowSpan 1)
               (ColSpan 1)
               [ Plain [ Strong [ Str "b" ] ] ]
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
, OrderedList
    ( 1 , Decimal , Period )
    [ [ Plain [ Str "one" ] ] , [ Plain [ Str "two" ] ] ]
, Para
    [ Str "A"
    , Space
    , Str "footnote"
    , Note [ Para [ Str "Note" , Space , Str "text." ] ]
    , Space
    , Str "and"
    , Space
    , Str "a"
    , Space
    , Link
        ( "" , [] , [] )
        [ Str "relative" , Space , Str "link" ]
        ( "foo/bar.html" , "" )
    , Str "."
    ]
]
```

The image data survives the base64 round trip.

```
% pandoc -f markdown -t fodt | pandoc -f fodt -t native
![](lalune.jpg)
^D
[ Para
    [ Image
        ( ""
        , []
        , [ ( "width" , "150.0pt" ) , ( "height" , "150.0pt" ) ]
        )
        []
        ( "Pictures/image1.jpg" , "" )
    ]
]
```
