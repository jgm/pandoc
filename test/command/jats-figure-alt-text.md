```
% pandoc -f jats -t native
<fig id="fig-1">
  <caption>
    <p>bar</p>
  </caption>
  <alt-text>alternative-decription</alt-text>
  <graphic xlink:href="foo.png" xlink:alt-text="baz" />
</fig>
^D
[ Figure
    ( "fig-1" , [] , [] )
    (Caption Nothing [ Plain [ Str "bar" ] ])
    [ Div
        ( "" , [ "caption" ] , [] )
        [ Header 6 ( "" , [] , [] ) [] , Para [ Str "bar" ] ]
    , Plain [ Str "alternative-decription" ]
    , Para
        [ Image ( "" , [] , [] ) [ Str "baz" ] ( "foo.png" , "" ) ]
    ]
]
```
