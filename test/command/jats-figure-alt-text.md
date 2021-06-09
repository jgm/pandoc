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
[ Para
    [ Image
        ( "fig-1" , [] , [ ( "alt" , "alternative-decription" ) ] )
        [ Str "bar" ]
        ( "foo.png" , "fig:" )
    ]
]
```
