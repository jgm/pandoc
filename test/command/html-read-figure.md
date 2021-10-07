```
% pandoc -f html -t native
<figure>
  <img src="foo.png" title="voyage">
  <figcaption>bar</figcaption>
</figure>
^D
[ Para
    [ Image
        ( "" , [] , [] ) [ Str "bar" ] ( "foo.png" , "fig:voyage" )
    ]
]
```

```
% pandoc -f html -t native
<figure>
  <figcaption>bar</figcaption>
  <img src="foo.png" title="voyage">
</figure>
^D
[ Para
    [ Image
        ( "" , [] , [] ) [ Str "bar" ] ( "foo.png" , "fig:voyage" )
    ]
]
```

```
% pandoc -f html -t native
<figure>
  <img src="foo.png" title="voyage">
</figure>
^D
[ Para
    [ Image ( "" , [] , [] ) [] ( "foo.png" , "fig:voyage" ) ]
]
```

```
% pandoc -f html -t native
<figure>
  <p><img src="foo.png" title="voyage"></p>
  <figcaption>bar</figcaption>
</figure>
^D
[ Para
    [ Image
        ( "" , [] , [] ) [ Str "bar" ] ( "foo.png" , "fig:voyage" )
    ]
]
```

```
% pandoc -f html -t native
<figure><img src="foo.png" title="voyage" alt="this is ignored"><figcaption>bar <strong>baz</strong></figcaption></figure>
^D
[ Para
    [ Image
        ( "" , [] , [] )
        [ Str "bar" , Space , Strong [ Str "baz" ] ]
        ( "foo.png" , "fig:voyage" )
    ]
]
```
