```
% pandoc -f html -t native
<figure>
  <img src="foo.png" title="voyage">
  <figcaption>bar</figcaption>
</figure>
^D
[ Figure
    ( "" , [] , [] )
    (Caption Nothing [ Plain [ Str "bar" ] ])
    [ Plain
        [ Image ( "" , [] , [] ) [] ( "foo.png" , "voyage" ) ]
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
[ Figure
    ( "" , [] , [] )
    (Caption Nothing [ Plain [ Str "bar" ] ])
    [ Plain
        [ Image ( "" , [] , [] ) [] ( "foo.png" , "voyage" ) ]
    ]
]
```

```
% pandoc -f html -t native
<figure>
  <img src="foo.png" title="voyage">
</figure>
^D
[ Figure
    ( "" , [] , [] )
    (Caption Nothing [])
    [ Plain
        [ Image ( "" , [] , [] ) [] ( "foo.png" , "voyage" ) ]
    ]
]
```

```
% pandoc -f html -t native
<figure>
  <p><img src="foo.png" title="voyage"></p>
  <figcaption>bar</figcaption>
</figure>
^D
[ Figure
    ( "" , [] , [] )
    (Caption Nothing [ Plain [ Str "bar" ] ])
    [ Para
        [ Image ( "" , [] , [] ) [] ( "foo.png" , "voyage" ) ]
    ]
]
```

```
% pandoc -f html -t native
<figure><img src="foo.png" title="voyage" alt="this is ignored"><figcaption>bar <strong>baz</strong></figcaption></figure>
^D
[ Figure
    ( "" , [] , [] )
    (Caption
       Nothing
       [ Plain [ Str "bar" , Space , Strong [ Str "baz" ] ] ])
    [ Plain
        [ Image
            ( "" , [] , [] )
            [ Str "this" , Space , Str "is" , Space , Str "ignored" ]
            ( "foo.png" , "voyage" )
        ]
    ]
]
```
