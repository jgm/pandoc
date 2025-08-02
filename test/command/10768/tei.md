Test for TEI writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t tei
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<p xml:id="test">This is a paragraph with attributes.</p>
```

Test for TEI writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t tei
[ Div
    ( "test"
    , [ "foo" , "bar" ]
    , [ ( "wrapper" , "1" ) , ( "custom" , "value" ) ]
    )
    [ Para
        [ Str "This"
        , Space
        , Str "is"
        , Space
        , Str "a"
        , Space
        , Str "paragraph"
        , Space
        , Str "with"
        , Space
        , Str "attributes."
        ]
    ]
]
^D
<p xml:id="test">This is a paragraph with attributes.</p>
```

Test for TEI writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t tei
[ Div
    ( "test"
    , [ "foo" ]
    , [ ( "wrapper" , "1" ) ]
    )
    [ Para
        [ Str "First"
        , Space
        , Str "paragraph"
        ]
    , Para
        [ Str "Second"
        , Space
        , Str "paragraph"
        ]
    ]
]
^D
<p>First paragraph</p>
<p>Second paragraph</p>
```

Test for TEI writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t tei
[ Div
    ( "test"
    , [ "foo" ]
    , [ ( "custom" , "value" ) ]
    )
    [ Para
        [ Str "Not"
        , Space
        , Str "a"
        , Space
        , Str "wrapper"
        ]
    ]
]
^D
<p xml:id="test">Not a wrapper</p>
