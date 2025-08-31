Test for ODT writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t odt --print-default-data-file=reference.odt > /dev/null && pandoc -f html -t odt | pandoc -f odt -t native
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
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
```

Test for ODT writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t odt --print-default-data-file=reference.odt > /dev/null && pandoc -f native -t odt | pandoc -f odt -t native
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
```

Test for ODT writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t odt --print-default-data-file=reference.odt > /dev/null && pandoc -f native -t odt | pandoc -f odt -t native
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
[ Div
    ( "" , [] , [] )
    [ Para [ Str "First" , Space , Str "paragraph" ]
    , Para [ Str "Second" , Space , Str "paragraph" ]
    ]
]
```

Test for ODT writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t odt --print-default-data-file=reference.odt > /dev/null && pandoc -f native -t odt | pandoc -f odt -t native
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
[ Div
    ( "" , [] , [] )
    [ Para
        [ Str "Not" , Space , Str "a" , Space , Str "wrapper" ]
    ]
]
