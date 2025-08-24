Test for Powerpoint writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t pptx -o /dev/null
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
```

Test for Powerpoint writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t pptx -o /dev/null
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
```

Test for Powerpoint writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t pptx -o /dev/null
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
```

Test for Powerpoint writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t pptx -o /dev/null
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
