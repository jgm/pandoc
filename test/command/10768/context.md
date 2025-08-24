Test for ConTeXt writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t context
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
This is a paragraph with attributes.
```

Test for ConTeXt writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t context
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
This is a paragraph with attributes.
```

Test for ConTeXt writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t context
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
\reference[test]{}%
First paragraph

Second paragraph
```

Test for ConTeXt writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t context
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
\reference[test]{}%
Not a wrapper
