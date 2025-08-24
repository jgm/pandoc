Test for Man writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t man
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
.PP
This is a paragraph with attributes.
```

Test for Man writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t man
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
.PP
This is a paragraph with attributes.
```

Test for Man writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t man
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
.PP
First paragraph
.PP
Second paragraph
```

Test for Man writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t man
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
.PP
Not a wrapper
