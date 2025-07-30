Test for AsciiDoc writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t asciidoc
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
This is a paragraph with attributes.

```

Test for AsciiDoc writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t asciidoc
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

Test for AsciiDoc writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t asciidoc
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
[[test]]
First paragraph

Second paragraph

```

Test for AsciiDoc writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t asciidoc
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
[[test]]
Not a wrapper
