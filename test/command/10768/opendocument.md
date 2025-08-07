Test for OpenDocument writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t opendocument
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<text:p text:style-name="Text_20_body">This is a paragraph with
attributes.</text:p>
```

Test for OpenDocument writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t opendocument
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
<text:p text:style-name="Text_20_body">This is a paragraph with
attributes.</text:p>
```

Test for OpenDocument writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t opendocument
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
<text:section text:name="test"><text:p text:style-name="Text_20_body">First
paragraph</text:p>
<text:p text:style-name="Text_20_body">Second
paragraph</text:p></text:section>
```

Test for OpenDocument writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t opendocument
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
<text:section text:name="test"><text:p text:style-name="Text_20_body">Not
a wrapper</text:p></text:section>
