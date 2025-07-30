Test for EPUB writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t epub3 --metadata title=Test -o /dev/null
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
```

Test for EPUB writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t epub3 --metadata title=Test -o /dev/null
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

Test for EPUB writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t epub3 --metadata title=Test -o /dev/null
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
