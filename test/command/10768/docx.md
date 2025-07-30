Test for DOCX writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t docx --print-default-data-file=reference.docx > /dev/null && pandoc -f html -t docx | pandoc -f docx -t native
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

Test for DOCX writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t docx --print-default-data-file=reference.docx > /dev/null && pandoc -f native -t docx | pandoc -f docx -t native
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

Test for DOCX writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t docx --print-default-data-file=reference.docx > /dev/null && pandoc -f native -t docx | pandoc -f docx -t native
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
[ Para [ Str "First" , Space , Str "paragraph" ]
, Para [ Str "Second" , Space , Str "paragraph" ]
]
```

Test for DOCX writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t docx --print-default-data-file=reference.docx > /dev/null && pandoc -f native -t docx | pandoc -f docx -t native
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
[ Para
    [ Str "Not" , Space , Str "a" , Space , Str "wrapper" ]
]
