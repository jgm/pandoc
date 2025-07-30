Test for RTF writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t rtf
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
{\pard \ql \f0 \sa180 \li0 \fi0 This is a paragraph with attributes.\par}
```

Test for RTF writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t rtf
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
{\pard \ql \f0 \sa180 \li0 \fi0 This is a paragraph with attributes.\par}
```

Test for RTF writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t rtf
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
{\pard \ql \f0 \sa180 \li0 \fi0 First paragraph\par}
{\pard \ql \f0 \sa180 \li0 \fi0 Second paragraph\par}
```

Test for RTF writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t rtf
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
{\pard \ql \f0 \sa180 \li0 \fi0 Not a wrapper\par}
