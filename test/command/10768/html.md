% pandoc -f native -t html
[ Div
    ( ""
    , []
    , [ ( "wrapper" , "1" ) ]
    )
    [ Para
        [ Str "Normal"
        , Space
        , Str "paragraph"
        ]
    ]
]
^D
<p>Normal paragraph</p>
```

```
% pandoc -f native -t html
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
<div id="test" class="foo" data-custom="value">
<p>Not a wrapper</p>
</div>
```

Test for unwrapWrapperDiv function - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t html
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
<div id="test" class="foo" data-wrapper="1">
<p>First paragraph</p>
<p>Second paragraph</p>
</div>
```

Test for unwrapWrapperDiv function - wrapper div with empty content:

```
% pandoc -f native -t html
[ Div
    ( ""
    , []
    , [ ( "wrapper" , "1" ) ]
    )
    []
]
^D
<div data-wrapper="1">

</div>
