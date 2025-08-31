Test for DocBook writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t docbook
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<para xml:id="test">
  This is a paragraph with attributes.
</para>
```

Test for DocBook writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t docbook
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
<para xml:id="test">
  This is a paragraph with attributes.
</para>
```

Test for DocBook writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t docbook
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
<anchor xml:id="test" />
<para>
  First paragraph
</para>
<para>
  Second paragraph
</para>
```

Test for DocBook writer - normal div without wrapper attribute should unwrap single block:

```
% pandoc -f native -t docbook
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
<para xml:id="test">
  Not a wrapper
</para>
