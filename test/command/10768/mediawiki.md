
Test for MediaWiki writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t mediawiki
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
This is a paragraph with attributes.
```

Test for MediaWiki writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t mediawiki
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

Test for MediaWiki writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t mediawiki
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
<div id="test" class="foo" wrapper="1">

First paragraph

Second paragraph


</div>
```

Test for MediaWiki writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t mediawiki
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
<div id="test" class="foo" custom="value">

Not a wrapper


</div>
