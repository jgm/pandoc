Test for FB2 writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t fb2
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<?xml version="1.0" encoding="UTF-8"?>
<FictionBook xmlns="http://www.gribuser.ru/xml/fictionbook/2.0" xmlns:l="http://www.w3.org/1999/xlink"><description><title-info><genre>unrecognised</genre></title-info><document-info><program-used>pandoc</program-used></document-info></description><body><title><p /></title><section><p>This is a paragraph with attributes.</p></section></body></FictionBook>
```

Test for FB2 writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t fb2
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
<?xml version="1.0" encoding="UTF-8"?>
<FictionBook xmlns="http://www.gribuser.ru/xml/fictionbook/2.0" xmlns:l="http://www.w3.org/1999/xlink"><description><title-info><genre>unrecognised</genre></title-info><document-info><program-used>pandoc</program-used></document-info></description><body><title><p /></title><section><p>This is a paragraph with attributes.</p></section></body></FictionBook>
```

Test for FB2 writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t fb2
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
<?xml version="1.0" encoding="UTF-8"?>
<FictionBook xmlns="http://www.gribuser.ru/xml/fictionbook/2.0" xmlns:l="http://www.w3.org/1999/xlink"><description><title-info><genre>unrecognised</genre></title-info><document-info><program-used>pandoc</program-used></document-info></description><body><title><p /></title><section><p>First paragraph</p><p>Second paragraph</p></section></body></FictionBook>
```

Test for FB2 writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t fb2
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
<?xml version="1.0" encoding="UTF-8"?>
<FictionBook xmlns="http://www.gribuser.ru/xml/fictionbook/2.0" xmlns:l="http://www.w3.org/1999/xlink"><description><title-info><genre>unrecognised</genre></title-info><document-info><program-used>pandoc</program-used></document-info></description><body><title><p /></title><section><p>Not a wrapper</p></section></body></FictionBook>
