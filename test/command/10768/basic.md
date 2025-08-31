% pandoc -f html -t native
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
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
```

```
% pandoc -f native -t html+paragraph_attributes
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
<p id="test" class="foo bar" data-custom="value">This is a paragraph
with attributes.</p>
```

```
% pandoc -f html+paragraph_attributes -t html+paragraph_attributes
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<p id="test" class="foo bar" data-custom="value">This is a paragraph
with attributes.</p>
```

```
% pandoc -f html+paragraph_attributes -t html5+paragraph_attributes
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<p id="test" class="foo bar" data-custom="value">This is a paragraph
with attributes.</p>
```

```
% pandoc --list-extensions=html | grep paragraph_attributes
^D
-paragraph_attributes
```

```
% pandoc --list-extensions=html5 | grep paragraph_attributes
^D
-paragraph_attributes
```

```
% pandoc --list-extensions | grep paragraph_attributes
^D
=> 1
```

```
% pandoc --list-extensions=markdown | grep paragraph_attributes
^D
=> 1
```

```
% pandoc -f native -t html+paragraph_attributes
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
<p id="test" class="foo bar" data-custom="value">This is a paragraph
with attributes.</p>
```

```
% pandoc -f native -t html-paragraph_attributes
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
<p>This is a paragraph with attributes.</p>
```

```
% pandoc -f native -t html5+paragraph_attributes
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
<p id="test" class="foo bar" data-custom="value">This is a paragraph
with attributes.</p>
```

```
% pandoc -f native -t html5-paragraph_attributes
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
<p>This is a paragraph with attributes.</p>
