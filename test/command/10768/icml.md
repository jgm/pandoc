Test for ICML writer - HTML with attributed paragraph should unwrap wrapper div:

```
% pandoc -f html -t icml
<p id="test" class="foo bar" data-custom="value">This is a paragraph with attributes.</p>
^D
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>This is a paragraph with attributes.</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
```

Test for ICML writer - Native AST with wrapper div should unwrap to simple paragraph:

```
% pandoc -f native -t icml
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
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>This is a paragraph with attributes.</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
```

Test for ICML writer - wrapper div with multiple blocks should not be unwrapped:

```
% pandoc -f native -t icml
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
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>First paragraph</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
<Br />
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>Second paragraph</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
```

Test for ICML writer - normal div without wrapper attribute should not be unwrapped:

```
% pandoc -f native -t icml
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
<ParagraphStyleRange AppliedParagraphStyle="ParagraphStyle/Paragraph">
  <CharacterStyleRange AppliedCharacterStyle="$ID/NormalCharacterStyle">
    <Content>Not a wrapper</Content>
  </CharacterStyleRange>
</ParagraphStyleRange>
