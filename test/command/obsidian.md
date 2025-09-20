obsidian tests:

```
% pandoc -f obsidian -t native
%% a comment %%
^D
[ Para [] ]
```

```
% pandoc -f obsidian -t native
A paragraph with a block ID. ^my-id
^D
[ Div
    ( "my-id" , [] , [] )
    [ Para
        [ Str "A"
        , Space
        , Str "paragraph"
        , Space
        , Str "with"
        , Space
        , Str "a"
        , Space
        , Str "block"
        , Space
        , Str "ID."
        ]
    ]
]
```

```
% pandoc -f obsidian -t native
An embed: ![[command/obsidian/transclusion]]
^D
[ Para
    [ Str "An"
    , Space
    , Str "embed:"
    , Space
    , Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "content"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "Obsidian"
    , Space
    , Str "transclusion"
    , Space
    , Str "test"
    , Space
    , Str "file."
    , LineBreak
    , Str "Introduction"
    , LineBreak
    , Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "introduction"
    , Space
    , Str "section"
    , Space
    , Str "with"
    , Space
    , Str "some"
    , Space
    , Str "content"
    , Space
    , Str "for"
    , Space
    , Str "heading"
    , Space
    , Str "transclusions."
    , LineBreak
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "block"
    , Space
    , Str "ID"
    , Space
    , Str "for"
    , Space
    , Str "testing"
    , Space
    , Str "block"
    , Space
    , Str "transclusions."
    , LineBreak
    , Str "Another"
    , Space
    , Str "Section"
    , LineBreak
    , Str "More"
    , Space
    , Str "content"
    , Space
    , Str "here."
    ]
]
```

```
% pandoc -f obsidian -t native
A block reference embed: ![[command/obsidian/transclusion#^my-id]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "block"
    , Space
    , Str "reference"
    , Space
    , Str "embed:"
    , Space
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "block"
    , Space
    , Str "ID"
    , Space
    , Str "for"
    , Space
    , Str "testing"
    , Space
    , Str "block"
    , Space
    , Str "transclusions."
    ]
]
```

```
% pandoc -f obsidian -t native
A wikilink transclusion: ![[command/obsidian/transclusion]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "wikilink"
    , Space
    , Str "transclusion:"
    , Space
    , Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "content"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "Obsidian"
    , Space
    , Str "transclusion"
    , Space
    , Str "test"
    , Space
    , Str "file."
    , LineBreak
    , Str "Introduction"
    , LineBreak
    , Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "introduction"
    , Space
    , Str "section"
    , Space
    , Str "with"
    , Space
    , Str "some"
    , Space
    , Str "content"
    , Space
    , Str "for"
    , Space
    , Str "heading"
    , Space
    , Str "transclusions."
    , LineBreak
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "block"
    , Space
    , Str "ID"
    , Space
    , Str "for"
    , Space
    , Str "testing"
    , Space
    , Str "block"
    , Space
    , Str "transclusions."
    , LineBreak
    , Str "Another"
    , Space
    , Str "Section"
    , LineBreak
    , Str "More"
    , Space
    , Str "content"
    , Space
    , Str "here."
    ]
]
```

```
% pandoc -f obsidian -t native
A block transclusion: ![[command/obsidian/transclusion#^my-id]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "block"
    , Space
    , Str "transclusion:"
    , Space
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "block"
    , Space
    , Str "ID"
    , Space
    , Str "for"
    , Space
    , Str "testing"
    , Space
    , Str "block"
    , Space
    , Str "transclusions."
    ]
]
```

```
% pandoc -f obsidian -t native
A heading transclusion: ![[command/obsidian/transclusion#Introduction]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "heading"
    , Space
    , Str "transclusion:"
    , Space
    , Str "Introduction"
    , LineBreak
    , Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "introduction"
    , Space
    , Str "section"
    , Space
    , Str "with"
    , Space
    , Str "some"
    , Space
    , Str "content"
    , Space
    , Str "for"
    , Space
    , Str "heading"
    , Space
    , Str "transclusions."
    , LineBreak
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "block"
    , Space
    , Str "ID"
    , Space
    , Str "for"
    , Space
    , Str "testing"
    , Space
    , Str "block"
    , Space
    , Str "transclusions."
    ]
]
```

```
% pandoc -f obsidian-wikilink_transclusions -t native
A wikilink transclusion: ![[obsidian/transclusion]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "wikilink"
    , Space
    , Str "transclusion:"
    , Space
    , Str "!"
    , Link
        ( "" , [ "wikilink" ] , [] )
        [ Str "obsidian/transclusion" ]
        ( "obsidian/transclusion" , "" )
    ]
]
```

```
% pandoc -f obsidian-wikilink_heading_transclusions -t native
A heading transclusion: ![[obsidian/transclusion#Introduction]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "heading"
    , Space
    , Str "transclusion:"
    , Space
    , Str "!"
    , Link
        ( ""
        , [ "wikilink" ]
        , [ ( "block-ref" , "Introduction" ) ]
        )
        [ Str "obsidian/transclusion#Introduction" ]
        ( "obsidian/transclusion" , "" )
    ]
]
```

```
% pandoc -f obsidian-wikilink_block_transclusions -t native
A block transclusion: ![[obsidian/transclusion#^my-id]]
^D
[ Para
    [ Str "A"
    , Space
    , Str "block"
    , Space
    , Str "transclusion:"
    , Space
    , Str "!"
    , Link
        ( "" , [ "wikilink" ] , [ ( "block-ref" , "^my-id" ) ] )
        [ Str "obsidian/transclusion#^my-id" ]
        ( "obsidian/transclusion" , "" )
    ]
]
```

```
% pandoc -f obsidian -t native
Text with ==highlighted== content.
^D
[ Para
    [ Str "Text"
    , Space
    , Str "with"
    , Space
    , Span ( "" , [ "mark" ] , [] ) [ Str "highlighted" ]
    , Space
    , Str "content."
    ]
]
```

```
% pandoc -f obsidian -t native
Nested heading transclusion: ![[command/obsidian/nested-transclusion#Nested Heading in Blockquote]]
^D
[ Para
    [ Str "Nested"
    , Space
    , Str "heading"
    , Space
    , Str "transclusion:"
    , Space
    , Str "Nested"
    , Space
    , Str "Heading"
    , Space
    , Str "in"
    , Space
    , Str "Blockquote"
    , LineBreak
    , Str "This"
    , Space
    , Str "heading"
    , Space
    , Str "is"
    , Space
    , Str "nested"
    , Space
    , Str "inside"
    , Space
    , Str "a"
    , Space
    , Str "blockquote"
    , Space
    , Str "for"
    , Space
    , Str "testing"
    , Space
    , Str "nested"
    , Space
    , Str "heading"
    , Space
    , Str "transclusions."
    , LineBreak
    , Str "Some"
    , Space
    , Str "content"
    , Space
    , Str "under"
    , Space
    , Str "the"
    , Space
    , Str "nested"
    , Space
    , Str "heading."
    , LineBreak
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "nested"
    , Space
    , Str "block"
    , Space
    , Str "ID."
    ]
]
```

```
% pandoc -f obsidian -t native
Nested block transclusion: ![[command/obsidian/nested-transclusion#^nested-block-id]]
^D
[ Para
    [ Str "Nested"
    , Space
    , Str "block"
    , Space
    , Str "transclusion:"
    , Space
    , Str "A"
    , Space
    , Str "paragraph"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "nested"
    , Space
    , Str "block"
    , Space
    , Str "ID."
    ]
]
```

```
% pandoc -f obsidian -t native
List nested heading transclusion: ![[command/obsidian/nested-transclusion#Heading in List Item]]
^D
[ Para
    [ Str "List"
    , Space
    , Str "nested"
    , Space
    , Str "heading"
    , Space
    , Str "transclusion:"
    , Space
    , Str "Heading"
    , Space
    , Str "in"
    , Space
    , Str "List"
    , Space
    , Str "Item"
    , LineBreak
    , Str "This"
    , Space
    , Str "heading"
    , Space
    , Str "is"
    , Space
    , Str "nested"
    , Space
    , Str "inside"
    , Space
    , Str "a"
    , Space
    , Str "list"
    , Space
    , Str "item."
    , LineBreak
    , Str "Content"
    , Space
    , Str "under"
    , Space
    , Str "the"
    , Space
    , Str "list"
    , Space
    , Str "heading."
    , LineBreak
    , Str "A"
    , Space
    , Str "block"
    , Space
    , Str "with"
    , Space
    , Str "an"
    , Space
    , Str "ID"
    , Space
    , Str "in"
    , Space
    , Str "a"
    , Space
    , Str "list."
    ]
]
```

```
% pandoc -f obsidian -t native
List nested block transclusion: ![[command/obsidian/nested-transclusion#^list-block-id]]
^D
[ Para
    [ Str "List"
    , Space
    , Str "nested"
    , Space
    , Str "block"
    , Space
    , Str "transclusion:"
    , Space
    , Str "A"
    , Space
    , Str "block"
    , Space
    , Str "with"
    , Space
    , Str "an"
    , Space
    , Str "ID"
    , Space
    , Str "in"
    , Space
    , Str "a"
    , Space
    , Str "list."
    ]
]
```

