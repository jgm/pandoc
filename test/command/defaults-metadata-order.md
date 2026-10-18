Metadata in defaults files keeps the order of its fields, in YAML
(including fields from an inherited defaults file) and in JSON.

```
% pandoc -d command/defaults-metadata-order.yaml
Text.
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "title" , MetaString "Child title" )
          , ( "keywords" , MetaString "k" )
          , ( "zeta"
            , MetaMap
                (fromList
                   [ ( "y" , MetaString "1" )
                   , ( "b" , MetaString "2" )
                   ])
            )
          , ( "author" , MetaString "A" )
          ]
    }
  [ Para [ Str "Text." ] ]
```

```
% pandoc -d command/defaults-metadata-order.json
Text.
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "zeta"
            , MetaMap
                (fromList
                   [ ( "y" , MetaString "1" )
                   , ( "b" , MetaString "2" )
                   ])
            )
          , ( "title" , MetaString "T" )
          , ( "author" , MetaString "A" )
          ]
    }
  [ Para [ Str "Text." ] ]
```
