```
% pandoc -f typst -t native -s
#set document(
  title: [My Title],
)

#title()
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "title"
            , MetaInlines [ Str "My" , Space , Str "Title" ]
            )
          ]
    }
  []
```

```
% pandoc -f typst -t native -s
#set document(
  title: [ignored],
)

#title[My Title]
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "title"
            , MetaInlines [ Str "My" , Space , Str "Title" ]
            )
          ]
    }
  []
```

```
% pandoc -f typst -t native -s
#title[My Title]
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "title"
            , MetaInlines [ Str "My" , Space , Str "Title" ]
            )
          ]
    }
  []
```
