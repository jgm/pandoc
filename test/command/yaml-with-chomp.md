```
% pandoc -s -t native
---
ml: |-
    TEST

    BLOCK
...
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "ml"
            , MetaInlines
                [ Str "TEST"
                , Space
                , Str "\182"
                , Space
                , Str "BLOCK"
                ]
            )
          ]
    }
  []
```
