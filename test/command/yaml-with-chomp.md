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
        fromList [ ( "ml" , MetaInlines [ Str "TEST\8233BLOCK" ] ) ]
    }
  []
```
