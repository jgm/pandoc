```
% pandoc -s -t native
---
ml: |-
    TEST

    BLOCK
...
^D
Pandoc (Meta {unMeta = fromList [("ml",MetaBlocks [Para [Str "TEST"],Plain [Str "BLOCK"]])]})
[]
```
