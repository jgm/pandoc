```
% pandoc -t native -s
---
hi: !!literal '*ook*'
there: '*ook*'
...
^D
Pandoc (Meta {unMeta = fromList [("hi",MetaString "*ook*"),("there",MetaInlines [Emph [Str "ook"]])]})
[]
```
