If multiple blocks define a field, the first is used.

```
% pandoc -s -t native
---
foo: bar
...

---
foo: bim
...
^D
Pandoc (Meta {unMeta = fromList [("foo",MetaInlines [Str "bim"])]})
[]
```
