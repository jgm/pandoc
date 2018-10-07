```
% pandoc -s -t native
---
foobar_: this should be ignored
foo:
  bar_: as should this
---
^D
Pandoc (Meta {unMeta = fromList [("foo",MetaMap (fromList []))]})
[]
```
```
% pandoc -s -t native
---
# For precedence, see multiple-metadata-blocks.md and vars-and-metadata.md
# For Bools, see also 4819.md
# For Multiline strings, see yaml-with-chomp.md
int: 7
float: 1.5
scientific: 3.7e-5
bool: true
more: False
nothing: null
emtpy: []
nested:
  int: 8
  float: 2.5
  bool: true
  more: False
  nothing: null
  emtpy: []
  scientific: 3.7e-5
---
^D
Pandoc (Meta {unMeta = fromList [("bool",MetaBool True),("emtpy",MetaList []),("float",MetaInlines [Str "1.5"]),("int",MetaInlines [Str "7"]),("more",MetaBool False),("nested",MetaMap (fromList [("bool",MetaBool True),("emtpy",MetaList []),("float",MetaInlines [Str "2.5"]),("int",MetaInlines [Str "8"]),("more",MetaBool False),("nothing",MetaInlines [Str "null"]),("scientific",MetaInlines [Str "3.7e-5"])])),("nothing",MetaInlines [Str "null"]),("scientific",MetaInlines [Str "3.7e-5"])]})
[]
```
```
% pandoc -s -t native
---
array:
  - foo: bar
  - bool: True
---
^D
Pandoc (Meta {unMeta = fromList [("array",MetaList [MetaMap (fromList [("foo",MetaInlines [Str "bar"])]),MetaMap (fromList [("bool",MetaBool True)])])]})
[]
```
```
% pandoc -s -t native --metadata-file command/yaml-metadata.yaml
---
title: document
---
^D
Pandoc (Meta {unMeta = fromList [("other",MetaInlines [Emph [Str "markdown"],Space,Str "value"]),("title",MetaInlines [Str "document"])]})
[]
```
```
% pandoc -s -t native --metadata-file command/yaml-metadata.yaml -M title=cmdline
^D
Pandoc (Meta {unMeta = fromList [("other",MetaInlines [Emph [Str "markdown"],Space,Str "value"]),("title",MetaString "cmdline")]})
[]
```
