```
% pandoc --shift-heading-level-by 1 -t native -s
---
title: My title
...

# First heading

## Second
^D
Pandoc (Meta {unMeta = fromList [("title",MetaInlines [Str "My",Space,Str "title"])]})
[Header 2 ("first-heading",[],[]) [Str "First",Space,Str "heading"]
,Header 3 ("second",[],[]) [Str "Second"]]
```

```
% pandoc --shift-heading-level-by -1 -t native -s
---
title: Old title
...

# First heading

## Second

# Another top-level heading
^D
Pandoc (Meta {unMeta = fromList [("title",MetaInlines [Str "First",Space,Str "heading"])]})
[Header 1 ("second",[],[]) [Str "Second"]
,Para [Str "Another",Space,Str "top-level",Space,Str "heading"]]
```

