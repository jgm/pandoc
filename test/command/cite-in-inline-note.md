```
% pandoc -t native
foo^[bar [@doe]]
^D
[Para [Str "foo",Note [Para [Str "bar",Space,Cite [Citation {citationId = "doe", citationPrefix = [], citationSuffix = [], citationMode = NormalCitation, citationNoteNum = 0, citationHash = 0}] [Str "[@doe]"]]]]]
```
