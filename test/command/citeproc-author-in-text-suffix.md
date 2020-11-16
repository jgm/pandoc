```
% pandoc -t native
@a [p. 33; @b]
^D
[Para [Cite [Citation {citationId = "a", citationPrefix = [], citationSuffix = [Str "p.\160\&33"], citationMode = AuthorInText, citationNoteNum = 1, citationHash = 0},Citation {citationId = "b", citationPrefix = [], citationSuffix = [], citationMode = NormalCitation, citationNoteNum = 1, citationHash = 0}] [Str "@a",Space,Str "[p.",Space,Str "33;",Space,Str "@b]"]]]
```
