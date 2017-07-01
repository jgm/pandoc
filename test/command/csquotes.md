```
% pandoc -f latex -t native
\textcquote{hamlet}{To be, or not to be: that is the question}
^D
[Para [Quoted DoubleQuote [Str "To",Space,Str "be,",Space,Str "or",Space,Str "not",Space,Str "to",Space,Str "be:",Space,Str "that",Space,Str "is",Space,Str "the",Space,Str "question"],Str "\160",Cite [Citation {citationId = "hamlet", citationPrefix = [], citationSuffix = [], citationMode = NormalCitation, citationNoteNum = 0, citationHash = 0}] [RawInline (Format "latex") "\\autocite{hamlet}"]]]
```

```
% pandoc -f latex -t native
\textcquote[P. 10]{harry:philosophersstone}{You're a wizard, Harry.}
^D
[Para [Quoted DoubleQuote [Str "You\8217re",Space,Str "a",Space,Str "wizard,",Space,Str "Harry."],Str "\160",Cite [Citation {citationId = "harry:philosophersstone", citationPrefix = [], citationSuffix = [Str "P.",Space,Str "10"], citationMode = NormalCitation, citationNoteNum = 0, citationHash = 0}] [RawInline (Format "latex") "\\autocite[P. 10]{harry:philosophersstone}"]]]
```

```
% pandoc -f latex -t native
\textcquote[See][10]{rings:fellowship}{One Ring to rule them all, One Ring to find them, One Ring to bring them all, and in the darkness bind them.}
^D
[Para [Quoted DoubleQuote [Str "One",Space,Str "Ring",Space,Str "to",Space,Str "rule",Space,Str "them",Space,Str "all,",Space,Str "One",Space,Str "Ring",Space,Str "to",Space,Str "find",Space,Str "them,",Space,Str "One",Space,Str "Ring",Space,Str "to",Space,Str "bring",Space,Str "them",Space,Str "all,",Space,Str "and",Space,Str "in",Space,Str "the",Space,Str "darkness",Space,Str "bind",Space,Str "them."],Str "\160",Cite [Citation {citationId = "rings:fellowship", citationPrefix = [Str "See"], citationSuffix = [Str "10"], citationMode = NormalCitation, citationNoteNum = 0, citationHash = 0}] [RawInline (Format "latex") "\\autocite[See][10]{rings:fellowship}"]]]
```
