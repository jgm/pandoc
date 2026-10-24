Emphasis whose text starts with punctuation, right after a word, cannot
be opened with `*` in CommonMark: the delimiter is not left-flanking. The
punctuation at that edge is written outside the delimiters, so every
reader sees the same text and the same emphasis.

```
% pandoc -f native -t markdown
[ Para [ Str "word", Emph [ Str ".", Space, Str "\171Quoted?\187" ], Space, Str "after" ] ]
^D
word. *«Quoted?»* after
```

```
% pandoc -f native -t commonmark_x
[ Para [ Str "word", Emph [ Str ".", Space, Str "\171Quoted?\187" ], Space, Str "after" ] ]
^D
word. *«Quoted?»* after
```

```
% pandoc -f native -t gfm
[ Para [ Str "word", Strong [ Str ":", Space, Str "What?" ], Space, Str "after" ] ]
^D
word: **What?** after
```

The same holds at the closing edge, before a word.

```
% pandoc -f native -t markdown
[ Para [ Str "before", Space, Emph [ Str "text", Str "," ], Str "word" ] ]
^D
before *text*,word
```

```
% pandoc -f commonmark_x -t native
word. *«Quoted?»* after
^D
[ Para
    [ Str "word."
    , Space
    , Emph [ Str "\171Quoted?\187" ]
    , Space
    , Str "after"
    ]
]
```

Emphasis that already flanks is written as before.

```
% pandoc -f native -t markdown
[ Para [ Str "a", Space, Emph [ Str "b." ], Space, Str "c", Emph [ Str "d" ], Str "e" ] ]
^D
a *b.* c*d*e
```

A closing delimiter cannot follow whitespace either, a no-break space
included.

```
% pandoc -f native -t markdown
[ Para [ Emph [ Str "a", Space, Str "end.\160" ], Str "next" ] ]
^D
*a end.* next
```
