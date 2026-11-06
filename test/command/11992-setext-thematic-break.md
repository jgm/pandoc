A line made only of dashes or equals must not be read back as a
thematic break or setext heading underline (#11992).

```
% pandoc -f native -t gfm
[Para [Str "---"]]
^D
\---
```

```
% pandoc -f native -t gfm
[Para [Str "Lead", LineBreak, Str "==="]]
^D
Lead\
\===
```

```
% pandoc -f native -t gfm
[Para [Str "Lead", LineBreak, Str "---"]]
^D
Lead\
\---
```

Wrapping must not put a run of equals or dashes alone on a line,
where it would become a setext underline or thematic break:

```
% pandoc -f native -t gfm --columns=20
[Para [Str "aaaaaaaaaaaaaaaaaaa", Space, Str "==="]]
^D
aaaaaaaaaaaaaaaaaaa ===
```

An inline run that is not alone on its line is left alone:

```
% pandoc -f native -t markdown
[Para [Str "lead", Space, Str "==="]]
^D
lead ===
```

With `smart`, the writer already escapes runs of dashes, so nothing
more is needed (regression guard):

```
% pandoc -f native -t markdown
[Para [Str "---"]]
^D
\-\--
```
