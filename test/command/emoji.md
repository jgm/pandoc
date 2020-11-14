```
% pandoc -t markdown+emoji -f markdown+emoji --markdown-headings=setext
:smile:
^D
:smile:
```

```
% pandoc -t markdown-emoji -f markdown+emoji --markdown-headings=setext
:smile:
^D
😄
```

```
% pandoc -t gfm -f markdown+emoji
:smile:
^D
:smile:
```

```
% pandoc -t gfm-emoji -f markdown+emoji
:smile:
^D
😄
```
