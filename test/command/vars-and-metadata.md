Variables should not leak into metadata in the Markdown writer:

```
% pandoc -t markdown -Vfoo=1 -Vbar=2 -s --markdown-headings=setext
---
foo: x
...
zib
^D
---
foo: x
---

zib
```
