```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Alice
  id: foo
  issued:
  - year: 2042
  other-ids:
  - bar
  - doz
  type: book
---

[@bar]
^D
[WARNING] Citeproc: citation bar not found
(**bar?**)
```
