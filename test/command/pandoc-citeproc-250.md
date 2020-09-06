```
% pandoc --citeproc -t markdown-citations
---
link-citations: true
references:
- author:
    family: Doe
  id: doe
  title: Title
---

[@doe]
^D
([Doe, n.d.](#ref-doe))

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-doe .csl-entry}
Doe. n.d. "Title."
:::
:::
```
