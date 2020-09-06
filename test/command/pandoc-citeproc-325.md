```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Smith
    given: John
  id: item1
  type: book
- author:
  - family: Smith
    given: John
  id: item2
  type: book
---

[@item1; @item2]
^D
(Smith, n.d.a, n.d.b)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Smith, John. n.d.a.
:::

::: {#ref-item2 .csl-entry}
---------. n.d.b.
:::
:::
```
