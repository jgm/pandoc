```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-note-bibliography.csl'
references:
- author:
    family: Author
    given:
    - Ann
  container-title: Journal
  id: item1
  issued:
  - year: 2011
  title: Title
  type: 'article-newspaper'
---

Foo [@item1].
^D
Foo.[^1]

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Author, Ann. "Title." *Journal*, 2011.
:::
:::

[^1]: Author, "Title."
```
