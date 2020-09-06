```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
    family: Author
    given:
    - Ann
  container-title: Journal
  id: item1
  issued:
  - day: 24
    month: 9
    year: 2011
  - day: 26
    month: 9
    year: 2011
  title: Title
  type: 'article-magazine'
---

@item1
^D
Author (2011)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Author, Ann. 2011. "Title." *Journal*, September 24--26, 2011.
:::
:::
```
