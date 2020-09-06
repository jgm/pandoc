```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Doe
    given: John
  container-title: Journal of Something
  id: item1
  issued:
    date-parts:
    - - 1987
    - - 1988
  page: '12-34'
  title: The title
  type: 'article-journal'
  volume: 3
- author:
  - family: Roe
    given: Ron
  container-title: Journal of Something
  id: item2
  issued:
    date-parts:
    - - 1987
  page: '12-34'
  title: The title
  type: 'article-journal'
  volume: 4
---

@item1; @item2
^D
Doe (1987--1988); Roe (1987)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Doe, John. 1987--1988. "The Title." *Journal of Something* 3: 12--34.
:::

::: {#ref-item2 .csl-entry}
Roe, Ron. 1987. "The Title." *Journal of Something* 4: 12--34.
:::
:::
```
