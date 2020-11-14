```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
references:
- author:
  - family: Doe
    given: Ann
  - family: Doe
    given: Ben
  - family: Roe
    given: Ron
  id: a
  issued:
    date-parts:
    - - 2007
  title: Title
  type: 'article-journal'
---

@a
^D
Doe, Doe, and Roe (2007)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-a .csl-entry}
Doe, Ann, Ben Doe, and Ron Roe. 2007. "Title."
:::
:::
```
