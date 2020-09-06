```
% pandoc --citeproc -t markdown-citations
---
references:
- id: smith1
  type: article-journal
  author:
  - family: Smith
    given: Mary
  issued:
  - year: 2019
  title: Foo

- id: smithsmith
  type: article-journal
  author:
  - family: Smith
    given: Mary
  - family: Smith
    given: John
  issued:
  - year: 2019
  title: Foo bar
...

[@smithsmith; @smith1]
^D
(Smith and Smith 2019; Smith 2019)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-smith1 .csl-entry}
Smith, Mary. 2019. "Foo."
:::

::: {#ref-smithsmith .csl-entry}
Smith, Mary, and John Smith. 2019. "Foo Bar."
:::
:::
```
