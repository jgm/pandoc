```
% pandoc --citeproc -t markdown-citations
---
references:
- id: stanze
  type: book
  issued:
  - year: 1547
  title: Stanze in lode della donna brutta
  publisher-place: Florence
  language: it-IT
...

@stanze is an anoynymous work.
^D
*Stanze in lode della donna brutta* (1547) is an anoynymous work.

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-stanze .csl-entry}
*Stanze in lode della donna brutta*. 1547. Florence.
:::
:::
```
