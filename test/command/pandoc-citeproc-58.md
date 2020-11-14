```
% pandoc --citeproc -t markdown-citations
---
csl: command/issue58.csl
references:
- id: stanze
  issued:
    date-parts:
    - - 1547
  language: 'it-IT'
  publisher-place: Florence
  title: Stanze in lode della donna brutta
  type: book
---

In this item, the title replaces the (unknown) author (see 14.79)
[@stanze, p. 12].

References
==========
^D
In this item, the title replaces the (unknown) author (see 14.79)
(*Stanze in lode della donna brutta* 1547, 12).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-stanze .csl-entry}
*Stanze in lode della donna brutta*. 1547. Florence.
:::
:::
```
