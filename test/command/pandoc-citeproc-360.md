```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-fullnote-bibliography.csl'
references:
- author:
  - family: 'L''Estrange'
    given: Michael
  - family: Merchant
    given: Stephen
  id: lestrange2017
  issued:
  - day: 18
    month: 7
    year: 2017
  language: 'en-US'
  title: 2017 Independent Intelligence Review
  title-short: Independent Intelligence Review
  type: report
---

[@lestrange2017]
^D
[^1]

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-lestrange2017 .csl-entry}
L'Estrange, Michael, and Stephen Merchant. "2017 Independent
Intelligence Review," July 18, 2017.
:::
:::

[^1]: Michael L'Estrange and Stephen Merchant, "2017 Independent
    Intelligence Review," July 18, 2017.
```
