```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/harvard-university-of-kent.csl'
references:
- author:
  - family: Doe
    given: Ann
  collection-title: The collection title
  dimensions: 789 pp.
  id: doe1
  issued:
  - year: 1999
  publisher: The publisher
  title: Title
  type: book
---

Foo [@doe1].

References {#references .unnumbered}
==========
^D
Foo (Doe 1999).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-doe1 .csl-entry}
Doe, A. (1999). *Title*. The collection title. The publisher. 789 pp.
:::
:::
```
