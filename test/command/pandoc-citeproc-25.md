```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Author
    given: Al
  id: item1
  issued:
    date-parts:
    - - 1998
  title: 'foo bar baz: bazbaz foo'
  type: 'article-journal'
---

Foo [@item1].

References {#references .unnumbered}
==========
^D
Foo (Author 1998).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Author, Al. 1998. "Foo Bar Baz: Bazbaz Foo."
:::
:::
```
