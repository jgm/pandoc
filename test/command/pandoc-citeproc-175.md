```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Doe
    given: Jane
  container-title: A magazine
  id: item1
  issued:
  - month: 1
    year: 2011
  - month: 2
    year: 2011
  page: '33-44'
  title: A title
  type: 'article-magazine'
---

Missing en-dash between months
------------------------------

Foo [@item1].

Expected
--------

> Doe, Jane. 2011. "A Title." *A Magazine*, January--February.
^D
## Missing en-dash between months

Foo (Doe 2011).

## Expected

> Doe, Jane. 2011. "A Title." *A Magazine*, January--February.

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Doe, Jane. 2011. "A Title." *A Magazine*, January--February 2011.
:::
:::
```
