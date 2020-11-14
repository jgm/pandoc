```
% pandoc --citeproc -t markdown-citations
---
csl: command/issue160.csl
references:
- author:
  - family: Doe
    given: Jane
  citation-label: Jane11
  id: item1
  issued:
    year: 2011
  title: A book
  type: book
---

No citation-label
-----------------

Foo [@item1].

Expected
--------

> Foo \[Jane11\].
>
> \[Jane11\] Jane Doe. A book. 2011.
^D
## No citation-label

Foo \[Jane11\].

## Expected

> Foo \[Jane11\].
>
> \[Jane11\] Jane Doe. A book. 2011.

::: {#refs .references .csl-bib-body}
::: {#ref-item1 .csl-entry}
\[Jane11\] Jane Doe. A book. 2011.
:::
:::
```
