```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
    family: Author
    given:
    - Al
  id: item1
  issued:
    year: 2013
  language: 'en-US'
  number-of-volumes: 2
  publisher: Publisher
  publisher-place: Location
  title: Title
  type: book
---

@item1
^D
Author (2013)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Author, Al. 2013. *Title*. 2 vols. Location: Publisher.
:::
:::
```
