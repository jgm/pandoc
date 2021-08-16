```
% pandoc --citeproc -t markdown-citations
---
csl: command/annales.csl
references:
- author:
  - family: Timmory
    given: François
  container-title: 'L''Écran français'
  id: timmory\_\_justice_1950
  issue: 272
  issued:
  - day: 25
    month: 9
    year: 1950
  language: 'fr-FR'
  page: 12
  title: '*Justice est faite* : soyons justes'
  type: 'article-journal'
---

Foo [@timmory__justice_1950].
^D
Foo.[^1]

::: {#refs .references .csl-bib-body}
::: {#ref-timmory__justice_1950 .csl-entry}
François [Timmory]{.smallcaps}, « *Justice est faite* : soyons justes »,
*L'Écran français*, 1950, nᵒ 272, p. 12.
:::
:::

[^1]: François [Timmory]{.smallcaps}, « *Justice est faite* : soyons
    justes », *L'Écran français*, 1950, nᵒ 272, p. 12.
```
