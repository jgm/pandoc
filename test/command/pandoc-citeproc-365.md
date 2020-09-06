```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/le-tapuscrit-note.csl'
references:
- ISBN: '978-2-912573-52-0'
  author:
  - family: Le Gras
    given: Gwénaëlle
  call-number: 'Tolbiac - Rez de Jardin - Littérature et art - Magasin -
    2010-82178'
  collection-title: 'Jeux d''acteurs'
  id: legras_michel_2010
  issued:
  - year: 2010
  language: fre
  number-of-pages: 128
  publisher: Scope
  publisher-place: Paris
  source: 'BnF Catalogue général (http://catalogue.bnf.fr)'
  title: 'Michel Simon : l''art de la disgrâce'
  title-short: Michel Simon
  type: book
---

Foo [@legras_michel_2010].
^D
Foo.[^1]

::: {#refs .references .csl-bib-body}
::: {#ref-legras_michel_2010 .csl-entry}
[Le Gras]{.smallcaps} Gwénaëlle, *Michel Simon : l'art de la disgrâce*,
Paris, Scope (coll. « Jeux d'acteurs »), 2010, 128 p.
:::
:::

[^1]: Gwénaëlle Le Gras, *Michel Simon : l'art de la disgrâce*, Paris,
    Scope, 2010, 128 p.
```
