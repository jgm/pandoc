```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-fullnote-bibliography.csl'
references:
- ISBN: 0888441088
  author:
  - family: Goering
    given: Joseph
  call-number: BV4009 .W55 1992
  collection-number: 108
  collection-title: Studies and Texts
  event-place: Toronto
  first-reference-note-number: 1
  id: 'goering:1992william'
  issued:
    date-parts:
    - - 1992
  publisher: Pontifical Institute of Mediaeval Studies
  publisher-place: Toronto
  source: toroprod.library.utoronto.ca Library Catalog
  title: 'William de Montibus (c. 1140--1213): The Schools and the
    Literature of Pastoral Care'
  title-short: William de Montibus
  type: book
---

\... a prose commentary [the text of fol. 9r is printed in
@goering:1992william, pp. 501--3]. \... a collection of verses with a
formal prose commentary [excerpts from this text were previously printed
in @goering:1992william, p. 508--14; it was also briefly described in
@goering:1992william, pp. 141--42] \... and finally a note starting with
a citation [@goering:1992william, pp. 141-42].
^D
\... a prose commentary.[^1] \... a collection of verses with a formal
prose commentary[^2] \... and finally a note starting with a
citation.[^3]

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-goering:1992william .csl-entry}
Goering, Joseph. *William de Montibus (c. 1140--1213): The Schools and
the Literature of Pastoral Care*. Studies and Texts 108. Toronto:
Pontifical Institute of Mediaeval Studies, 1992.
:::
:::

[^1]: The text of fol. 9r is printed in Joseph Goering, *William de
    Montibus (c. 1140--1213): The Schools and the Literature of Pastoral
    Care*, Studies and Texts 108 (Toronto: Pontifical Institute of
    Mediaeval Studies, 1992), 501--3.

[^2]: Excerpts from this text were previously printed in Goering,
    508--14; it was also briefly described in Goering, 141--42.

[^3]: Goering, *William de Montibus*, 141--42.
```
