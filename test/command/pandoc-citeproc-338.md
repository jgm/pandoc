```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/din-1505-2.csl'
lang: de
nocite: '@*'
pagetitle: Citation
references:
- ISBN: '978-3-642-32078-1'
  author:
  - family: Wolfinger
    given: Christine
  edition: '11., vollst. überarb. Aufl.'
  id: 'item-1'
  issued:
  - year: 2013
  keyword: UNIX; LINUX
  number-of-pages: 'XVIII, 529 S. : Ill., graph. Darst.'
  publisher: Springer Vieweg
  publisher-place: 'Berlin \[u.a.\]'
  title: 'Keine Angst vor Linux, Unix: ein Lehrbuch für Linux- und
    Unix-Anwender'
  type: book
---

^D
::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item-1 .csl-entry}
[Wolfinger, Christine]{.smallcaps}: *Keine Angst vor Linux, Unix: ein
Lehrbuch für Linux- und Unix-Anwender*. 11., vollst. überarb. Aufl.
Aufl. Berlin \[u.a.\] : Springer Vieweg, 2013 --- ISBN 978-3-642-32078-1
:::
:::
```
