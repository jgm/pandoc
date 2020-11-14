```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
    family: Pelikan
    given: Jaroslav
  container-title: 'The Christian tradition: A history of the
    development of doctrine'
  id: CTv1c2
  issued:
  - year: 1971
  language: 'en-US'
  page: '34-56'
  publisher: University of Chicago Press
  publisher-place: Chicago
  title: Chapter two
  type: chapter
  volume: 1
  volume-title: 'The emergence of the Catholic tradition (100--600)'
- author:
    family: Pelikan
    given: Jaroslav
  container-title: 'The Christian tradition: A history of the
    development of doctrine'
  id: CTv1
  issued:
  - year: 1971
  language: 'en-US'
  publisher: University of Chicago Press
  publisher-place: Chicago
  title: 'The emergence of the Catholic tradition (100--600)'
  type: book
  volume: 1
- author:
    family: Pelikan
    given: Jaroslav
  id: CT
  issued:
  - year: 1971
  language: 'en-US'
  publisher: University of Chicago Press
  publisher-place: Chicago
  title: 'The Christian tradition: A history of the development of
    doctrine'
  type: book
---

Foo [@CT, 1:12]. Bar [@CTv1, 12]. Baz [@CTv1c2, 12].

References {#references .unnumbered}
==========
^D
Foo (Pelikan 1971b, 1:12). Bar (Pelikan 1971c, 1:12). Baz (Pelikan
1971a, 12).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-CTv1c2 .csl-entry}
Pelikan, Jaroslav. 1971a. "Chapter Two." In *The Christian Tradition: A
History of the Development of Doctrine*, 1:34--56. Chicago: University
of Chicago Press.
:::

::: {#ref-CT .csl-entry}
---------. 1971b. *The Christian Tradition: A History of the Development
of Doctrine*. Chicago: University of Chicago Press.
:::

::: {#ref-CTv1 .csl-entry}
---------. 1971c. *The Emergence of the Catholic Tradition (100--600)*.
*The Christian Tradition: A History of the Development of Doctrine*.
Vol. 1. Chicago: University of Chicago Press.
:::
:::
```
