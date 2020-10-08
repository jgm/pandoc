```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Gaonkar 2001)

Gaonkar, Dilip Parameshwar. 2001. “On Alternative Modernities.” In
*Alternative Modernities*, edited by Dilip Parameshwar Gaonkar, 1–23.
Durham; London: Duke University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Gaonkar, 2001)

Gaonkar, D. P. (2001). On alternative modernities. In D. P. Gaonkar
(Ed.), *Alternative modernities* (pp. 1–23). Durham; London: Duke
University Press.


}

@InCollection{gaonkar:in,
  author       = {Gaonkar, Dilip Parameshwar},
  editor       = {Gaonkar, Dilip Parameshwar},
  title        = {On Alternative Modernities},
  date         = 2001,
  booktitle    = {Alternative Modernities},
  publisher    = {Duke University Press},
  location     = {Durham and London},
  isbn         = {0-822-32714-7},
  pages        = {1-23},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Gaonkar
    given: Dilip Parameshwar
  container-title: Alternative modernities
  editor:
  - family: Gaonkar
    given: Dilip Parameshwar
  id: "gaonkar:in"
  isbn: 0-822-32714-7
  issued: 2001
  page: 1-23
  publisher: Duke University Press
  publisher-place: Durham; London
  title: On alternative modernities
  type: chapter
---


```
