```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Augustine 1995)

Augustine, Robert L. 1995. *Heterogeneous Catalysis for the Synthetic
Chemist*. New York: Marcel Dekker.


Formatted with pandoc and apa.csl, 2013-10-23:

(Augustine, 1995)

Augustine, R. L. (1995). *Heterogeneous catalysis for the synthetic
chemist*. New York: Marcel Dekker.


}

@Book{augustine,
  author       = {Augustine, Robert L.},
  title        = {Heterogeneous catalysis for the synthetic chemist},
  date         = 1995,
  publisher    = {Marcel Dekker},
  location     = {New York},
  hyphenation  = {american},
  shorttitle   = {Heterogeneous catalysis},
  annotation   = {A plain book entry},
}

^D
---
nocite: "[@*]"
references:
- annote: A plain book entry
  author:
  - family: Augustine
    given: Robert L.
  id: augustine
  issued: 1995
  language: en-US
  publisher: Marcel Dekker
  publisher-place: New York
  title: Heterogeneous catalysis for the synthetic chemist
  title-short: Heterogeneous catalysis
  type: book
---


```
