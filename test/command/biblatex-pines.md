```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Pines 1979)

Pines, Shlomo. 1979. “The Limitations of Human Knowledge According to
Al-Farabi, ibn Bajja, and Maimonides.” In *Studies in Medieval Jewish
History and Literature*, edited by Isadore Twersky, 82–109. Cambridge,
Mass.: Harvard University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Pines, 1979)

Pines, S. (1979). The limitations of human knowledge according to
Al-Farabi, ibn Bajja, and Maimonides. In I. Twersky (Ed.), *Studies in
medieval Jewish history and literature* (pp. 82–109). Cambridge, Mass.:
Harvard University Press.


}

@string{ hup     = {Harvard University Press} }

@InCollection{pines,
  author       = {Pines, Shlomo},
  editor       = {Twersky, Isadore},
  title        = {The Limitations of Human Knowledge According to {Al-Farabi}, {ibn
                  Bajja}, and {Maimonides}},
  date         = 1979,
  booktitle    = {Studies in Medieval {Jewish} History and Literature},
  publisher    = hup,
  location     = {Cambridge, Mass.},
  pages        = {82-109},
  keywords     = {secondary},
  hyphenation  = {american},
  indextitle   = {Limitations of Human Knowledge According to {Al-Farabi}, {ibn
                  Bajja}, and {Maimonides}, The},
  shorttitle   = {Limitations of Human Knowledge},
  annotation   = {A typical incollection entry. Note the
                  indextitle field},
}

^D
---
nocite: "[@*]"
references:
- annote: A typical incollection entry. Note the indextitle field
  author:
  - family: Pines
    given: Shlomo
  container-title: Studies in medieval Jewish history and literature
  editor:
  - family: Twersky
    given: Isadore
  id: pines
  issued: 1979
  keyword: secondary
  language: en-US
  page: 82-109
  publisher: Harvard University Press
  publisher-place: Cambridge, Mass.
  title: The limitations of human knowledge according to Al-Farabi, [ibn
    Bajja]{.nocase}, and Maimonides
  title-short: Limitations of human knowledge
  type: chapter
---


```
