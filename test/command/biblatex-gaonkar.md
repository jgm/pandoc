```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Gaonkar 2001)

Gaonkar, Dilip Parameshwar, ed. 2001. *Alternative Modernities*. Durham;
London: Duke University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Gaonkar, 2001)

Gaonkar, D. P. (Ed.). (2001). *Alternative modernities*. Durham; London:
Duke University Press.


}

@Collection{gaonkar,
  editor       = {Gaonkar, Dilip Parameshwar},
  title        = {Alternative Modernities},
  date         = 2001,
  publisher    = {Duke University Press},
  location     = {Durham and London},
  isbn         = {0-822-32714-7},
  hyphenation  = {american},
  annotation   = {This is a collection entry. Note the format of the
                  location field in the database file as well as the
                  isbn field},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a collection entry. Note the format of the location
    field in the database file as well as the isbn field
  editor:
  - family: Gaonkar
    given: Dilip Parameshwar
  id: gaonkar
  isbn: 0-822-32714-7
  issued: 2001
  language: en-US
  publisher: Duke University Press
  publisher-place: Durham; London
  title: Alternative modernities
  type: book
---


```
