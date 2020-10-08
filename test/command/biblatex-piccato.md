```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Piccato 2001)

Piccato, Pablo. 2001. *City of Suspects: Crime in Mexico City,
1900–1931*. Durham; London: Duke University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Piccato, 2001)

Piccato, P. (2001). *City of suspects: Crime in Mexico City, 1900–1931*.
Durham; London: Duke University Press.


}

@Book{piccato,
  author       = {Piccato, Pablo},
  title        = {City of Suspects},
  date         = 2001,
  publisher    = {Duke University Press},
  location     = {Durham and London},
  hyphenation  = {american},
  subtitle     = {Crime in {Mexico City}, 1900--1931},
  shorttitle   = {City of Suspects},
  annotation   = {This is a book entry. Note the format of the
                  location field in the database file},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a book entry. Note the format of the location field in
    the database file
  author:
  - family: Piccato
    given: Pablo
  id: piccato
  issued: 2001
  language: en-US
  publisher: Duke University Press
  publisher-place: Durham; London
  title: "City of suspects: Crime in Mexico City, 1900--1931"
  title-short: City of suspects
  type: book
---


```
