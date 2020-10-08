```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Gerhardt 2000)

Gerhardt, Michael J. 2000. *The Federal Appointments Process: A
Constitutional and Historical Analysis*. Durham; London: Duke University
Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Gerhardt, 2000)

Gerhardt, M. J. (2000). *The federal appointments process: A
constitutional and historical analysis*. Durham; London: Duke University
Press.


}

@Book{gerhardt,
  author       = {Gerhardt, Michael J.},
  title        = {The Federal Appointments Process},
  date         = 2000,
  publisher    = {Duke University Press},
  location     = {Durham and London},
  hyphenation  = {american},
  sorttitle    = {Federal Appointments Process},
  indextitle   = {Federal Appointments Process, The},
  subtitle     = {A Constitutional and Historical Analysis},
  shorttitle   = {Federal Appointments Process},
  annotation   = {This is a book entry. Note the format of the
                  location field as well as the sorttitle and
                  indextitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a book entry. Note the format of the location field as
    well as the sorttitle and indextitle fields
  author:
  - family: Gerhardt
    given: Michael J.
  id: gerhardt
  issued: 2000
  language: en-US
  publisher: Duke University Press
  publisher-place: Durham; London
  title: "The federal appointments process: A constitutional and
    historical analysis"
  title-short: Federal appointments process
  type: book
---


```
