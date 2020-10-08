```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Worman 2002)

Worman, Nancy. 2002. *The Cast of Character: Style in Greek Literature*.
Austin: University of Texas Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Worman, 2002)

Worman, N. (2002). *The cast of character: Style in Greek literature*.
Austin: University of Texas Press.


}

@Book{worman,
  author       = {Worman, Nancy},
  title        = {The Cast of Character},
  date         = 2002,
  publisher    = {University of Texas Press},
  location     = {Austin},
  hyphenation  = {american},
  sorttitle    = {Cast of Character},
  indextitle   = {Cast of Character, The},
  subtitle     = {Style in {Greek} Literature},
  shorttitle   = {Cast of Character},
  annotation   = {A book entry. Note the sorttitle and
                  indextitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry. Note the sorttitle and indextitle fields
  author:
  - family: Worman
    given: Nancy
  id: worman
  issued: 2002
  language: en-US
  publisher: University of Texas Press
  publisher-place: Austin
  title: "The cast of character: Style in Greek literature"
  title-short: Cast of character
  type: book
---


```
