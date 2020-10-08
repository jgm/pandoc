```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Hammond 1997)

Hammond, Christopher. 1997. *The Basics of Crystallography and
Diffraction*. Oxford: International Union of Crystallography; Oxford
University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Hammond, 1997)

Hammond, C. (1997). *The basics of crystallography and diffraction*.
Oxford: International Union of Crystallography; Oxford University Press.


}

@Book{hammond,
  author       = {Hammond, Christopher},
  title        = {The basics of crystallography and diffraction},
  date         = 1997,
  publisher    = {International Union of Crystallography and Oxford University
                  Press},
  location     = {Oxford},
  hyphenation  = {british},
  sorttitle    = {Basics of crystallography and diffraction},
  indextitle   = {Basics of crystallography and diffraction, The},
  shorttitle   = {Crystallography and diffraction},
  annotation   = {A book entry. Note the sorttitle and
                  indextitle fields as well as the format of the
                  publisher field},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry. Note the sorttitle and indextitle fields as well
    as the format of the publisher field
  author:
  - family: Hammond
    given: Christopher
  id: hammond
  issued: 1997
  language: en-GB
  publisher: International Union of Crystallography; Oxford University
    Press
  publisher-place: Oxford
  title: The basics of crystallography and diffraction
  title-short: Crystallography and diffraction
  type: book
---


```
