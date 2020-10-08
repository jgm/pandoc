```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(van Gennep 1909)

van Gennep, Arnold. 1909. *Les rites de passage*. Paris: Nourry.


Formatted with pandoc and apa.csl, 2013-10-23:

(van Gennep, 1909)

van Gennep, A. (1909). *Les rites de passage*. Paris: Nourry.


}

@Book{vangennep,
  author       = {van Gennep, Arnold},
  title        = {Les rites de passage},
  date         = 1909,
  publisher    = {Nourry},
  location     = {Paris},
  options      = {useprefix},
  hyphenation  = {french},
  sorttitle    = {Rites de passage},
  indextitle   = {Rites de passage, Les},
  shorttitle   = {Rites de passage},
  annotation   = {A book entry. Note the format of the printed name and
                  compare the useprefix option in the options
                  field as well as brandt and geer},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry. Note the format of the printed name and compare
    the useprefix option in the options field as well as brandt and geer
  author:
  - family: Gennep
    given: Arnold
    non-dropping-particle: van
  id: vangennep
  issued: 1909
  language: fr-FR
  publisher: Nourry
  publisher-place: Paris
  title: Les rites de passage
  title-short: Rites de passage
  type: book
---


```
