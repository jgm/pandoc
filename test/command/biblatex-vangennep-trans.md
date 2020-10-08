```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(van Gennep 1960)

van Gennep, Arnold. 1960. *The Rites of Passage*. Translated by Monika
B. Vizedom and Gabrielle L. Caffee. University of Chicago Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(van Gennep, 1960)

van Gennep, A. (1960). *The rites of passage*. (M. B. Vizedom & G. L.
Caffee, Trans.). University of Chicago Press.


}

@Book{vangennep:trans,
  author       = {van Gennep, Arnold},
  title        = {The Rites of Passage},
  year         = 1960,
  translator   = {Vizedom, Monika B. and Caffee, Gabrielle L.},
  language     = {english},
  origlanguage = {french},
  publisher    = {University of Chicago Press},
  options      = {useprefix},
  indextitle   = {Rites of Passage, The},
  sorttitle    = {Rites of Passage},
  shorttitle   = {Rites of Passage},
  hyphenation  = {american},
  annotation   = {A translation of the vangennep entry. Note the
                  translator and origlanguage fields. Compare
                  with the vangennep:related entry.},
}

^D
---
nocite: "[@*]"
references:
- annote: "A translation of the vangennep entry. Note the translator and
    origlanguage fields. Compare with the vangennep:related entry."
  author:
  - family: Gennep
    given: Arnold
    non-dropping-particle: van
  id: "vangennep:trans"
  issued: 1960
  language: en-US
  publisher: University of Chicago Press
  title: The rites of passage
  title-short: Rites of passage
  translator:
  - family: Vizedom
    given: Monika B.
  - family: Caffee
    given: Gabrielle L.
  type: book
---


```
