```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Vizedom and Caffee 1960)

Vizedom, Monika B., and Gabrielle L. Caffee, trans. 1960. *The Rites of
Passage*. University of Chicago Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Vizedom & Caffee, 1960)

Vizedom, M. B., & Caffee, G. L. (Trans.). (1960). *The rites of
passage*. University of Chicago Press.


NOTES:

- biblio2yaml
	- "related = {vangennep}, relatedtype = {translationof}": no equivalent implemented in CSL

}

@Book{vizedom:related,
  title        = {The Rites of Passage},
  year         = 1960,
  translator   = {Vizedom, Monika B. and Caffee, Gabrielle L.},
  language     = {english},
  publisher    = {University of Chicago Press},
  hyphenation  = {american},
  options      = {usetranslator},
  related      = {vangennep},
  relatedtype  = {translationof},
  indextitle   = {Rites of Passage, The},
  sorttitle    = {Rites of Passage},
  shorttitle   = {Rites of Passage},
  annotation   = {A translated work from vangennep. Note the format of
                  the related and relatedtype fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A translated work from vangennep. Note the format of the
    related and relatedtype fields
  id: "vizedom:related"
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
