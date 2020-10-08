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

NOTES:

- biblio2yaml
	- "related = {vizedom:related}, relatedtype = {bytranslator}": no equivalent implemented in CSL
	- "options = {useprefix}," is shorthand for "options = {useprefix=true},"


}

@Book{vangennep:related,
  author       = {van Gennep, Arnold},
  title        = {Les rites de passage},
  date         = 1909,
  publisher    = {Nourry},
  location     = {Paris},
  options      = {useprefix},
  hyphenation  = {french},
  related      = {vizedom:related},
  relatedtype  = {bytranslator},
  sorttitle    = {Rites de passage},
  indextitle   = {Rites de passage, Les},
  shorttitle   = {Rites de passage},
  annotation   = {A variant of the vangennep entry related to its
                  translation. Note the format of the related and
                  relatedtype fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A variant of the vangennep entry related to its translation.
    Note the format of the related and relatedtype fields
  author:
  - family: Gennep
    given: Arnold
    non-dropping-particle: van
  id: "vangennep:related"
  issued: 1909
  language: fr-FR
  publisher: Nourry
  publisher-place: Paris
  title: Les rites de passage
  title-short: Rites de passage
  type: book
---


```
