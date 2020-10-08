```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Averroes 1892)

Averroes. 1892. *Des Averroës Abhandlung: “Über die Möglichkeit der
Conjunktion” oder “Über den materiellen Intellekt”*. Ludwig Hannes.
Halle an der Saale: C. A. Kaemmerer.


Formatted with pandoc and apa.csl, 2013-10-23:

(Averroes, 1892)

Averroes. (1892). *Des Averroës Abhandlung: “Über die Möglichkeit der
Conjunktion” oder “Über den materiellen Intellekt”*. (L. Hannes). Halle
an der Saale: C. A. Kaemmerer.


NOTES:

- citeproc
	- term "edited and translated by" missing

}

@Book{averroes-hannes,
  author       = {Averroes},
  title        = {Des Averro{\"e}s Abhandlung: \mkbibquote{{\"U}ber die
                  M{\"o}glichkeit der Conjunktion} oder \mkbibquote{{\"U}ber den
                  materiellen Intellekt}},
  date         = 1892,
  editor       = {Hannes, Ludwig},
  translator   = {Hannes, Ludwig},
  annotator    = {Hannes, Ludwig},
  publisher    = {C.~A. Kaemmerer},
  location     = {Halle an der Saale},
  keywords     = {primary},
  hyphenation  = {german},
  sorttitle    = {Uber die Moglichkeit der Conjunktion},
  indexsorttitle= {Uber die Moglichkeit der Conjunktion},
  indextitle   = {{\"U}ber die M{\"o}glichkeit der Conjunktion},
  shorttitle   = {{\"U}ber die M{\"o}glichkeit der Conjunktion},
  annotation   = {An annotated edition. Note the concatenation of the
                  editor, translator, and annotator
                  fields. Also note the shorttitle,
                  indextitle, sorttitle, and
                  indexsorttitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: An annotated edition. Note the concatenation of the editor,
    translator, and annotator fields. Also note the shorttitle,
    indextitle, sorttitle, and indexsorttitle fields
  author:
  - family: Averroes
  editor:
  - family: Hannes
    given: Ludwig
  id: averroes-hannes
  issued: 1892
  keyword: primary
  language: de-DE
  publisher: C. A. Kaemmerer
  publisher-place: Halle an der Saale
  title: "Des Averroës Abhandlung: \"Über die Möglichkeit der
    Conjunktion\" oder \"Über den materiellen Intellekt\""
  title-short: Über die Möglichkeit der Conjunktion
  translator:
  - family: Hannes
    given: Ludwig
  type: book
---


```
