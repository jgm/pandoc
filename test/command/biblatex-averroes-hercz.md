```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Averroes 1869)

Averroes. 1869. *Drei Abhandlungen über die Conjunction des separaten
Intellects mit dem Menschen: Von Averroes (Vater und Sohn), aus dem
Arabischen übersetzt von Samuel Ibn Tibbon*. J. Hercz. Berlin:
S. Hermann.


Formatted with pandoc and apa.csl, 2013-10-23:

(Averroes, 1869)

Averroes. (1869). *Drei Abhandlungen über die Conjunction des separaten
Intellects mit dem Menschen: Von Averroes (Vater und Sohn), aus dem
Arabischen übersetzt von Samuel Ibn Tibbon*. (J. Hercz). Berlin:
S. Hermann.


NOTES:

- citeproc
	- term "edited and translated by" missing

}

@Book{averroes-hercz,
  author       = {Averroes},
  title        = {Drei Abhandlungen {\"u}ber die Conjunction des separaten
                  Intellects mit dem Menschen},
  date         = 1869,
  editor       = {Hercz, J.},
  translator   = {Hercz, J.},
  publisher    = {S.~Hermann},
  location     = {Berlin},
  keywords     = {primary},
  hyphenation  = {german},
  indexsorttitle= {Drei Abhandlungen uber die Conjunction},
  indextitle   = {Drei Abhandlungen {\"u}ber die Conjunction},
  subtitle     = {Von Averroes (Vater und Sohn), aus dem Arabischen
                  {\"u}bersetzt von Samuel Ibn Tibbon},
  shorttitle   = {Drei Abhandlungen},
  annotation   = {A book entry. Note the concatenation of the
                  editor and translator fields as well as the
                  indextitle and indexsorttitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry. Note the concatenation of the editor and
    translator fields as well as the indextitle and indexsorttitle
    fields
  author:
  - family: Averroes
  editor:
  - family: Hercz
    given: J.
  id: averroes-hercz
  issued: 1869
  keyword: primary
  language: de-DE
  publisher: S. Hermann
  publisher-place: Berlin
  title: "Drei Abhandlungen über die Conjunction des separaten
    Intellects mit dem Menschen: Von Averroes (Vater und Sohn), aus dem
    Arabischen übersetzt von Samuel Ibn Tibbon"
  title-short: Drei Abhandlungen
  translator:
  - family: Hercz
    given: J.
  type: book
---


```
