```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Cicero 1995)

Cicero, Marcus Tullius. 1995. *De natura deorum. Über das Wesen der
Götter*. Ursula Blank-Sangmeister. Stuttgart: Reclam.


Formatted with pandoc and apa.csl, 2013-10-23:

(Cicero, 1995)

Cicero, M. T. (1995). *De natura deorum. Über das Wesen der Götter*. (U.
Blank-Sangmeister). Stuttgart: Reclam.


NOTES:

- biblio2yaml
	- afterword, language: no CSL variables available

- citeproc:
	- term such as "edited and translated by" should appear

}

@Book{cicero,
  author       = {Cicero, Marcus Tullius},
  title        = {De natura deorum. {\"U}ber das Wesen der G{\"o}tter},
  date         = 1995,
  editor       = {Blank-Sangmeister, Ursula},
  translator   = {Blank-Sangmeister, Ursula},
  afterword    = {Thraede, Klaus},
  language     = {langlatin and langgerman},
  publisher    = {Reclam},
  location     = {Stuttgart},
  hyphenation  = {german},
  indextitle   = {De natura deorum},
  shorttitle   = {De natura deorum},
  annotation   = {A bilingual edition of Cicero's \emph{De natura deorum}, with
                  a German translation. Note the format of the language
                  field in the database file, the concatenation of the
                  editor and translator fields, and the
                  afterword field},
}

^D
---
nocite: "[@*]"
references:
- annote: A bilingual edition of Cicero's *De natura deorum*, with a
    German translation. Note the format of the language field in the
    database file, the concatenation of the editor and translator
    fields, and the afterword field
  author:
  - family: Cicero
    given: Marcus Tullius
  editor:
  - family: Blank-Sangmeister
    given: Ursula
  id: cicero
  issued: 1995
  language: de-DE
  publisher: Reclam
  publisher-place: Stuttgart
  title: De natura deorum. Über das Wesen der Götter
  title-short: De natura deorum
  translator:
  - family: Blank-Sangmeister
    given: Ursula
  type: book
---


```
