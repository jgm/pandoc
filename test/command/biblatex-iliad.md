```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Homer 2004)

Homer. 2004. *Die Ilias*. Translated by Wolfgang Schadewaldt. 3rd ed.
Düsseldorf; Zürich: Artemis & Winkler.


Formatted with pandoc and apa.csl, 2013-10-23:

(Homer, 2004)

Homer. (2004). *Die Ilias*. (W. Schadewaldt, trans.) (3rd ed.).
Düsseldorf; Zürich: Artemis & Winkler.

}

@Book{iliad,
  author       = {Homer},
  title        = {Die Ilias},
  date         = 2004,
  translator   = {Schadewaldt, Wolfgang},
  introduction = {Latacz, Joachim},
  edition      = 3,
  publisher    = {Artemis \& Winkler},
  location     = {D{\"u}sseldorf and Z{\"u}rich},
  hyphenation  = {german},
  sorttitle    = {Ilias},
  indextitle   = {Ilias, Die},
  shorttitle   = {Ilias},
  annotation   = {A German translation of the \emph{Iliad}. Note the
                  translator and introduction fields and the
                  format of the location field in the database
                  file. Also note the sorttitle and indextitle
                  fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A German translation of the *Iliad*. Note the translator and
    introduction fields and the format of the location field in the
    database file. Also note the sorttitle and indextitle fields
  author:
  - family: Homer
  edition: 3
  id: iliad
  issued: 2004
  language: de-DE
  publisher: Artemis & Winkler
  publisher-place: Düsseldorf; Zürich
  title: Die Ilias
  title-short: Ilias
  translator:
  - family: Schadewaldt
    given: Wolfgang
  type: book
---


```
