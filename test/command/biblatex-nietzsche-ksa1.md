```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2015-03-08:

(Nietzsche 1988)

Nietzsche, Friedrich. 1988. *Sämtliche Werke: Kritische Studienausgabe*.
Edited by Giorgio Colli and Mazzino Montinari. 2nd ed. Vol. 1. München;
Berlin; New York: Deutscher Taschenbuch-Verlag; Walter de Gruyter.


Formatted with pandoc and apa.csl, 2015-03-08:

(Nietzsche, 1988)

Nietzsche, F. (1988). *Sämtliche Werke: Kritische Studienausgabe*. (G.
Colli & M. Montinari, eds., F. Nietzsche) (2nd ed., Vol. 1). München;
Berlin; New York: Deutscher Taschenbuch-Verlag; Walter de Gruyter.


NOTES:

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@string{ dtv     = {Deutscher Taschenbuch-Verlag} }

@Book{nietzsche:ksa1,
  author       = {Nietzsche, Friedrich},
  title        = {Die Geburt der Trag{\"o}die. Unzeitgem{\"a}{\ss}e
                  Betrachtungen I--IV. Nachgelassene Schriften 1870--1973},
  date         = 1988,
  editor       = {Colli, Giorgio and Montinari, Mazzino},
  maintitle    = {S{\"a}mtliche Werke},
  mainsubtitle = {Kritische Studienausgabe},
  volume       = 1,
  edition      = 2,
  publisher    = dtv # { and Walter de Gruyter},
  location     = {M{\"u}nchen and Berlin and New York},
  hyphenation  = {german},
  sortyear     = {1988-1},
  sorttitle    = {Werke-01-000},
  indexsorttitle= {Samtliche Werke I},
  bookauthor   = {Nietzsche, Friedrich},
  indextitle   = {S{\"a}mtliche Werke I},
  shorttitle   = {S{\"a}mtliche Werke I},
  annotation   = {A single volume from the critical edition of Nietzsche's
                  works. This book entry explicitly refers to the first
                  volume only. Note the title and maintitle
                  fields. Also note the sorttitle and sortyear
                  fields. We want this entry to be listed after the entry
                  referring to the entire edition},
}

^D
---
nocite: "[@*]"
references:
- annote: A single volume from the critical edition of Nietzsche's
    works. This book entry explicitly refers to the first volume only.
    Note the title and maintitle fields. Also note the sorttitle and
    sortyear fields. We want this entry to be listed after the entry
    referring to the entire edition
  author:
  - family: Nietzsche
    given: Friedrich
  container-author:
  - family: Nietzsche
    given: Friedrich
  edition: 2
  editor:
  - family: Colli
    given: Giorgio
  - family: Montinari
    given: Mazzino
  id: "nietzsche:ksa1"
  issued: 1988
  language: de-DE
  publisher: Deutscher Taschenbuch-Verlag; Walter de Gruyter
  publisher-place: München; Berlin; New York
  title: "Sämtliche Werke: Kritische Studienausgabe"
  type: book
  volume: 1
  volume-title: Die Geburt der Tragödie. Unzeitgemäße Betrachtungen
    I--IV. Nachgelassene Schriften 1870--1973
---


```
