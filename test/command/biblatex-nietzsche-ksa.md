```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Nietzsche 1988)

Nietzsche, Friedrich. 1988. *Sämtliche Werke: Kritische Studienausgabe*.
Edited by Giorgio Colli and Mazzino Montinari. 2nd ed. 15. München;
Berlin; New York: Deutscher Taschenbuch-Verlag; Walter de Gruyter.


Formatted with pandoc and apa.csl, 2013-10-23:

(Nietzsche, 1988)

Nietzsche, F. (1988). *Sämtliche Werke: Kritische Studienausgabe*. (G.
Colli & M. Montinari, eds.) (2nd ed., 1-15). München; Berlin; New York:
Deutscher Taschenbuch-Verlag; Walter de Gruyter.


NOTES:

- biblio2yaml
	- term "vols." missing

}

@string{ dtv     = {Deutscher Taschenbuch-Verlag} }

@Book{nietzsche:ksa,
  author       = {Nietzsche, Friedrich},
  title        = {S{\"a}mtliche Werke},
  date         = 1988,
  editor       = {Colli, Giorgio and Montinari, Mazzino},
  edition      = 2,
  volumes      = 15,
  publisher    = dtv # { and Walter de Gruyter},
  location     = {M{\"u}nchen and Berlin and New York},
  hyphenation  = {german},
  sortyear     = {1988-0},
  sorttitle    = {Werke-00-000},
  indexsorttitle= {Samtliche Werke},
  subtitle     = {Kritische Studienausgabe},
  annotation   = {The critical edition of Nietzsche's works. This is a
                  book entry referring to a 15-volume work as a
                  whole. Note the volumes field and the format of the
                  publisher and location fields in the
                  database file. Also note the sorttitle and
                  sortyear fields which are used to fine-tune the
                  sorting order of the bibliography. We want this item listed
                  first in the bibliography},
}

^D
---
nocite: "[@*]"
references:
- annote: The critical edition of Nietzsche's works. This is a book
    entry referring to a 15-volume work as a whole. Note the volumes
    field and the format of the publisher and location fields in the
    database file. Also note the sorttitle and sortyear fields which are
    used to fine-tune the sorting order of the bibliography. We want
    this item listed first in the bibliography
  author:
  - family: Nietzsche
    given: Friedrich
  edition: 2
  editor:
  - family: Colli
    given: Giorgio
  - family: Montinari
    given: Mazzino
  id: "nietzsche:ksa"
  issued: 1988
  language: de-DE
  number-of-volumes: 15
  publisher: Deutscher Taschenbuch-Verlag; Walter de Gruyter
  publisher-place: München; Berlin; New York
  title: "Sämtliche Werke: Kritische Studienausgabe"
  title-short: Sämtliche Werke
  type: book
---


```
