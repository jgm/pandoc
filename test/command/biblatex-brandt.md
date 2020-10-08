```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Brandt and Hoffmann 1987)

Brandt, Ahasver von, and Erich Hoffmann. 1987. “Die nordischen Länder
von der Mitte des 11. Jahrhunderts bis 1448.” In *Europa im Hoch- und
Spätmittelalter*, edited by Ferdinand Seibt, 884–917. Handbuch der
europäischen Geschichte 2. Stuttgart: Klett-Cotta.


Formatted with pandoc and apa.csl, 2013-10-23:

(Brandt & Hoffmann, 1987)

Brandt, A. von, & Hoffmann, E. (1987). Die nordischen Länder von der
Mitte des 11. Jahrhunderts bis 1448. In F. Seibt (ed.), *Europa im Hoch-
und Spätmittelalter* (pp. 884–917). Stuttgart: Klett-Cotta.


}

@InCollection{brandt,
  author       = {von Brandt, Ahasver and Erich Hoffmann},
  editor       = {Ferdinand Seibt},
  title        = {Die nordischen L{\"a}nder von der Mitte des 11.~Jahrhunderts
                  bis 1448},
  date         = 1987,
  booktitle    = {Europa im Hoch- und Sp{\"a}tmittelalter},
  series       = {Handbuch der europ{\"a}ischen Geschichte},
  number       = 2,
  publisher    = {Klett-Cotta},
  location     = {Stuttgart},
  pages        = {884-917},
  options      = {useprefix=false},
  hyphenation  = {german},
  indexsorttitle= {Nordischen Lander von der Mitte des 11. Jahrhunderts bis
                  1448},
  indextitle   = {Nordischen L{\"a}nder von der Mitte des 11.~Jahrhunderts bis
                  1448, Die},
  shorttitle   = {Die nordischen L{\"a}nder},
  annotation   = {An incollection entry with a series and a
                  number. Note the format of the printed name and
                  compare the useprefix option in the options
                  field as well as vangennep. Also note the
                  indextitle, and indexsorttitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: An incollection entry with a series and a number. Note the
    format of the printed name and compare the useprefix option in the
    options field as well as vangennep. Also note the indextitle, and
    indexsorttitle fields
  author:
  - dropping-particle: von
    family: Brandt
    given: Ahasver
  - family: Hoffmann
    given: Erich
  collection-number: 2
  collection-title: Handbuch der europäischen Geschichte
  container-title: Europa im Hoch- und Spätmittelalter
  editor:
  - family: Seibt
    given: Ferdinand
  id: brandt
  issued: 1987
  language: de-DE
  page: 884-917
  publisher: Klett-Cotta
  publisher-place: Stuttgart
  title: Die nordischen Länder von der Mitte des 11. Jahrhunderts bis
    1448
  title-short: Die nordischen Länder
  type: chapter
---


```
