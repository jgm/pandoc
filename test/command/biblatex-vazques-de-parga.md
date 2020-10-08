```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Vázques de Parga, Lacarra, and Uría Ríu 1993)

Vázques de Parga, Luis, José María Lacarra, and Juan Uría Ríu. 1993.
*Las Peregrinaciones a Santiago de Compostela*. 3. Pamplona: Iberdrola.


Formatted with pandoc and apa.csl, 2013-10-23:

(Vázques de Parga, Lacarra, & Uría Ríu, 1993)

Vázques de Parga, L., Lacarra, J. M., & Uría Ríu, J. (1993). *Las
Peregrinaciones a Santiago de Compostela* (1-3). Pamplona: Iberdrola.


NOTES:

- citeproc
	- term "vols." missing

}

@Book{vazques-de-parga,
  author       = {V{\'a}zques{ de }Parga, Luis and Lacarra, Jos{\'e} Mar{\'i}a
                  and Ur{\'i}a R{\'i}u, Juan},
  title        = {Las Peregrinaciones a Santiago de Compostela},
  date         = 1993,
  volumes      = 3,
  note         = {Ed. facs. de la realizada en 1948--49},
  publisher    = {Iberdrola},
  location     = {Pamplona},
  hyphenation  = {spanish},
  sorttitle    = {Peregrinaciones a Santiago de Compostela},
  indextitle   = {Peregrinaciones a Santiago de Compostela, Las},
  shorttitle   = {Peregrinaciones},
  annotation   = {A multivolume book cited as a whole. This is a book
                  entry with volumes, note,
                  sorttitle, and indextitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A multivolume book cited as a whole. This is a book entry with
    volumes, note, sorttitle, and indextitle fields
  author:
  - family: Vázques de Parga
    given: Luis
  - family: Lacarra
    given: José María
  - family: Uría Ríu
    given: Juan
  id: vazques-de-parga
  issued: 1993
  language: es-ES
  note: Ed. facs. de la realizada en 1948--49
  number-of-volumes: 3
  publisher: Iberdrola
  publisher-place: Pamplona
  title: Las Peregrinaciones a Santiago de Compostela
  title-short: Peregrinaciones
  type: book
---


```
