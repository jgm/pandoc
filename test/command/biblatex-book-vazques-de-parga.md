```
% pandoc -f biblatex -t markdown -s
@comment{excerpted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

Note handling of Author = {Vázques{ de }Parga, Luis}

}

@book{vazques-de-parga,
	Annotation = {A multivolume book cited as a whole. This is a book entry with volumes, note, sorttitle, and indextitle fields},
	Author = {Vázques{ de }Parga, Luis and Lacarra, José María and Uría Ríu, Juan},
	Date = 1993,
	Hyphenation = {spanish},
	Indextitle = {Peregrinaciones a Santiago de Compostela, Las},
	Location = {Pamplona},
	Note = {Ed. facs. de la realizada en 1948--49},
	Publisher = {Iberdrola},
	Shorttitle = {Peregrinaciones},
	Sorttitle = {Peregrinaciones a Santiago de Compostela},
	Title = {Las Peregrinaciones a Santiago de Compostela},
	Volumes = 3}
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
