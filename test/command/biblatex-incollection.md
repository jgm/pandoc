```
% pandoc -f biblatex -t markdown -s
@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

@string{hup = {Harvard University Press}}

@incollection{brandt,
	Annotation = {An incollection entry with a series and a number. Note the format of the printed name and compare the useprefix option in the options field as well as vangennep. Also note the indextitle, and indexsorttitle fields},
	Author = {von Brandt, Ahasver and Hoffmann, Erich},
	Booktitle = {Europa im Hoch- und Spätmittelalter},
	Date = 1987,
	Editor = {Seibt, Ferdinand},
	Hyphenation = {german},
	Indexsorttitle = {Nordischen Lander von der Mitte des 11. Jahrhunderts bis 1448},
	Indextitle = {Nordischen Länder von der Mitte des 11.~Jahrhunderts bis 1448, Die},
	Location = {Stuttgart},
	Number = 2,
	Options = {useprefix=false},
	Pages = {884-917},
	Publisher = {Klett-Cotta},
	Series = {Handbuch der europäischen Geschichte},
	Shorttitle = {Die nordischen Länder},
	Title = {Die nordischen Länder von der Mitte des 11.~Jahrhunderts bis 1448}}

@incollection{hyman,
	Annotation = {An incollection entry with a series and number field},
	Author = {Hyman, Arthur},
	Booktitle = {Studies in {Aristotle}},
	Date = 1981,
	Editor = {O'Meara, Dominic J.},
	Hyphenation = {american},
	Indextitle = {Aristotle's Theory of the Intellect},
	Keywords = {secondary},
	Location = {Washington, D.C.},
	Number = 9,
	Pages = {161-191},
	Publisher = {The Catholic University of America Press},
	Series = {Studies in Philosophy and the History of Philosophy},
	Shorttitle = {Aristotle's Theory of the Intellect},
	Title = {Aristotle's Theory of the Intellect and its Interpretation by {Averroes}}}

@incollection{pines,
	Annotation = {A typical incollection entry. Note the indextitle field},
	Author = {Pines, Shlomo},
	Booktitle = {Studies in Medieval {Jewish} History and Literature},
	Date = 1979,
	Editor = {Twersky, Isadore},
	Hyphenation = {american},
	Indextitle = {Limitations of Human Knowledge According to Al-Farabi, ibn Bajja, and Maimonides, The},
	Keywords = {secondary},
	Location = {Cambridge, Mass.},
	Pages = {82-109},
	Publisher = hup,
	Shorttitle = {Limitations of Human Knowledge},
	Title = {The Limitations of Human Knowledge According to {Al-Farabi}, {ibn Bajja}, and {Maimonides}}}

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
- annote: An incollection entry with a series and number field
  author:
  - family: Hyman
    given: Arthur
  collection-number: 9
  collection-title: Studies in philosophy and the history of philosophy
  container-title: Studies in Aristotle
  editor:
  - family: O'Meara
    given: Dominic J.
  id: hyman
  issued: 1981
  keyword: secondary
  language: en-US
  page: 161-191
  publisher: The Catholic University of America Press
  publisher-place: Washington, D.C.
  title: Aristotle's theory of the intellect and its interpretation by
    Averroes
  title-short: Aristotle's theory of the intellect
  type: chapter
- annote: A typical incollection entry. Note the indextitle field
  author:
  - family: Pines
    given: Shlomo
  container-title: Studies in medieval Jewish history and literature
  editor:
  - family: Twersky
    given: Isadore
  id: pines
  issued: 1979
  keyword: secondary
  language: en-US
  page: 82-109
  publisher: Harvard University Press
  publisher-place: Cambridge, Mass.
  title: The limitations of human knowledge according to Al-Farabi, [ibn
    Bajja]{.nocase}, and Maimonides
  title-short: Limitations of human knowledge
  type: chapter
---


```
