```
% pandoc -f biblatex -t markdown -s
@comment{
adapted from
http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

@book{averroes/bland,
	Annotation = {A book entry with a series and a number. Note the concatenation of the editor and translator fields as well as the indextitle field},
	Author = {Averroes},
	Date = 1982,
	Editor = {Bland, Kalman P.},
	Hyphenation = {american},
	Indextitle = {Epistle on the Possibility of Conjunction, The},
	Keywords = {primary},
	Location = {New York},
	Number = 7,
	Publisher = {Jewish Theological Seminary of America},
	Series = {{Moreshet: Studies in Jewish History, Literature and Thought}},
	Shorttitle = {Possibility of Conjunction},
	Title = {The Epistle on the Possibility of Conjunction with the Active Intellect by {Ibn Rushd} with the Commentary of {Moses Narboni}},
	Translator = {Bland, Kalman P.}}

@book{averroes/hannes,
	Annotation = {An annotated edition. Note the concatenation of the editor, translator, and annotator fields. Also note the shorttitle, indextitle, sorttitle, and indexsorttitle fields},
	Annotator = {Hannes, Ludwig},
	Author = {Averroes},
	Date = 1892,
	Editor = {Hannes, Ludwig},
	Hyphenation = {german},
	Indexsorttitle = {Uber die Moglichkeit der Conjunktion},
	Indextitle = {Über die Möglichkeit der Conjunktion},
	Keywords = {primary},
	Location = {Halle an der Saale},
	Publisher = {C.~A. Kaemmerer},
	Shorttitle = {Über die Möglichkeit der Conjunktion},
	Sorttitle = {Uber die Moglichkeit der Conjunktion},
	Title = {Des Averroës Abhandlung: \mkbibquote{Über die Möglichkeit der Conjunktion} oder \mkbibquote{Über den materiellen Intellekt}},
	Translator = {Hannes, Ludwig}}

@book{averroes/hercz,
	Annotation = {A book entry. Note the concatenation of the editor and translator fields as well as the indextitle and indexsorttitle fields},
	Author = {Averroes},
	Date = 1869,
	Editor = {Hercz, J.},
	Hyphenation = {german},
	Indexsorttitle = {Drei Abhandlungen uber die Conjunction},
	Indextitle = {Drei Abhandlungen über die Conjunction},
	Keywords = {primary},
	Location = {Berlin},
	Publisher = {S.~Hermann},
	Shorttitle = {Drei Abhandlungen},
	Subtitle = {Von Averroes (Vater und Sohn), aus dem Arabischen übersetzt von Samuel Ibn Tibbon},
	Title = {Drei Abhandlungen über die Conjunction des separaten Intellects mit dem Menschen},
	Translator = {Hercz, J.}}
^D
---
nocite: "[@*]"
references:
- annote: A book entry with a series and a number. Note the
    concatenation of the editor and translator fields as well as the
    indextitle field
  author:
  - family: Averroes
  collection-number: 7
  collection-title: "[Moreshet: Studies in Jewish History, Literature
    and Thought]{.nocase}"
  editor:
  - family: Bland
    given: Kalman P.
  id: averroes/bland
  issued: 1982
  keyword: primary
  language: en-US
  publisher: Jewish Theological Seminary of America
  publisher-place: New York
  title: The epistle on the possibility of conjunction with the active
    intellect by Ibn Rushd with the commentary of Moses Narboni
  title-short: Possibility of conjunction
  translator:
  - family: Bland
    given: Kalman P.
  type: book
- annote: An annotated edition. Note the concatenation of the editor,
    translator, and annotator fields. Also note the shorttitle,
    indextitle, sorttitle, and indexsorttitle fields
  author:
  - family: Averroes
  editor:
  - family: Hannes
    given: Ludwig
  id: averroes/hannes
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
- annote: A book entry. Note the concatenation of the editor and
    translator fields as well as the indextitle and indexsorttitle
    fields
  author:
  - family: Averroes
  editor:
  - family: Hercz
    given: J.
  id: averroes/hercz
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
