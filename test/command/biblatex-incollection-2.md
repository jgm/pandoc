```
% pandoc -f biblatex -t markdown -s

@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}


@string{hup = {Harvard University Press}}


@incollection{westfahl:space,
	Annotation = {A cross-referenced article from a collection. This is an incollection entry with a crossref field. Note the subtitle and indextitle fields},
	Author = {Westfahl, Gary},
	Crossref = {westfahl:frontier},
	Hyphenation = {american},
	Indextitle = {True Frontier, The},
	Pages = {55-65},
	Subtitle = {Confronting and Avoiding the Realities of Space in {American} Science Fiction Films},
	Title = {The True Frontier}}

@incollection{gaonkar:in,
	Author = {Gaonkar, Dilip Parameshwar},
	Booktitle = {Alternative Modernities},
	Date = 2001,
	Editor = {Gaonkar, Dilip Parameshwar},
	Isbn = {0-822-32714-7},
	Location = {Durham and London},
	Pages = {1-23},
	Publisher = {Duke University Press},
	Title = {On Alternative Modernities}}

@collection{westfahl:frontier,
	Annotation = {This is a collection entry. Note the format of the location field as well as the subtitle and booksubtitle fields},
	Booksubtitle = {The Frontier Theme in Science Fiction},
	Booktitle = {Space and Beyond},
	Date = 2000,
	Editor = {Westfahl, Gary},
	Hyphenation = {american},
	Location = {Westport, Conn. and London},
	Publisher = {Greenwood},
	Subtitle = {The Frontier Theme in Science Fiction},
	Title = {Space and Beyond}}
^D
---
nocite: "[@*]"
references:
- annote: A cross-referenced article from a collection. This is an
    incollection entry with a crossref field. Note the subtitle and
    indextitle fields
  author:
  - family: Westfahl
    given: Gary
  container-title: "Space and beyond: The frontier theme in science
    fiction"
  editor:
  - family: Westfahl
    given: Gary
  id: "westfahl:space"
  issued: 2000
  language: en-US
  page: 55-65
  publisher: Greenwood
  publisher-place: Westport, Conn.; London
  title: "The true frontier: Confronting and avoiding the realities of
    space in American science fiction films"
  title-short: The true frontier
  type: chapter
- author:
  - family: Gaonkar
    given: Dilip Parameshwar
  container-title: Alternative modernities
  editor:
  - family: Gaonkar
    given: Dilip Parameshwar
  id: "gaonkar:in"
  isbn: 0-822-32714-7
  issued: 2001
  page: 1-23
  publisher: Duke University Press
  publisher-place: Durham; London
  title: On alternative modernities
  type: chapter
- annote: This is a collection entry. Note the format of the location
    field as well as the subtitle and booksubtitle fields
  editor:
  - family: Westfahl
    given: Gary
  id: "westfahl:frontier"
  issued: 2000
  language: en-US
  publisher: Greenwood
  publisher-place: Westport, Conn.; London
  title: "Space and beyond: The frontier theme in science fiction"
  title-short: Space and beyond
  type: book
---


```
