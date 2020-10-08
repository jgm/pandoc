```
% pandoc -f biblatex -t markdown -s
@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

TODO (as a stopgap):
Combine biblatex “volume = 7” and “part = 2” to CSL “volume: 7.2”

}

@book{coleridge,
	Annotation = {One (partial) volume of a multivolume book. This is a book entry with a volume and a part field which explicitly refers to the second (physical) part of the seventh (logical) volume. Also note the series and number fields},
	Author = {Coleridge, Samuel Taylor},
	Date = 1983,
	Editor = {Coburn, Kathleen and Engell, James and Bate, W. Jackson},
	Hyphenation = {british},
	Indextitle = {Biographia literaria},
	Location = {London},
	Maintitle = {The collected works of {Samuel Taylor Coleridge}},
	Number = 75,
	Part = 2,
	Publisher = {Routledge {and} Kegan Paul},
	Series = {Bollingen Series},
	Shorttitle = {Biographia literaria},
	Title = {Biographia literaria, or {Biographical} sketches of my literary life and opinions},
	Volume = 7}
^D
---
nocite: "[@*]"
references:
- annote: One (partial) volume of a multivolume book. This is a book
    entry with a volume and a part field which explicitly refers to the
    second (physical) part of the seventh (logical) volume. Also note
    the series and number fields
  author:
  - family: Coleridge
    given: Samuel Taylor
  collection-number: 75
  collection-title: Bollingen series
  editor:
  - family: Coburn
    given: Kathleen
  - family: Engell
    given: James
  - family: Bate
    given: W. Jackson
  id: coleridge
  issued: 1983
  language: en-GB
  publisher: Routledge and Kegan Paul
  publisher-place: London
  title: The collected works of Samuel Taylor Coleridge
  type: book
  volume: 7.2
  volume-title: Biographia literaria, or Biographical sketches of my
    literary life and opinions
---


```
