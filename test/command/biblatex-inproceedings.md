```
% pandoc -f biblatex -t markdown -s
@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

@string{cup = {Cambridge University Press}}

@inproceedings{moraux,
	Annotation = {This is a typical inproceedings entry. Note the booksubtitle, shorttitle, indextitle, and indexsorttitle fields. Also note the eventdate field.},
	Author = {Moraux, Paul},
	Booktitle = {Aristotle on Mind and the Senses},
	Booktitleaddon = {Proceedings of the Seventh Symposium Aristotelicum},
	Date = 1979,
	Editor = {Lloyd, G. E. R. and Owen, G. E. L.},
	Eventdate = 1975,
	Hyphenation = {french},
	Indexsorttitle = {De Anima dans la tradition grecque},
	Indextitle = {\emph{De Anima} dans la tradition grècque, Le},
	Keywords = {secondary},
	Location = {Cambridge},
	Pages = {281-324},
	Publisher = cup,
	Shorttitle = {\emph{De Anima} dans la tradition grècque},
	Subtitle = {Quelques aspects de l'interpretation du traité, de Theophraste à Themistius},
	Title = {Le \emph{De Anima} dans la tradition grècque}}

@inproceedings{salam,
	Author = {Salam, Abdus},
	Booksubtitle = {Relativistic groups and analyticity},
	Booktitle = {Elementary particle theory},
	Booktitleaddon = {Proceedings of the Eighth {Nobel} Symposium},
	Date = 1968,
	Editor = {Svartholm, Nils},
	Eventdate = {1968-05-19/1968-05-25},
	Location = {Stockholm},
	Pages = {367-377},
	Publisher = {Almquist \& Wiksell},
	Title = {Weak and Electromagnetic Interactions},
	Venue = {Aspenäsgarden, Lerum}}
^D
---
nocite: "[@*]"
references:
- annote: This is a typical inproceedings entry. Note the booksubtitle,
    shorttitle, indextitle, and indexsorttitle fields. Also note the
    eventdate field.
  author:
  - family: Moraux
    given: Paul
  container-title: Aristotle on Mind and the Senses. Proceedings of the
    Seventh Symposium Aristotelicum
  editor:
  - family: Lloyd
    given: G. E. R.
  - family: Owen
    given: G. E. L.
  event-date: 1975
  id: moraux
  issued: 1979
  keyword: secondary
  language: fr-FR
  page: 281-324
  publisher: Cambridge University Press
  publisher-place: Cambridge
  title: "Le *De Anima* dans la tradition grècque: Quelques aspects de
    l'interpretation du traité, de Theophraste à Themistius"
  title-short: "*De Anima* dans la tradition grècque"
  type: paper-conference
- author:
  - family: Salam
    given: Abdus
  container-title: "Elementary particle theory: Relativistic groups and
    analyticity. Proceedings of the eighth Nobel symposium"
  editor:
  - family: Svartholm
    given: Nils
  event-date: 1968-05-19/1968-05-25
  event-place: Aspenäsgarden, Lerum
  id: salam
  issued: 1968
  page: 367-377
  publisher: Almquist & Wiksell
  publisher-place: Stockholm
  title: Weak and electromagnetic interactions
  type: paper-conference
---


```
