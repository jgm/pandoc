```
% pandoc -f biblatex -t markdown -s
@comment{excerpt from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

\mkbibquote{} should be replaced by a matching set of quotation marks that can be used by citeproc for quote substitution and flipflopping. English smart double quotation marks seem best, as they cannot be confused with apostrophes.

\enquote{}, \enquote*{} should be replaced by a matching set of quotation marks, too: “foo”, ‘bar’.
}

@string{pup = {Princeton University Press}}

@book{nussbaum,
	Annotation = {A book entry. Note the sorttitle and indexsorttitle fields and the markup of the quotes in the database file},
	Author = {Nussbaum, Martha},
	Date = 1978,
	Hyphenation = {american},
	Indexsorttitle = {Aristotle's De Motu Animalium},
	Keywords = {secondary},
	Location = {Princeton},
	Publisher = pup,
	Sorttitle = {Aristotle's De Motu Animalium},
	Title = {Aristotle's \mkbibquote{De Motu Animalium}}}

^D
---
nocite: "[@*]"
references:
- annote: A book entry. Note the sorttitle and indexsorttitle fields and
    the markup of the quotes in the database file
  author:
  - family: Nussbaum
    given: Martha
  id: nussbaum
  issued: 1978
  keyword: secondary
  language: en-US
  publisher: Princeton University Press
  publisher-place: Princeton
  title: Aristotle's "De Motu Animalium"
  type: book
---


```
