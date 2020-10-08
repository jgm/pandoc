```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Nussbaum 1978)

Nussbaum, Martha. 1978. *Aristotle’s “De Motu Animalium”*. Princeton:
Princeton University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Nussbaum, 1978)

Nussbaum, M. (1978). *Aristotle’s “De Motu Animalium”*. Princeton:
Princeton University Press.


}

@string{ pup     = {Princeton University Press} }

@Book{nussbaum,
  author       = {Nussbaum, Martha},
  title        = {Aristotle's \mkbibquote{De Motu Animalium}},
  date         = 1978,
  publisher    = pup,
  location     = {Princeton},
  keywords     = {secondary},
  hyphenation  = {american},
  sorttitle    = {Aristotle's De Motu Animalium},
  indexsorttitle= {Aristotle's De Motu Animalium},
  annotation   = {A book entry. Note the sorttitle and
                  indexsorttitle fields and the markup of the quotes in
                  the database file},
}

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
  title: "Aristotle's \"De Motu Animalium\""
  type: book
---


```
