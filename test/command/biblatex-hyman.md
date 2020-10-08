```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Hyman 1981)

Hyman, Arthur. 1981. “Aristotle’s Theory of the Intellect and Its
Interpretation by Averroes.” In *Studies in Aristotle*, edited by
Dominic J. O’Meara, 161–191. Studies in Philosophy and the History of
Philosophy 9. Washington, D.C.: The Catholic University of America
Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Hyman, 1981)

Hyman, A. (1981). Aristotle’s theory of the intellect and its
interpretation by Averroes. In D. J. O’Meara (Ed.), *Studies in
Aristotle* (pp. 161–191). Washington, D.C.: The Catholic University of
America Press.


}

@InCollection{hyman,
  author       = {Arthur Hyman},
  editor       = {O'Meara, Dominic J.},
  title        = {Aristotle's Theory of the Intellect and its Interpretation by
                  {Averroes}},
  date         = 1981,
  booktitle    = {Studies in {Aristotle}},
  series       = {Studies in Philosophy and the History of Philosophy},
  number       = 9,
  publisher    = {The Catholic University of America Press},
  location     = {Washington, D.C.},
  pages        = {161-191},
  keywords     = {secondary},
  hyphenation  = {american},
  indextitle   = {Aristotle's Theory of the Intellect},
  shorttitle   = {Aristotle's Theory of the Intellect},
  annotation   = {An incollection entry with a series and
                  number field},
}

^D
---
nocite: "[@*]"
references:
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
---


```
