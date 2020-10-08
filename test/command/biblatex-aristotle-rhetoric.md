```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Aristotle 1877)

Aristotle. 1877. *The Rhetoric of Aristotle with a Commentary by the
Late Edward Meredith Cope*. Edited by Edward Meredith Cope. 3. Cambridge
University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Aristotle, 1877)

Aristotle. (1877). *The rhetoric of Aristotle with a commentary by the
late Edward Meredith Cope*. (E. M. Cope, Ed.) (1-3). Cambridge
University Press.


NOTES:

- biblio2yaml
	- commentator has no counterpart in CSL

}

@string{ cup     = {Cambridge University Press} }

@Book{aristotle:rhetoric,
  author       = {Aristotle},
  title        = {The Rhetoric of {Aristotle} with a commentary by the late {Edward
                  Meredith Cope}},
  date         = 1877,
  editor       = {Cope, Edward Meredith},
  commentator  = {Cope, Edward Meredith},
  volumes      = 3,
  publisher    = cup,
  keywords     = {primary},
  hyphenation  = {british},
  sorttitle    = {Rhetoric of Aristotle},
  indextitle   = {Rhetoric of {Aristotle}, The},
  shorttitle   = {Rhetoric},
  annotation   = {A commented edition. Note the concatenation of the
                  editor and commentator fields as well as the
                  volumes, sorttitle, and indextitle
                  fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A commented edition. Note the concatenation of the editor and
    commentator fields as well as the volumes, sorttitle, and indextitle
    fields
  author:
  - family: Aristotle
  editor:
  - family: Cope
    given: Edward Meredith
  id: "aristotle:rhetoric"
  issued: 1877
  keyword: primary
  language: en-GB
  number-of-volumes: 3
  publisher: Cambridge University Press
  title: The rhetoric of Aristotle with a commentary by the late Edward
    Meredith Cope
  title-short: Rhetoric
  type: book
---


```
