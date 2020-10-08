```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Aristotle 1907)

Aristotle. 1907. *De Anima*. Edited by Robert Drew Hicks. Cambridge:
Cambridge University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Aristotle, 1907)

Aristotle. (1907). *De anima*. (R. D. Hicks, Ed.). Cambridge: Cambridge
University Press.


}

@string{ cup     = {Cambridge University Press} }

@Book{aristotle:anima,
  author       = {Aristotle},
  title        = {De Anima},
  date         = 1907,
  editor       = {Hicks, Robert Drew},
  publisher    = cup,
  location     = {Cambridge},
  keywords     = {primary},
  hyphenation  = {british},
  annotation   = {A book entry with an author and an
                  editor},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry with an author and an editor
  author:
  - family: Aristotle
  editor:
  - family: Hicks
    given: Robert Drew
  id: "aristotle:anima"
  issued: 1907
  keyword: primary
  language: en-GB
  publisher: Cambridge University Press
  publisher-place: Cambridge
  title: De anima
  type: book
---


```
