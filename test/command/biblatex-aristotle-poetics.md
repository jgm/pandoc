```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Aristotle 1968)

Aristotle. 1968. *Poetics*. Edited by D. W. Lucas. Clarendon Aristotle.
Oxford: Clarendon Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Aristotle, 1968)

Aristotle. (1968). *Poetics*. (D. W. Lucas, Ed.). Oxford: Clarendon
Press.


}

@Book{aristotle:poetics,
  author       = {Aristotle},
  title        = {Poetics},
  date         = 1968,
  editor       = {Lucas, D. W.},
  series       = {Clarendon {Aristotle}},
  publisher    = {Clarendon Press},
  location     = {Oxford},
  keywords     = {primary},
  hyphenation  = {british},
  shorttitle   = {Poetics},
  annotation   = {A book entry with an author and an
                  editor as well as a series field},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry with an author and an editor as well as a series
    field
  author:
  - family: Aristotle
  collection-title: Clarendon Aristotle
  editor:
  - family: Lucas
    given: D. W.
  id: "aristotle:poetics"
  issued: 1968
  keyword: primary
  language: en-GB
  publisher: Clarendon Press
  publisher-place: Oxford
  title: Poetics
  title-short: Poetics
  type: book
---


```
