```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Massa 2004)

Massa, Werner. 2004. *Crystal Structure Determination*. 2nd ed. Berlin:
Spinger.


Formatted with pandoc and apa.csl, 2013-10-23:

(Massa, 2004)

Massa, W. (2004). *Crystal structure determination* (2nd ed.). Berlin:
Spinger.


}

@Book{massa,
  author       = {Werner Massa},
  title        = {Crystal structure determination},
  date         = 2004,
  edition      = 2,
  publisher    = {Spinger},
  location     = {Berlin},
  hyphenation  = {british},
  annotation   = {A book entry with an edition field},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry with an edition field
  author:
  - family: Massa
    given: Werner
  edition: 2
  id: massa
  issued: 2004
  language: en-GB
  publisher: Spinger
  publisher-place: Berlin
  title: Crystal structure determination
  type: book
---


```
