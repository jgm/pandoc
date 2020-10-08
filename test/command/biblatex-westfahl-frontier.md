```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Westfahl 2000)

Westfahl, Gary, ed. 2000. *Space and Beyond: The Frontier Theme in
Science Fiction*. Westport, Conn.; London: Greenwood.


Formatted with pandoc and apa.csl, 2013-10-23:

(Westfahl, 2000)

Westfahl, G. (Ed.). (2000). *Space and beyond: The frontier theme in
science fiction*. Westport, Conn.; London: Greenwood.


}

@Collection{westfahl:frontier,
  editor       = {Westfahl, Gary},
  title        = {Space and Beyond},
  date         = 2000,
  subtitle     = {The Frontier Theme in Science Fiction},
  publisher    = {Greenwood},
  location     = {Westport, Conn. and London},
  hyphenation  = {american},
  booktitle    = {Space and Beyond},
  booksubtitle = {The Frontier Theme in Science Fiction},
  annotation   = {This is a collection entry. Note the format of the
                  location field as well as the subtitle and
                  booksubtitle fields},
}

^D
---
nocite: "[@*]"
references:
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
