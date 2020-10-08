```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Westfahl 2000a) (Westfahl 2000b)

Westfahl, Gary. 2000a. “The True Frontier: Confronting and Avoiding the
Realities of Space in American Science Fiction Films.” In *Space and
Beyond: The Frontier Theme in Science Fiction*, edited by Gary Westfahl,
55–65. Westport, Conn.; London: Greenwood.

———, ed. 2000b. *Space and Beyond: The Frontier Theme in Science
Fiction*. Westport, Conn.; London: Greenwood.


Formatted with pandoc and apa.csl, 2013-10-23:

(Westfahl, 2000) (Westfahl, 2000)

Westfahl, G. (2000). The true frontier: Confronting and avoiding the
realities of space in American science fiction films. In G. Westfahl
(Ed.), *Space and beyond: The frontier theme in science fiction* (pp.
55–65). Westport, Conn.; London: Greenwood.

Westfahl, G. (Ed.). (2000). *Space and beyond: The frontier theme in
science fiction*. Westport, Conn.; London: Greenwood.


}

@InCollection{westfahl:space,
  author       = {Westfahl, Gary},
  title        = {The True Frontier},
  subtitle     = {Confronting and Avoiding the Realities of Space in {American}
                  Science Fiction Films},
  pages        = {55-65},
  crossref     = {westfahl:frontier},
  hyphenation  = {american},
  indextitle   = {True Frontier, The},
  annotation   = {A cross-referenced article from a collection. This is
                  an incollection entry with a crossref
                  field. Note the subtitle and indextitle
                  fields},
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
- annote: A cross-referenced article from a collection. This is an
    incollection entry with a crossref field. Note the subtitle and
    indextitle fields
  author:
  - family: Westfahl
    given: Gary
  container-title: "Space and beyond: The frontier theme in science
    fiction"
  editor:
  - family: Westfahl
    given: Gary
  id: "westfahl:space"
  issued: 2000
  language: en-US
  page: 55-65
  publisher: Greenwood
  publisher-place: Westport, Conn.; London
  title: "The true frontier: Confronting and avoiding the realities of
    space in American science fiction films"
  title-short: The true frontier
  type: chapter
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
