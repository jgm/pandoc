```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Gonzalez 2001)

Gonzalez, Ray. 2001. *The Ghost of John Wayne and Other Stories*.
Tucson: The University of Arizona Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Gonzalez, 2001)

Gonzalez, R. (2001). *The ghost of John Wayne and other stories*.
Tucson: The University of Arizona Press.

}

@Book{gonzalez,
  author       = {Gonzalez, Ray},
  title        = {The Ghost of {John Wayne} and Other Stories},
  date         = 2001,
  publisher    = {The University of Arizona Press},
  location     = {Tucson},
  isbn         = {0-816-52066-6},
  hyphenation  = {american},
  sorttitle    = {Ghost of John Wayne and Other Stories},
  indextitle   = {Ghost of {John Wayne} and Other Stories, The},
  shorttitle   = {Ghost of {John Wayne}},
  annotation   = {A collection of short stories. This is a book entry.
                  Note the sorttitle and indextitle fields in
                  the database file. There's also an isbn field},
}

^D
---
nocite: "[@*]"
references:
- annote: A collection of short stories. This is a book entry. Note the
    sorttitle and indextitle fields in the database file. There's also
    an isbn field
  author:
  - family: Gonzalez
    given: Ray
  id: gonzalez
  isbn: 0-816-52066-6
  issued: 2001
  language: en-US
  publisher: The University of Arizona Press
  publisher-place: Tucson
  title: The ghost of John Wayne and other stories
  title-short: Ghost of John Wayne
  type: book
---


```
