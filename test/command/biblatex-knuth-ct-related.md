```
% pandoc -f biblatex -t markdown -s --markdown-headings=setext
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Knuth 1984–1986)

Knuth, Donald E. 1984–1986. *Computers & Typesetting*. 5. Reading,
Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2013-10-23:

(Knuth, 1984–1986)

Knuth, D. E. (1984–1986). *Computers & typesetting* (1-5). Reading,
Mass.: Addison-Wesley.


NOTES:

- biblio2yaml
	- related = {...}, relatedtype  = {multivolume}, -- no counterpart in CSL 

- citeproc
	- term "vols." missing

}

@Book{knuth:ct:related,
  author       = {Knuth, Donald E.},
  title        = {Computers \& Typesetting},
  date         = {1984/1986},
  volumes      = 5,
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  hyphenation  = {american},
  sortyear     = {1984-0},
  sorttitle    = {Computers & Typesetting},
  indexsorttitle= {Computers & Typesetting},
  related      = {knuth:ct:a,knuth:ct:b,knuth:ct:c,knuth:ct:d,knuth:ct:e},
  relatedtype  = {multivolume},
  annotation   = {A five-volume book cited as a whole and related to its
                  individual volumes. Note the related and
                  relatedtype fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A five-volume book cited as a whole and related to its
    individual volumes. Note the related and relatedtype fields
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct:related"
  issued: 1984/1986
  language: en-US
  number-of-volumes: 5
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  type: book
---


```
