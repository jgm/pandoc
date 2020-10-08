```
% pandoc -f biblatex -t markdown -s
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

- citeproc
	- term "vols." is missing

}

@Book{knuth:ct,
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
  annotation   = {A five-volume book cited as a whole. This is a book
                  entry, note the volumes field},
}

^D
---
nocite: "[@*]"
references:
- annote: A five-volume book cited as a whole. This is a book entry,
    note the volumes field
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct"
  issued: 1984/1986
  language: en-US
  number-of-volumes: 5
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  type: book
---


```
