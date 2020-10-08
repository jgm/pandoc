```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2015-03-08:

(Knuth 1986)

Knuth, Donald E. 1986. *Computers & Typesetting*. Vol. E. Reading,
Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2015-03-08:

(Knuth, 1986)

Knuth, D. E. (1986). *Computers & typesetting* (Vol. E). Reading, Mass.:
Addison-Wesley.


NOTES:

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@Book{knuth:ct:e,
  author       = {Knuth, Donald E.},
  title        = {Computer Modern Typefaces},
  date         = 1986,
  maintitle    = {Computers \& Typesetting},
  volume       = {E},
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  hyphenation  = {american},
  sortyear     = {1986-4},
  sorttitle    = {Computers & Typesetting E},
  annotation   = {The fifth volume of a five-volume book. Note the
                  sorttitle and sortyear fields},
}

^D
---
nocite: "[@*]"
references:
- annote: The fifth volume of a five-volume book. Note the sorttitle and
    sortyear fields
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct:e"
  issued: 1986
  language: en-US
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  type: book
  volume: E
  volume-title: Computer modern typefaces
---


```
