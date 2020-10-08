```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2015-03-08:

(Knuth 1986)

Knuth, Donald E. 1986. *Computers & Typesetting*. Vol. C. Reading,
Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2015-03-08:

(Knuth, 1986)

Knuth, D. E. (1986). *Computers & typesetting* (Vol. C). Reading, Mass.:
Addison-Wesley.


NOTES:

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@Book{knuth:ct:c,
  author       = {Knuth, Donald E.},
  title        = {The {METAFONTbook}},
  date         = 1986,
  maintitle    = {Computers \& Typesetting},
  volume       = {C},
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  hyphenation  = {american},
  sortyear     = {1986-2},
  sorttitle    = {Computers & Typesetting C},
  indextitle   = {METAFONTbook, The},
  shorttitle   = {{METAFONTbook}},
  annotation   = {The third volume of a five-volume book. Note the
                  sorttitle and sortyear fields as well as the
                  indextitle field},
}

^D
---
nocite: "[@*]"
references:
- annote: The third volume of a five-volume book. Note the sorttitle and
    sortyear fields as well as the indextitle field
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct:c"
  issued: 1986
  language: en-US
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  type: book
  volume: C
  volume-title: The METAFONTbook
---


```
