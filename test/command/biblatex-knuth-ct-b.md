```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2015-03-08:

(Knuth 1986)

Knuth, Donald E. 1986. *Computers & Typesetting*. Vol. B. Reading,
Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2015-03-08:

(Knuth, 1986)

Knuth, D. E. (1986). *Computers & typesetting* (Vol. B). Reading, Mass.:
Addison-Wesley.


NOTES:

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@Book{knuth:ct:b,
  author       = {Knuth, Donald E.},
  title        = {\TeX: {T}he Program},
  date         = 1986,
  maintitle    = {Computers \& Typesetting},
  volume       = {B},
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  hyphenation  = {american},
  sortyear     = {1986-1},
  sorttitle    = {Computers & Typesetting B},
  indexsorttitle= {TeX: The Program},
  shorttitle   = {\TeX},
  annotation   = {The second volume of a five-volume book. Note the
                  sorttitle and sortyear fields. Also note the
                  indexsorttitle field},
}

^D
---
nocite: "[@*]"
references:
- annote: The second volume of a five-volume book. Note the sorttitle
    and sortyear fields. Also note the indexsorttitle field
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct:b"
  issued: 1986
  language: en-US
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  title-short: TeX
  type: book
  volume: B
  volume-title: "TeX: The program"
---


```
