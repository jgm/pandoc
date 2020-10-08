```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Knuth 1984)

Knuth, Donald E. 1984. *Computers & Typesetting*. Vol. A. Reading,
Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2013-10-23:

(Knuth, 1984)

Knuth, D. E. (1984). *Computers & typesetting* (Vol. A). Reading, Mass.:
Addison-Wesley.


NOTES:

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@Book{knuth:ct:a,
  author       = {Knuth, Donald E.},
  title        = {The {\TeX} book},
  date         = 1984,
  maintitle    = {Computers \& Typesetting},
  volume       = {A},
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  hyphenation  = {american},
  sortyear     = {1984-1},
  sorttitle    = {Computers & Typesetting A},
  indexsorttitle= {The TeXbook},
  indextitle   = {\protect\TeX book, The},
  shorttitle   = {\TeX book},
  annotation   = {The first volume of a five-volume book. Note the
                  sorttitle and sortyear fields. We want this
                  volume to be listed after the entry referring to the entire
                  five-volume set. Also note the indextitle and
                  indexsorttitle fields. Indexing packages that don't
                  generate robust index entries require some control sequences
                  to be protected from expansion},
}

^D
---
nocite: "[@*]"
references:
- annote: The first volume of a five-volume book. Note the sorttitle and
    sortyear fields. We want this volume to be listed after the entry
    referring to the entire five-volume set. Also note the indextitle
    and indexsorttitle fields. Indexing packages that don't generate
    robust index entries require some control sequences to be protected
    from expansion
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct:a"
  issued: 1984
  language: en-US
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  type: book
  volume: A
  volume-title: The TeX book
---


```
