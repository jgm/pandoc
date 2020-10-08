```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2015-03-08:

(Knuth 1986)

Knuth, Donald E. 1986. *Computers & Typesetting*. Vol. D. Reading,
Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2015-03-08:

(Knuth, 1986)

Knuth, D. E. (1986). *Computers & typesetting* (Vol. D). Reading, Mass.:
Addison-Wesley.


NOTES:

- biblio2yaml
	- Should letters following a colon, such as the "T" in "{{METAFONT}: {T}he Program}" be protected by default? -- I'm not sure ...

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@Book{knuth:ct:d,
  author       = {Knuth, Donald E.},
  title        = {{METAFONT}: {T}he Program},
  date         = 1986,
  maintitle    = {Computers \& Typesetting},
  volume       = {D},
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  hyphenation  = {american},
  sortyear     = {1986-3},
  sorttitle    = {Computers & Typesetting D},
  shorttitle   = {{METAFONT}},
  annotation   = {The fourth volume of a five-volume book. Note the
                  sorttitle and sortyear fields},
}

^D
---
nocite: "[@*]"
references:
- annote: The fourth volume of a five-volume book. Note the sorttitle
    and sortyear fields
  author:
  - family: Knuth
    given: Donald E.
  id: "knuth:ct:d"
  issued: 1986
  language: en-US
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: Computers & typesetting
  title-short: METAFONT
  type: book
  volume: D
  volume-title: "METAFONT: The program"
---


```
