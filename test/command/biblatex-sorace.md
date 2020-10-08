```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Sorace, Reinhardt, and Vaughn 1997)

Sorace, Ronald E., Victor S. Reinhardt, and Steven A. Vaughn. 1997.
“High-speed Digital-to-RF Converter.” U.S. patent.


Formatted with pandoc and apa.csl, 2013-10-23:

(Sorace, Reinhardt, & Vaughn, 1997)

Sorace, R. E., Reinhardt, V. S., & Vaughn, S. A. (1997, September 16).
High-speed digital-to-RF converter. U.S. patent.


}

@Patent{sorace,
  author       = {Sorace, Ronald E. and Reinhardt, Victor S. and Vaughn, Steven
                  A.},
  title        = {High-Speed Digital-to-{RF} Converter},
  number       = 5668842,
  date         = {1997-09-16},
  holder       = {{Hughes Aircraft Company}},
  type         = {patentus},
  hyphenation  = {american},
  annotation   = {This is a patent entry with a holder field.
                  Note the format of the type and date fields
                  in the database file. Compare almendro,
                  laufenberg, and kowalik},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a patent entry with a holder field. Note the format of
    the type and date fields in the database file. Compare almendro,
    laufenberg, and kowalik
  author:
  - family: Sorace
    given: Ronald E.
  - family: Reinhardt
    given: Victor S.
  - family: Vaughn
    given: Steven A.
  genre: U.S. patent
  id: sorace
  issued: 1997-09-16
  language: en-US
  number: 5668842
  title: High-speed digital-to-RF converter
  type: patent
---


```
