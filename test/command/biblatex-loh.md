```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Loh 1992)

Loh, Nin C. 1992. “High-resolution Micromachined Interferometric
Accelerometer.” Master’s thesis, Cambridge, Mass.: Massachusetts
Institute of Technology.


Formatted with pandoc and apa.csl, 2013-10-23:

(Loh, 1992)

Loh, N. C. (1992). *High-resolution micromachined interferometric
accelerometer* (Master’s thesis). Massachusetts Institute of Technology,
Cambridge, Mass.


NOTES:

- biblio2yaml
	- At some point, actual localization of "localization keys" will have to be implemented

}

@Thesis{loh,
  author       = {Loh, Nin C.},
  title        = {High-Resolution Micromachined Interferometric Accelerometer},
  type         = {mathesis},
  institution  = {Massachusetts Institute of Technology},
  date         = 1992,
  location     = {Cambridge, Mass.},
  hyphenation  = {american},
  annotation   = {This is a typical thesis entry for an MA thesis. Note
                  the type field in the database file which uses a
                  localization key},
}
^D
---
nocite: "[@*]"
references:
- annote: This is a typical thesis entry for an MA thesis. Note the type
    field in the database file which uses a localization key
  author:
  - family: Loh
    given: Nin C.
  genre: Master's thesis
  id: loh
  issued: 1992
  language: en-US
  publisher: Massachusetts Institute of Technology
  publisher-place: Cambridge, Mass.
  title: High-resolution micromachined interferometric accelerometer
  type: thesis
---


```
