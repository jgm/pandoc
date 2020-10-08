```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Almendro et al. 1998)

Almendro, José L., Jacinto Martín, Alberto Sánchez, and Fernando Nozal.
1998. “Elektromagnetisches Signalhorn.”


Formatted with pandoc and apa.csl, 2013-10-23:

(Almendro, Martín, Sánchez, & Nozal, 1998)

Almendro, J. L., Martín, J., Sánchez, A., & Nozal, F. (1998).
Elektromagnetisches Signalhorn.


NOTES:

- CSL styles’ handling of patent items needs to be improved

}

@Patent{almendro,
  author       = {Almendro, Jos{\'e} L. and Mart{\'i}n, Jacinto and S{\'a}nchez,
                  Alberto and Nozal, Fernando},
  title        = {Elektromagnetisches Signalhorn},
  number       = {EU-29702195U},
  date         = 1998,
  location     = {countryfr and countryuk and countryde},
  hyphenation  = {german},
  annotation   = {This is a patent entry with a location
                  field. The number is given in the number field. Note
                  the format of the location field in the database
                  file. Compare laufenberg, sorace, and
                  kowalik},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a patent entry with a location field. The number is
    given in the number field. Note the format of the location field in
    the database file. Compare laufenberg, sorace, and kowalik
  author:
  - family: Almendro
    given: José L.
  - family: Martín
    given: Jacinto
  - family: Sánchez
    given: Alberto
  - family: Nozal
    given: Fernando
  id: almendro
  issued: 1998
  jurisdiction: France; United Kingdom; Germany
  language: de-DE
  number: EU-29702195U
  title: Elektromagnetisches Signalhorn
  type: patent
---


```
