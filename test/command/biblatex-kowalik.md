```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Kowalik and Isard 1995)

Kowalik, F., and M. Isard. 1995. “Estimateur d’un défaut de
fonctionnement d’un modulateur en quadrature et étage de modulation
l’utilisant.” French patent request.


Formatted with pandoc and apa.csl, 2013-10-23:

(Kowalik & Isard, 1995)

Kowalik, F., & Isard, M. (1995, January 11). Estimateur d’un défaut de
fonctionnement d’un modulateur en quadrature et étage de modulation
l’utilisant. French patent request.


}

@Patent{kowalik,
  author       = {Kowalik, F. and Isard, M.},
  title        = {Estimateur d'un d{\'e}faut de fonctionnement d'un modulateur
                  en quadrature et {\'e}tage de modulation l'utilisant},
  number       = 9500261,
  date         = {1995-01-11},
  type         = {patreqfr},
  hyphenation  = {french},
  indextitle   = {Estimateur d'un d{\'e}faut de fonctionnement},
  annotation   = {This is a patent entry for a French patent request
                  with a full date. The number is given in the number
                  field. Note the format of the type and date
                  fields in the database file. Compare almendro,
                  laufenberg, and sorace},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a patent entry for a French patent request with a full
    date. The number is given in the number field. Note the format of
    the type and date fields in the database file. Compare almendro,
    laufenberg, and sorace
  author:
  - family: Kowalik
    given: F.
  - family: Isard
    given: M.
  genre: French patent request
  id: kowalik
  issued: 1995-01-11
  language: fr-FR
  number: 9500261
  title: Estimateur d'un défaut de fonctionnement d'un modulateur en
    quadrature et étage de modulation l'utilisant
  type: patent
---


```
