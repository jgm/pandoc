```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Bertram and Wentworth 1996)

Bertram, Aaron, and Richard Wentworth. 1996. “Gromov Invariants for
Holomorphic Maps on Riemann Surfaces.” *J. Amer. Math. Soc.* 9 (2):
529–571.


Formatted with pandoc and apa.csl, 2013-10-23:

(Bertram & Wentworth, 1996)

Bertram, A., & Wentworth, R. (1996). Gromov invariants for holomorphic
maps on Riemann surfaces. *J. Amer. Math. Soc.*, *9*(2), 529–571.


}

@string{ jams    = {J.~Amer. Math. Soc.} }

@Article{bertram,
  author       = {Bertram, Aaron and Wentworth, Richard},
  title        = {Gromov invariants for holomorphic maps on {Riemann} surfaces},
  journaltitle = jams,
  date         = 1996,
  volume       = 9,
  number       = 2,
  pages        = {529-571},
  hyphenation  = {american},
  shorttitle   = {Gromov invariants},
  annotation   = {An article entry with a volume and a
                  number field},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry with a volume and a number field
  author:
  - family: Bertram
    given: Aaron
  - family: Wentworth
    given: Richard
  container-title: J. Amer. Math. Soc.
  id: bertram
  issue: 2
  issued: 1996
  language: en-US
  page: 529-571
  title: Gromov invariants for holomorphic maps on Riemann surfaces
  title-short: Gromov invariants
  type: article-journal
  volume: 9
---


```
