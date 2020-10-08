```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Weinberg 1967)

Weinberg, Steven. 1967. “A Model of Leptons.” *Phys. Rev. Lett.* 19:
1264–1266.


Formatted with pandoc and apa.csl, 2013-10-23:

(Weinberg, 1967)

Weinberg, S. (1967). A model of leptons. *Phys. Rev. Lett.*, *19*,
1264–1266.


}

@Article{weinberg,
  author       = {Weinberg, Steven},
  title        = {A Model of Leptons},
  journaltitle = {Phys.~Rev.~Lett.},
  date         = 1967,
  volume       = 19,
  pages        = {1264-1266},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Weinberg
    given: Steven
  container-title: Phys. Rev. Lett.
  id: weinberg
  issued: 1967
  page: 1264-1266
  title: A model of leptons
  type: article-journal
  volume: 19
---


```
