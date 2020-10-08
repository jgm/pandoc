```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Springer 1950)

Springer, Otto. 1950. “Mediaeval Pilgrim Routes from Scandinavia to
Rome.” *Mediaeval Studies* 12: 92–122.


Formatted with pandoc and apa.csl, 2013-10-23:

(Springer, 1950)

Springer, O. (1950). Mediaeval pilgrim routes from Scandinavia to Rome.
*Mediaeval Studies*, *12*, 92–122.


}

@Article{springer,
  author       = {Springer, Otto},
  title        = {Mediaeval Pilgrim Routes from {Scandinavia} to {Rome}},
  journaltitle = {Mediaeval Studies},
  date         = 1950,
  volume       = 12,
  pages        = {92-122},
  hyphenation  = {british},
  shorttitle   = {Mediaeval Pilgrim Routes},
  annotation   = {A plain article entry},
}

^D
---
nocite: "[@*]"
references:
- annote: A plain article entry
  author:
  - family: Springer
    given: Otto
  container-title: Mediaeval Studies
  id: springer
  issued: 1950
  language: en-GB
  page: 92-122
  title: Mediaeval pilgrim routes from Scandinavia to Rome
  title-short: Mediaeval pilgrim routes
  type: article-journal
  volume: 12
---


```
