```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Moore 1965)

Moore, Gordon E. 1965. “Cramming More Components onto Integrated
Circuits.” *Electronics* 38 (8): 114–117.


Formatted with pandoc and apa.csl, 2013-10-23:

(Moore, 1965)

Moore, G. E. (1965). Cramming more components onto integrated circuits.
*Electronics*, *38*(8), 114–117.


}

@Article{moore,
  author       = {Moore, Gordon E.},
  title        = {Cramming more components onto integrated circuits},
  journaltitle = {Electronics},
  year         = 1965,
  volume       = 38,
  number       = 8,
  pages        = {114-117},
  hyphenation  = {american},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Moore
    given: Gordon E.
  container-title: Electronics
  id: moore
  issue: 8
  issued: 1965
  language: en-US
  page: 114-117
  title: Cramming more components onto integrated circuits
  type: article-journal
  volume: 38
---


```
