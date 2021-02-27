```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Baez and Lauda 2004)

Baez, John C., and Aaron D. Lauda. 2004. “Higher-dimensional Algebra V:
2-groups” (version 3). *Theory and Applications of Categories* 12:
423–491.


Formatted with pandoc and apa.csl, 2013-10-23:

(Baez & Lauda, 2004)

Baez, J. C., & Lauda, A. D. (2004). Higher-dimensional algebra V:
2-groups. *Theory and Applications of Categories*, *12*, 423–491.


NOTES:

- biblio2yaml
	- eprint: see baez-online

}

@Article{baez-article,
  author       = {Baez, John C. and Lauda, Aaron D.},
  title        = {Higher-Dimensional Algebra {V}: 2-Groups},
  journaltitle = {Theory and Applications of Categories},
  date         = 2004,
  volume       = 12,
  pages        = {423-491},
  version      = 3,
  eprint       = {math/0307200v3},
  eprinttype   = {arxiv},
  hyphenation  = {american},
  annotation   = {An article with eprint and
                  eprinttype fields. Note that the arXiv reference is
                  transformed into a clickable link if hyperref support
                  has been enabled.  Compare baez\slash online, which
                  is the same item given as an online entry},
}

^D
---
nocite: "[@*]"
references:
- annote: An article with eprint and eprinttype fields. Note that the
    arXiv reference is transformed into a clickable link if hyperref
    support has been enabled. Compare baez/online, which is the same
    item given as an online entry
  author:
  - family: Baez
    given: John C.
  - family: Lauda
    given: Aaron D.
  container-title: Theory and Applications of Categories
  id: baez-article
  issued: 2004
  language: en-US
  page: 423-491
  title: "Higher-dimensional algebra V: 2-groups"
  title-short: Higher-dimensional algebra V
  type: article-journal
  url: "https://arxiv.org/abs/math/0307200v3"
  version: 3
  volume: 12
---


```
