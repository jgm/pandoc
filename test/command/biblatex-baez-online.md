```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Baez and Lauda 2004)

Baez, John C., and Aaron D. Lauda. 2004. “Higher-dimensional Algebra V:
2-groups” (version 3). October 27.


Formatted with pandoc and apa.csl, 2013-10-23:

(Baez & Lauda, 2004)

Baez, J. C., & Lauda, A. D. (2004, October 27). Higher-dimensional
algebra V: 2-groups.


NOTES:

- biblio2yaml:
	- eprinttype = {arxiv}, eprint = {math/0307200v3}, 
	  should be converted to a url: http://arxiv.org/abs/math/0307200v3
	  (prefix http://arxiv.org/abs/ seems to work for all arxiv material)

}

@Online{baez-online,
  author       = {Baez, John C. and Lauda, Aaron D.},
  title        = {Higher-Dimensional Algebra {V}: 2-Groups},
  date         = {2004-10-27},
  version      = 3,
  hyphenation  = {american},
  eprinttype   = {arxiv},
  eprint       = {math/0307200v3},
  annotation   = {An online reference from arXiv. Note the
                  eprint and eprinttype fields. Compare
                  baez\slash article which is the same item given as an
                  article entry with eprint information},
}

^D
---
nocite: "[@*]"
references:
- annote: An online reference from arXiv. Note the eprint and eprinttype
    fields. Compare baez/article which is the same item given as an
    article entry with eprint information
  author:
  - family: Baez
    given: John C.
  - family: Lauda
    given: Aaron D.
  id: baez-online
  issued: 2004-10-27
  language: en-US
  title: "Higher-dimensional algebra V: 2-groups"
  title-short: Higher-dimensional algebra V
  type: webpage
  url: "https://arxiv.org/abs/math/0307200v3"
  version: 3
---


```
