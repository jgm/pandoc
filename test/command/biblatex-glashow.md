```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Glashow 1961)

Glashow, Sheldon. 1961. “Partial Symmetries of Weak Interactions.”
*Nucl. Phys.* 22: 579–588.


Formatted with pandoc and apa.csl, 2013-10-23:

(Glashow, 1961)

Glashow, S. (1961). Partial symmetries of weak interactions.
*Nucl. Phys.*, *22*, 579–588.


}

@Article{glashow,
  author       = {Glashow, Sheldon},
  title        = {Partial Symmetries of Weak Interactions},
  journaltitle = {Nucl.~Phys.},
  date         = 1961,
  volume       = 22,
  pages        = {579-588},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Glashow
    given: Sheldon
  container-title: Nucl. Phys.
  id: glashow
  issued: 1961
  page: 579-588
  title: Partial symmetries of weak interactions
  type: article-journal
  volume: 22
---


```
