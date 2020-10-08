```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Sarfraz and Razzak 2002)

Sarfraz, M., and M. F. A. Razzak. 2002. “Technical Section: An Algorithm
for Automatic Capturing of the Font Outlines.” *Computers and Graphics*
26 (5): 795–804.


Formatted with pandoc and apa.csl, 2013-10-23:

(Sarfraz & Razzak, 2002)

Sarfraz, M., & Razzak, M. F. A. (2002). Technical section: An algorithm
for automatic capturing of the font outlines. *Computers and Graphics*,
*26*(5), 795–804.


}

@Article{sarfraz,
  author       = {M. Sarfraz and M. F. A. Razzak},
  title        = {Technical section: {An} algorithm for automatic capturing of
                  the font outlines},
  year         = 2002,
  volume       = 26,
  number       = 5,
  pages        = {795-804},
  issn         = {0097-8493},
  journal      = {Computers and Graphics},
  annotation   = {An article entry with an issn field},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry with an issn field
  author:
  - family: Sarfraz
    given: M.
  - family: Razzak
    given: M. F. A.
  container-title: Computers and Graphics
  id: sarfraz
  issn: 0097-8493
  issue: 5
  issued: 2002
  page: 795-804
  title: "Technical section: An algorithm for automatic capturing of the
    font outlines"
  title-short: Technical section
  type: article-journal
  volume: 26
---


```
