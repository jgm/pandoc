```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Salam 1968)

Salam, Abdus. 1968. “Weak and Electromagnetic Interactions.” In
*Elementary Particle Theory: Relativistic Groups and Analyticity.
Proceedings of the Eighth Nobel Symposium*, edited by Nils Svartholm,
367–377. Stockholm: Almquist & Wiksell.


Formatted with pandoc and apa.csl, 2013-10-23:

(Salam, 1968)

Salam, A. (1968). Weak and electromagnetic interactions. In N. Svartholm
(Ed.), *Elementary particle theory: Relativistic groups and analyticity.
Proceedings of the eighth Nobel symposium* (pp. 367–377). Stockholm:
Almquist & Wiksell.


}

@InProceedings{salam,
  author       = {Salam, Abdus},
  editor       = {Svartholm, Nils},
  title        = {Weak and Electromagnetic Interactions},
  date         = 1968,
  booktitle    = {Elementary particle theory},
  booksubtitle = {Relativistic groups and analyticity},
  booktitleaddon= {Proceedings of the Eighth {Nobel} Symposium},
  eventdate    = {1968-05-19/1968-05-25},
  venue        = {Aspen{\"a}sgarden, Lerum},
  publisher    = {Almquist \& Wiksell},
  location     = {Stockholm},
  pages        = {367-377},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Salam
    given: Abdus
  container-title: "Elementary particle theory: Relativistic groups and
    analyticity. Proceedings of the eighth Nobel symposium"
  editor:
  - family: Svartholm
    given: Nils
  event-date: 1968-05-19/1968-05-25
  event-place: Aspenäsgarden, Lerum
  id: salam
  issued: 1968
  page: 367-377
  publisher: Almquist & Wiksell
  publisher-place: Stockholm
  title: Weak and electromagnetic interactions
  type: paper-conference
---


```
