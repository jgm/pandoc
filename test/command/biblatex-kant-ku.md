```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Kant 1968)

Kant, Immanuel. 1968. “Kritik der Urtheilskraft.” In *Kants Werke.
Akademie Textausgabe*, by Immanuel Kant, 5:165–485. Berlin: Walter de
Gruyter.


Formatted with pandoc and apa.csl, 2013-10-23:

(Kant, 1968)

Kant, I. (1968). Kritik der Urtheilskraft. In *Kants Werke. Akademie
Textausgabe* (Vol. 5, pp. 165–485). Berlin: Walter de Gruyter.


NOTES:

- citeproc
	- support for the not yet official "volume-title" is missing
	
- CSL style file
	- if author and container-author are identical, container-author should be suppressed (apparently csl style file issue; zotero shows same behaviour)

}

@InBook{kant:ku,
  title        = {Kritik der Urtheilskraft},
  date         = 1968,
  author       = {Kant, Immanuel},
  booktitle    = {Kritik der praktischen Vernunft. Kritik der Urtheilskraft},
  bookauthor   = {Kant, Immanuel},
  maintitle    = {Kants Werke. Akademie Textausgabe},
  volume       = 5,
  publisher    = {Walter de Gruyter},
  location     = {Berlin},
  pages        = {165-485},
  shorthand    = {KU},
  hyphenation  = {german},
  annotation   = {An edition of Kant's \emph{Collected Works}, volume five. This
                  is an inbook entry which explicitly refers to the
                  \emph{Critique of Judgment} only, not to the entire fifth
                  volume},
}

^D
---
nocite: "[@*]"
references:
- annote: An edition of Kant's *Collected Works*, volume five. This is
    an inbook entry which explicitly refers to the *Critique of
    Judgment* only, not to the entire fifth volume
  author:
  - family: Kant
    given: Immanuel
  container-author:
  - family: Kant
    given: Immanuel
  container-title: Kants Werke. Akademie Textausgabe
  id: "kant:ku"
  issued: 1968
  language: de-DE
  page: 165-485
  publisher: Walter de Gruyter
  publisher-place: Berlin
  title: Kritik der Urtheilskraft
  type: chapter
  volume: 5
  volume-title: Kritik der praktischen Vernunft. Kritik der
    Urtheilskraft
---


```
