```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Moraux 1979)

Moraux, Paul. 1979. “Le *De Anima* dans la tradition grècque: Quelques
aspects de l’interpretation du traité, de Theophraste à Themistius.” In
*Aristotle on Mind and the Senses. Proceedings of the Seventh Symposium
Aristotelicum*, edited by G. E. R. Lloyd and G. E. L. Owen, 281–324.
Cambridge: Cambridge University Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Moraux, 1979)

Moraux, P. (1979). Le *De Anima* dans la tradition grècque: Quelques
aspects de l’interpretation du traité, de Theophraste à Themistius. In
G. E. R. Lloyd & G. E. L. Owen (eds.), *Aristotle on Mind and the
Senses. Proceedings of the Seventh Symposium Aristotelicum* (pp.
281–324). Cambridge: Cambridge University Press.


NOTES:

- Since case (conversion) can only be specified per entry, not per field, for apa.csl the case of container-title would have to be adjusted manually.

}

@string{ cup     = {Cambridge University Press} }

@InProceedings{moraux,
  author       = {Moraux, Paul},
  editor       = {Lloyd, G. E. R. and Owen, G. E. L.},
  title        = {Le \emph{De Anima} dans la tradition gr{\`e}cque},
  date         = 1979,
  booktitle    = {Aristotle on Mind and the Senses},
  subtitle     = {Quelques aspects de l'interpretation du trait{\'e}, de
                  Theophraste {\`a} Themistius},
  booktitleaddon= {Proceedings of the Seventh Symposium Aristotelicum},
  eventdate    = 1975,
  publisher    = cup,
  location     = {Cambridge},
  pages        = {281-324},
  keywords     = {secondary},
  hyphenation  = {french},
  indexsorttitle= {De Anima dans la tradition grecque},
  indextitle   = {\emph{De Anima} dans la tradition gr{\`e}cque, Le},
  shorttitle   = {\emph{De Anima} dans la tradition gr{\`e}cque},
  annotation   = {This is a typical inproceedings entry. Note the
                  booksubtitle, shorttitle,
                  indextitle, and indexsorttitle fields. Also
                  note the eventdate field.},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a typical inproceedings entry. Note the booksubtitle,
    shorttitle, indextitle, and indexsorttitle fields. Also note the
    eventdate field.
  author:
  - family: Moraux
    given: Paul
  container-title: Aristotle on Mind and the Senses. Proceedings of the
    Seventh Symposium Aristotelicum
  editor:
  - family: Lloyd
    given: G. E. R.
  - family: Owen
    given: G. E. L.
  event-date: 1975
  id: moraux
  issued: 1979
  keyword: secondary
  language: fr-FR
  page: 281-324
  publisher: Cambridge University Press
  publisher-place: Cambridge
  title: "Le *De Anima* dans la tradition grècque: Quelques aspects de
    l'interpretation du traité, de Theophraste à Themistius"
  title-short: "*De Anima* dans la tradition grècque"
  type: paper-conference
---


```
