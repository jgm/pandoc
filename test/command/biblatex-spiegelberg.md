```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Spiegelberg 1969)

Spiegelberg, Herbert. 1969. ““Intention” und “Intentionalität” in der
Scholastik, bei Brentano und Husserl.” *Studia Philosophica* 29:
189–216.


Formatted with pandoc and apa.csl, 2013-10-23:

(Spiegelberg, 1969)

Spiegelberg, H. (1969). “Intention” und “Intentionalität” in der
Scholastik, bei Brentano und Husserl. *Studia Philosophica*, *29*,
189–216.


NOTES:

- citeproc
	- flipflopping of quotes incorrect

}

@Article{spiegelberg,
  author       = {Spiegelberg, Herbert},
  title        = {\mkbibquote{Intention} und \mkbibquote{Intentionalit{\"a}t} in
                  der Scholastik, bei Brentano und Husserl},
  journaltitle = {Studia Philosophica},
  date         = 1969,
  volume       = 29,
  pages        = {189-216},
  hyphenation  = {german},
  sorttitle    = {Intention und Intentionalitat in der Scholastik, bei Brentano
                  und Husserl},
  indexsorttitle= {Intention und Intentionalitat in der Scholastik, bei Brentano
                  und Husserl},
  shorttitle   = {Intention und Intentionalit{\"a}t},
  annotation   = {An article entry. Note the sorttitle and
                  indexsorttitle fields and the markup of the quotes in
                  the database file},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry. Note the sorttitle and indexsorttitle fields
    and the markup of the quotes in the database file
  author:
  - family: Spiegelberg
    given: Herbert
  container-title: Studia Philosophica
  id: spiegelberg
  issued: 1969
  language: de-DE
  page: 189-216
  title: "\"Intention\" und \"Intentionalität\" in der Scholastik, bei
    Brentano und Husserl"
  title-short: Intention und Intentionalität
  type: article-journal
  volume: 29
---


```
