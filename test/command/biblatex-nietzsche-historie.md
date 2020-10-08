```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Nietzsche 1988)

Nietzsche, Friedrich. 1988. “Unzeitgemässe Betrachtungen. Zweites Stück:
Vom Nutzen und Nachtheil der Historie für das Leben.” In *Sämtliche
Werke: Kritische Studienausgabe*, by Friedrich Nietzsche, edited by
Giorgio Colli and Mazzino Montinari, 1:243–334. München; Berlin; New
York: Deutscher Taschenbuch-Verlag; Walter de Gruyter.


Formatted with pandoc and apa.csl, 2013-10-23:

(Nietzsche, 1988)

Nietzsche, F. (1988). Unzeitgemässe Betrachtungen. Zweites Stück: Vom
Nutzen und Nachtheil der Historie für das Leben. In G. Colli & M.
Montinari (eds.), *Sämtliche Werke: Kritische Studienausgabe* (Vol. 1,
pp. 243–334). München; Berlin; New York: Deutscher Taschenbuch-Verlag;
Walter de Gruyter.


}

@string{ dtv     = {Deutscher Taschenbuch-Verlag} }

@InBook{nietzsche:historie,
  title        = {Unzeitgem{\"a}sse Betrachtungen. Zweites St{\"u}ck},
  date         = 1988,
  author       = {Nietzsche, Friedrich},
  booktitle    = {Die Geburt der Trag{\"o}die. Unzeitgem{\"a}{\ss}e
                  Betrachtungen I--IV. Nachgelassene Schriften 1870--1973},
  bookauthor   = {Nietzsche, Friedrich},
  editor       = {Colli, Giorgio and Montinari, Mazzino},
  subtitle     = {Vom Nutzen und Nachtheil der Historie f{\"u}r das Leben},
  maintitle    = {S{\"a}mtliche Werke},
  mainsubtitle = {Kritische Studienausgabe},
  volume       = 1,
  publisher    = dtv # { and Walter de Gruyter},
  location     = {M{\"u}nchen and Berlin and New York},
  pages        = {243-334},
  hyphenation  = {german},
  sortyear     = {1988-2},
  sorttitle    = {Werke-01-243},
  indexsorttitle= {Vom Nutzen und Nachtheil der Historie fur das Leben},
  indextitle   = {Vom Nutzen und Nachtheil der Historie f{\"u}r das Leben},
  shorttitle   = {Vom Nutzen und Nachtheil der Historie},
  annotation   = {A single essay from the critical edition of Nietzsche's works.
                  This inbook entry explicitly refers to an essay found
                  in the first volume. Note the title,
                  booktitle, and maintitle fields. Also note
                  the sorttitle and sortyear fields. We want
                  this entry to be listed after the entry referring to the
                  entire first volume},
}

^D
---
nocite: "[@*]"
references:
- annote: A single essay from the critical edition of Nietzsche's works.
    This inbook entry explicitly refers to an essay found in the first
    volume. Note the title, booktitle, and maintitle fields. Also note
    the sorttitle and sortyear fields. We want this entry to be listed
    after the entry referring to the entire first volume
  author:
  - family: Nietzsche
    given: Friedrich
  container-author:
  - family: Nietzsche
    given: Friedrich
  container-title: "Sämtliche Werke: Kritische Studienausgabe"
  editor:
  - family: Colli
    given: Giorgio
  - family: Montinari
    given: Mazzino
  id: "nietzsche:historie"
  issued: 1988
  language: de-DE
  page: 243-334
  publisher: Deutscher Taschenbuch-Verlag; Walter de Gruyter
  publisher-place: München; Berlin; New York
  title: "Unzeitgemässe Betrachtungen. Zweites Stück: Vom Nutzen und
    Nachtheil der Historie für das Leben"
  title-short: Vom Nutzen und Nachtheil der Historie
  type: chapter
  volume: 1
  volume-title: Die Geburt der Tragödie. Unzeitgemäße Betrachtungen
    I--IV. Nachgelassene Schriften 1870--1973
---


```
