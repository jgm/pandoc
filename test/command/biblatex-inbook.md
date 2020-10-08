```
% pandoc -f biblatex -t markdown -s
@comment{
adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

TODO / citeproc: in biblatex "inbook" entries, citeproc should suppress bookauthor = CSL container-author if identical with author. -- See annotation in kant:kpv.
}

@string{dtv = {Deutscher Taschenbuch-Verlag}}

@inbook{kant:kpv,
	Annotation = {An edition of Kant's \emph{Collected Works}, volume five. This is an inbook entry which explicitly refers to the \emph{Critique of Practical Reason} only, not to the entire fifth volume. Note the author and bookauthor fields in the database file. By default, the bookauthor is omitted if the values of the author and bookauthor fields are identical},
	Author = {Kant, Immanuel},
	Bookauthor = {Kant, Immanuel},
	Booktitle = {Kritik der praktischen Vernunft. Kritik der Urtheilskraft},
	Date = 1968,
	Hyphenation = {german},
	Location = {Berlin},
	Maintitle = {Kants Werke. Akademie Textausgabe},
	Pages = {1-163},
	Publisher = {Walter de Gruyter},
	Shorthand = {KpV},
	Shorttitle = {Kritik der praktischen Vernunft},
	Title = {Kritik der praktischen Vernunft},
	Volume = 5}

@inbook{kant:ku,
	Annotation = {An edition of Kant's \emph{Collected Works}, volume five. This is an inbook entry which explicitly refers to the \emph{Critique of Judgment} only, not to the entire fifth volume},
	Author = {Kant, Immanuel},
	Bookauthor = {Kant, Immanuel},
	Booktitle = {Kritik der praktischen Vernunft. Kritik der Urtheilskraft},
	Date = 1968,
	Hyphenation = {german},
	Location = {Berlin},
	Maintitle = {Kants Werke. Akademie Textausgabe},
	Pages = {165-485},
	Publisher = {Walter de Gruyter},
	Shorthand = {KU},
	Title = {Kritik der Urtheilskraft},
	Volume = 5}

@inbook{nietzsche:historie,
	Annotation = {A single essay from the critical edition of Nietzsche's works. This inbook entry explicitly refers to an essay found in the first volume. Note the title, booktitle, and maintitle fields. Also note the sorttitle and sortyear fields. We want this entry to be listed after the entry referring to the entire first volume},
	Author = {Nietzsche, Friedrich},
	Bookauthor = {Nietzsche, Friedrich},
	Booktitle = {Die Geburt der Tragödie. Unzeitgemäße Betrachtungen I--IV. Nachgelassene Schriften 1870--1973},
	Date = 1988,
	Editor = {Colli, Giorgio and Montinari, Mazzino},
	Hyphenation = {german},
	Indexsorttitle = {Vom Nutzen und Nachtheil der Historie fur das Leben},
	Indextitle = {Vom Nutzen und Nachtheil der Historie für das Leben},
	Location = {München and Berlin and New York},
	Mainsubtitle = {Kritische Studienausgabe},
	Maintitle = {Sämtliche Werke},
	Pages = {243-334},
	Publisher = dtv # { and Walter de Gruyter},
	Shorttitle = {Vom Nutzen und Nachtheil der Historie},
	Sorttitle = {Werke-01-243},
	Sortyear = {1988-2},
	Subtitle = {Vom Nutzen und Nachtheil der Historie für das Leben},
	Title = {Unzeitgemässe Betrachtungen. Zweites Stück},
	Volume = 1}

^D
---
nocite: "[@*]"
references:
- annote: An edition of Kant's *Collected Works*, volume five. This is
    an inbook entry which explicitly refers to the *Critique of
    Practical Reason* only, not to the entire fifth volume. Note the
    author and bookauthor fields in the database file. By default, the
    bookauthor is omitted if the values of the author and bookauthor
    fields are identical
  author:
  - family: Kant
    given: Immanuel
  container-author:
  - family: Kant
    given: Immanuel
  container-title: Kants Werke. Akademie Textausgabe
  id: "kant:kpv"
  issued: 1968
  language: de-DE
  page: 1-163
  publisher: Walter de Gruyter
  publisher-place: Berlin
  title: Kritik der praktischen Vernunft
  title-short: Kritik der praktischen Vernunft
  type: chapter
  volume: 5
  volume-title: Kritik der praktischen Vernunft. Kritik der
    Urtheilskraft
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
