```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Averroes 1982)

Averroes. 1982. *The Epistle on the Possibility of Conjunction with the
Active Intellect by Ibn Rushd with the Commentary of Moses Narboni*.
Kalman P. Bland. Moreshet: Studies in Jewish History, Literature and
Thought 7. New York: Jewish Theological Seminary of America.


Formatted with pandoc and apa.csl, 2013-10-23:

(Averroes, 1982)

Averroes. (1982). *The epistle on the possibility of conjunction with
the active intellect by Ibn Rushd with the commentary of Moses Narboni*.
(K. P. Bland). New York: Jewish Theological Seminary of America.


NOTES:

- citeproc
	- term "edited and translated by" missing

}

@Book{averroes-bland,
  author       = {Averroes},
  title        = {The Epistle on the Possibility of Conjunction with the Active
                  Intellect by {Ibn Rushd} with the Commentary of {Moses Narboni}},
  date         = 1982,
  editor       = {Bland, Kalman P.},
  translator   = {Bland, Kalman P.},
  series       = {Moreshet: {Studies} in {Jewish} History, Literature and Thought},
  number       = 7,
  publisher    = {Jewish Theological Seminary of America},
  location     = {New York},
  keywords     = {primary},
  hyphenation  = {american},
  indextitle   = {Epistle on the Possibility of Conjunction, The},
  shorttitle   = {Possibility of Conjunction},
  annotation   = {A book entry with a series and a
                  number. Note the concatenation of the editor
                  and translator fields as well as the
                  indextitle field},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry with a series and a number. Note the
    concatenation of the editor and translator fields as well as the
    indextitle field
  author:
  - family: Averroes
  collection-number: 7
  collection-title: "Moreshet: Studies in Jewish history, literature and
    thought"
  editor:
  - family: Bland
    given: Kalman P.
  id: averroes-bland
  issued: 1982
  keyword: primary
  language: en-US
  publisher: Jewish Theological Seminary of America
  publisher-place: New York
  title: The epistle on the possibility of conjunction with the active
    intellect by Ibn Rushd with the commentary of Moses Narboni
  title-short: Possibility of conjunction
  translator:
  - family: Bland
    given: Kalman P.
  type: book
---


```
