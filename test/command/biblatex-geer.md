```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Geer 1985)

Geer, Ingrid de. 1985. “Earl, Saint, Bishop, Skald – and Music: The
Orkney Earldom of the Twelfth Century. A Musicological Study.” PhD
thesis, Uppsala: Uppsala Universitet.


Formatted with pandoc and apa.csl, 2013-10-23:

(Geer, 1985)

Geer, I. de. (1985). *Earl, saint, bishop, skald – and music: The Orkney
earldom of the twelfth century. A musicological study* (PhD thesis).
Uppsala Universitet, Uppsala.


}

@Thesis{geer,
  author       = {de Geer, Ingrid},
  title        = {Earl, Saint, Bishop, Skald~-- and Music},
  type         = {phdthesis},
  institution  = {Uppsala Universitet},
  date         = 1985,
  subtitle     = {The {Orkney} Earldom of the Twelfth Century. {A} Musicological
                  Study},
  location     = {Uppsala},
  options      = {useprefix=false},
  hyphenation  = {british},
  annotation   = {This is a typical thesis entry for a PhD thesis. Note
                  the type field in the database file which uses a
                  localization key. Also note the format of the printed name and
                  compare the useprefix option in the options
                  field as well as vangennep},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a typical thesis entry for a PhD thesis. Note the type
    field in the database file which uses a localization key. Also note
    the format of the printed name and compare the useprefix option in
    the options field as well as vangennep
  author:
  - dropping-particle: de
    family: Geer
    given: Ingrid
  genre: PhD thesis
  id: geer
  issued: 1985
  language: en-GB
  publisher: Uppsala Universitet
  publisher-place: Uppsala
  title: "Earl, saint, bishop, skald -- and music: The Orkney earldom of
    the twelfth century. A musicological study"
  title-short: Earl, saint, bishop, skald -- and music
  type: thesis
---


```
