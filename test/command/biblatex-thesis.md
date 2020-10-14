```
% pandoc -f biblatex -t markdown -s
@comment{excerpted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

TODO: Uppercase letters following hyphens need to be converted to lowercase, too (e.g., "r" in "High-Resolution". -- Same for citeproc when doing title-case conversion!)
}

@thesis{geer,
	Annotation = {This is a typical thesis entry for a PhD thesis. Note the type field in the database file which uses a localization key. Also note the format of the printed name and compare the useprefix option in the options field as well as vangennep},
	Author = {de Geer, Ingrid},
	Date = 1985,
	Hyphenation = {british},
	Institution = {Uppsala Universitet},
	Location = {Uppsala},
	Options = {useprefix=false},
	Subtitle = {The {Orkney} Earldom of the Twelfth Century. {A} Musicological Study},
	Title = {Earl, Saint, Bishop, Skald~-- and Music},
	Type = {phdthesis}}

@thesis{loh,
	Annotation = {This is a typical thesis entry for an MA thesis. Note the type field in the database file which uses a localization key},
	Author = {Loh, Nin C.},
	Date = 1992,
	Hyphenation = {american},
	Institution = {Massachusetts Institute of Technology},
	Location = {Cambridge, Mass.},
	Title = {High-Resolution Micromachined Interferometric Accelerometer},
	Type = {mathesis}}

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
- annote: This is a typical thesis entry for an MA thesis. Note the type
    field in the database file which uses a localization key
  author:
  - family: Loh
    given: Nin C.
  genre: Master's thesis
  id: loh
  issued: 1992
  language: en-US
  publisher: Massachusetts Institute of Technology
  publisher-place: Cambridge, Mass.
  title: High-resolution micromachined interferometric accelerometer
  type: thesis
---


```
