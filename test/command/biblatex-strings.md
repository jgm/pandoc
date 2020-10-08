```
% pandoc -f biblatex -t markdown -s
@comment{excerpt from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

@string{anch-ie = {Angew.~Chem. Int.~Ed.}}

@article{herrmann,
	Author = {Herrmann, Wolfgang A. and Öfele, Karl and Schneider, Sabine K. and Herdtweck, Eberhardt and Hoffmann, Stephan D.},
	Date = 2006,
	Hyphenation = {english},
	Indextitle = {Carbocyclic carbene as an efficient catalyst, A},
	Journaltitle = anch-ie,
	Number = 23,
	Pages = {3859-3862},
	Title = {A Carbocyclic Carbene as an Efficient Catalyst Ligand for {C--C} Coupling Reactions},
	Volume = 45}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Herrmann
    given: Wolfgang A.
  - family: Öfele
    given: Karl
  - family: Schneider
    given: Sabine K.
  - family: Herdtweck
    given: Eberhardt
  - family: Hoffmann
    given: Stephan D.
  container-title: Angew. Chem. Int. Ed.
  id: herrmann
  issue: 23
  issued: 2006
  language: en-US
  page: 3859-3862
  title: A carbocyclic carbene as an efficient catalyst ligand for C--C
    coupling reactions
  type: article-journal
  volume: 45
---


```
