```
% pandoc -f biblatex -t markdown -s
@inbook{item1,
	Author = {Author, Al},
	Booksubtitle = {Booksubtitle},
	Booktitle = {Booktitle},
	Booktitleaddon = {Booktitleaddon},
	Date = {2011},
	Hyphenation = {french},
	Location = {Location},
	Mainsubtitle = {Mainsubtitle},
	Maintitle = {Maintitle},
	Maintitleaddon = {Maintitleaddon},
	Number = {3},
	Publisher = {Publisher},
	Series = {Series},
	Subtitle = {Subtitle},
	Title = {Title of the ``inbook'' Entry},
	Titleaddon = {Titleaddon},
	Volume = {4}}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Author
    given: Al
  collection-number: 3
  collection-title: Series
  container-title: "Maintitle: Mainsubtitle. Maintitleaddon"
  id: item1
  issued: 2011
  language: fr-FR
  publisher: Publisher
  publisher-place: Location
  title: "Title of the \"inbook\" Entry: Subtitle. Titleaddon"
  type: chapter
  volume: 4
  volume-title: "Booktitle: Booksubtitle. Booktitleaddon"
---


```
