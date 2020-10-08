```
% pandoc -f biblatex -t markdown -s
@book{item1,
	Author = {Author, Al},
	Date = {2013},
	Hyphenation = {french},
	Location = {Location},
	Mainsubtitle = {Mainsubtitle},
	Maintitle = {Maintitle},
	Maintitleaddon = {Maintitleaddon},
	Number = {3},
	Publisher = {Publisher},
	Series = {Series},
	Subtitle = {Subtitle},
	Title = {Title of the Book},
	Titleaddon = {Titleaddon},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Author
    given: Al
  collection-number: 3
  collection-title: Series
  id: item1
  issued: 2013
  language: fr-FR
  publisher: Publisher
  publisher-place: Location
  title: "Maintitle: Mainsubtitle. Maintitleaddon"
  type: book
  volume-title: "Title of the Book: Subtitle. Titleaddon"
---


```
