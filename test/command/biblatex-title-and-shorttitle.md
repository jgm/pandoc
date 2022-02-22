```
% pandoc -f biblatex -t markdown -s
@comment{

TODO:
Slight inconsistency:
When a biblatex “title” field contains a colon, the part before the colon is mapped to CSL “title-short”.
When there’s a biblatex “title” and a “subtitle” field, CSL “title-short” is not set, though it would make at least as much sense to map “title” (without “subtitle”) to CSL “title-short” in this case.

CSL “container-title-short” could also be set
- from biblatex “shortjournal”
- for inbook, incollection etc. from the “shorttitle” field of the cross-referenced book, collection etc. entry (see item5, item6)
... but it might not really be worth it, “container-title-short” not being used once in my sample of 70+ CSL styles.

}

@book{item4,
	Shorttitle = {The Shorttitle},
	Subtitle = {And a Subtitle, in Two Separate Fields; plus a Separate “Shorttitle” Field},
	Title = {The Title: With a Colon in the “Title” Field}}

@book{item3,
	Subtitle = {And a Subtitle, in two separate fields},
	Title = {The Title: With a Colon in the “title” field}}

@book{item2,
	Subtitle = {The Subtitle, In Two Separate fields},
	Title = {The Title}}

@book{item1,
	Title = {The Title: And the Subtitle, all in the “title” Field}}

@inbook{item5,
	Title = {The inbook Title: And the Subtitle, all in the “title” Field},
	Crossref = {item6}}

@book{item6,
	Title = {The Title: And the Subtitle, all in the “title” Field},
	Shorttitle = {The Shorttitle},
}
^D
---
nocite: "[@*]"
references:
- id: item4
  title: "The title: With a colon in the \"title\" field: And a subtitle,
    in two separate fields; plus a separate \"shorttitle\" field"
  title-short: The shorttitle
  type: book
- id: item3
  title: "The title: With a colon in the \"title\" field: And a subtitle,
    in two separate fields"
  title-short: The title
  type: book
- id: item2
  title: "The title: The subtitle, in two separate fields"
  title-short: The title
  type: book
- id: item1
  title: "The title: And the subtitle, all in the \"title\" field"
  title-short: The title
  type: book
- container-title: "The title: And the subtitle, all in the \"title\"
    field"
  id: item5
  title: "The inbook title: And the subtitle, all in the \"title\" field"
  title-short: The inbook title
  type: chapter
- id: item6
  title: "The title: And the subtitle, all in the \"title\" field"
  title-short: The shorttitle
  type: book
---


```
