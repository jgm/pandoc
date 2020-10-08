```
% pandoc -f biblatex -t markdown -s
@book{item1,
	Title = {The Title: \bibstring{newseries}},
        Hyphenation = {english}
}

^D
---
nocite: "[@*]"
references:
- id: item1
  language: en-US
  title: "The title: New series"
  title-short: The title
  type: book
---


```
