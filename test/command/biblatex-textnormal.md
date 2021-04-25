```
% pandoc -f biblatex -t markdown -s
@book{item1,
	Title = {The Title \textnormal{of this book}},
}

^D
---
nocite: "[@*]"
references:
- id: item1
  title: The title [of this book]{.nodecor}
  type: book
---


```
