```
% pandoc -f biblatex -t markdown -s
@article{item1,
    Title = {The Title:
     \textit{italics},
     \textbf{bold},
     \textsubscript{subscript},
     \textsuperscript{superscript},
     \textsc{small-caps}}
}
^D
---
nocite: "[@*]"
references:
- id: item1
  title: "The title: *Italics*, **bold**, ~subscript~, ^superscript^,
    [small-caps]{.smallcaps}"
  title-short: The title
  type: article-journal
---


```
