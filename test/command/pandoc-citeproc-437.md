```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
csl: command/issue437.csl
references:
- author:
  - family: Smith
    given: John
  id: hirt2009
  issued:
  - year: 2009
  publisher: Publishing House
  publisher-place: Lausanne
  title: Some Book
  type: book
---

> Here is a quote. [@hirt2009]
^D
> Here is a quote.[^1]

[^1]: John Smith, Some Book, Lausanne, 2009
```
