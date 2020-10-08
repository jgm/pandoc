```
% pandoc -f biblatex -t markdown -s
@book{goethe2005,
  langid = {german},
  location = {{Frankfurt am Main}},
  title = {Not A Real Book},
  date = {2005},
  author = {family=Goethe, given=Johann Wolfgang, prefix=von, useprefix=false and given=Antonie, prefix=van, family=Leeuwenhoek, useprefix=true}, editor = {Schöne, Albrecht}
}

^D
---
nocite: "[@*]"
references:
- author:
  - dropping-particle: von
    family: Goethe
    given: Johann Wolfgang
  - family: Leeuwenhoek
    given: Antonie
    non-dropping-particle: van
  editor:
  - family: Schöne
    given: Albrecht
  id: goethe2005
  issued: 2005
  language: de-DE
  publisher-place: Frankfurt am Main
  title: Not A Real Book
  type: book
---


```
