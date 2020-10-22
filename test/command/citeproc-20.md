```
% pandoc --citeproc -t plain
Lorem ipsum dolor sit amet^[Consectetur adipisicing elit: «sed do eiusmod tempor incididunt» [@doe_1989, 15].].

---
csl: command/chicago-fullnote-bibliography.csl
suppress-bibliography: true
references:
- id: doe_1989
  author:
    - family: Doe
      given: John
  issued:
    - year: 1989
  publisher: ABC
  publisher-place: New York
  title: Tempor
  type: book
...
^D
Lorem ipsum dolor sit amet[1].

[1] Consectetur adipisicing elit: «sed do eiusmod tempor incididunt»
(John Doe, Tempor (New York: ABC, 1989), 15).
```
