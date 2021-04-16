```
% pandoc --citeproc -t plain
---
lang: en-US
csl: command/apa.csl
references:
- id: a1
  type: book
  author:
  - family: Ubina
    given: A. John
  issued: 1985
- id: a2
  type: book
  author:
  - family: Über
    given: Aglaia
  issued: 1996
- id: a3
  type: book
  author:
  - family: Oñate
    given: José
  issued: 1985
- id: a4
  type: book
  author:
  - family: Onush
    given: Frank
  issued: 2002
- id: a5
  type: book
  author:
  - family: O'Neil
    given: Timothy
  issued: 2010
---

[@a1;@a2;@a3;@a4;@a5]
^D
(O’Neil, 2010; Oñate, 1985; Onush, 2002; Über, 1996; Ubina, 1985)

O’Neil, T. (2010).

Oñate, J. (1985).

Onush, F. (2002).

Über, A. (1996).

Ubina, A. J. (1985).
```

```
% pandoc --citeproc -t plain
---
lang: es
csl: command/apa.csl
references:
- id: a1
  type: book
  author:
  - family: Ubina
    given: A. John
  issued: 1985
- id: a2
  type: book
  author:
  - family: Über
    given: Aglaia
  issued: 1996
- id: a3
  type: book
  author:
  - family: Oñate
    given: José
  issued: 1985
- id: a4
  type: book
  author:
  - family: Onush
    given: Frank
  issued: 2002
- id: a5
  type: book
  author:
  - family: O'Neil
    given: Timothy
  issued: 2010
---

[@a1;@a2;@a3;@a4;@a5]
^D
(O’Neil, 2010; Onush, 2002; Oñate, 1985; Über, 1996; Ubina, 1985)

O’Neil, T. (2010).

Onush, F. (2002).

Oñate, J. (1985).

Über, A. (1996).

Ubina, A. J. (1985).
```

```
% pandoc -C -t plain
---
nocite: '@*'
lang: fr-FR-u-kb-true
references:
- id: cote
  author: cote
- id: côte
  author: côte
- id: coté
  author: coté
- id: côté
  author: côté
...
^D
cote. s. d.

côte. s. d.

coté. s. d.

côté. s. d.
```
