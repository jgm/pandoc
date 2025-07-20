```
% pandoc --citeproc -Mlang=it-IT -t markdown-citations
Foo [@a 50: «Disse: "bar"»]. «Disse: "baz"»

---
suppress-bibliography: true
references:
- id: a
  author:
    - literal: Aristotele
  title: Metafisica
  type: book
...
^D
Foo (Aristotele, s.d., 50: «Disse: "bar"»). «Disse: "baz"»
```

The Quoted is passed to citeproc as a Span ("",["csl-quoted"],[])
so that flipflopping and localization occur.
```
% pandoc -C -t plain -Mlang=en --csl command/le-tapuscrit-note.csl
---
references:
- id: a
  author:
    - literal: Aristotele
  title: Metafisica et "Physica"
  type: article-journal
...

Foo [@a 50].
^D
Foo.[1]

ARISTOTELE, “Metafisica et ‘Physica’.”

[1] Aristotele, “Metafisica et ‘Physica’,” p. 50.
```

```
% pandoc -C -t plain -Mlang=it --csl command/le-tapuscrit-note.csl
---
references:
- id: a
  author:
    - literal: Aristotele
  title: Metafisica et "Physica"
  type: article-journal
...

Foo [@a 50].
^D
Foo.[1]

ARISTOTELE, «Metafisica et “Physica”».

[1] Aristotele, «Metafisica et “Physica”», p. 50.
```

