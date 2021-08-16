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
% pandoc -C -t plain -Mlang=en
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
Foo (Aristotele, n.d., 50).

Aristotele. n.d. “Metafisica Et ‘Physica’.”
```

```
% pandoc -C -t plain -Mlang=it
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
Foo (Aristotele, s.d., 50).

Aristotele. s.d. «Metafisica et “Physica”».
```

