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
