```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/zeitschrift-fur-kunstgeschichte.csl'
references:
- editor:
  - family: Nietzsche
    given: Friedrich
  id: Nie72
  issued:
  - year: 1872
  title: Die geburt
  type: book
---

@Nie72
^D
Friedrich Nietzsche (ed.)[^1]

::: {#refs .references .csl-bib-body .hanging-indent entry-spacing="0"}
::: {#ref-Nie72 .csl-entry}
Nietzsche, Friedrich (ed.), *Die geburt*, 1872.
:::
:::

[^1]: *Die geburt*, 1872.
```
