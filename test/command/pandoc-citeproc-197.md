```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-fullnote-bibliography.csl'
nocite: '@test'
references:
- editor:
  - family: Abelard
    given: Peter
  id: test
  issued:
    date-parts:
    - - 1989
  publisher: Clarendon Press
  publisher-place: Oxford
  title: Test
  type: book
---

This is a test [@test].
^D
This is a test.[^1]

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-test .csl-entry}
Abelard, Peter, ed. *Test*. Oxford: Clarendon Press, 1989.
:::
:::

[^1]: Peter Abelard, ed., *Test* (Oxford: Clarendon Press, 1989).
```
