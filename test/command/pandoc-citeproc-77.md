```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-fullnote-bibliography.csl'
references:
- author:
  - family: Doe
    given: 'John, III'
    parse-names: true
  id: item1
  type: book
- author:
  - family: van Gogh
    given: Vincent
    parse-names: true
  id: item2
  type: book
- author:
  - family: Humboldt
    given: Alexander von
    parse-names: true
  id: item3
  type: book
- author:
  - family: Bennett
    given: 'Frank G.,! Jr.'
    parse-names: true
  id: item4
  type: book
- author:
  - family: Dumboldt
    given: 'Ezekiel, III'
    parse-names: true
  id: item5
  type: book
---

[@item1; @item2; @item3; @item4; @item5]
^D
[^1]

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item4 .csl-entry}
Bennett, Frank G., Jr., n.d.
:::

::: {#ref-item1 .csl-entry}
Doe, John, III, n.d.
:::

::: {#ref-item5 .csl-entry}
Dumboldt, Ezekiel, III, n.d.
:::

::: {#ref-item2 .csl-entry}
Gogh, Vincent van, n.d.
:::

::: {#ref-item3 .csl-entry}
Humboldt, Alexander von, n.d.
:::
:::

[^1]: John Doe III, n.d.; Vincent van Gogh, n.d.; Alexander von
    Humboldt, n.d.; Frank G. Bennett, Jr., n.d.; Ezekiel Dumboldt III,
    n.d.
```
