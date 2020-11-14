```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/modern-humanities-research-association.csl'
references:
- author:
  - family: Doe
    given: John
  id: doe
  issued:
    date-parts:
    - - 1985
  publisher: Publisher
  title: Title
  type: book
- author:
  - family: Roe
    given: Rob
  id: roe
  issued:
    date-parts:
    - - 1985
  publisher: Publisher
  title: Title
  type: book
---

Text
====

Foo [@doe, VIII, 89]

Foo [@roe, III, 89]

Foo [@doe, LVIII, 89]

Foo [@roe, MVIII, 89]

Foo [@doe, CL, 89]

References
==========
^D
# Text

Foo[^1]

Foo[^2]

Foo[^3]

Foo[^4]

Foo[^5]

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-doe .csl-entry}
Doe, John, *Title* (Publisher, 1985)
:::

::: {#ref-roe .csl-entry}
Roe, Rob, *Title* (Publisher, 1985)
:::
:::

[^1]: John Doe, *Title* (Publisher, 1985), VIII, 89.

[^2]: Rob Roe, *Title* (Publisher, 1985), III, 89.

[^3]: Doe, LVIII, 89.

[^4]: Roe, MVIII, 89.

[^5]: Doe, CL, 89.
```
