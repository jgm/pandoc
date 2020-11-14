```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
csl: command/vancouver.csl
references:
- author:
  - family: James
    given: M.R.C.E.L.
  id: james
- author:
  - family: MacFarlane
    given: J. G.
  id: macfarlane
---

@james; @macfarlane
^D
(1); (2)

::: {#refs .references .csl-bib-body}
::: {#ref-james .csl-entry}
[1. ]{.csl-left-margin}[James MRCEL. ]{.csl-right-inline}
:::

::: {#ref-macfarlane .csl-entry}
[2. ]{.csl-left-margin}[MacFarlane JG. ]{.csl-right-inline}
:::
:::
```
