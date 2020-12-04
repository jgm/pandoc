```
% pandoc --citeproc -t markdown-citations
---
csl: command/archeologie-medievale.csl
references:
- title: Work A
  id: a
  issued:
    date-parts:
    - - 2000
  author:
  - given: John
    family: Doe
  type: book
- title: Work B
  id: b
  issued:
    date-parts:
    - - 1990
  author:
  - given: Jane
    family: Roe
  type: book
---
@a @a @b @b @a @a @b @b
^D
Doe[^1] Doe[^2] Roe[^3] Roe[^4] Doe[^5] Doe[^6] Roe[^7] Roe[^8]

::: {#refs .references .csl-bib-body}
::: {#ref-a .csl-entry}
[[Doe J.]{.smallcaps} ]{.csl-block}
[2000, *Work A*,.]{.csl-left-margin}
:::

::: {#ref-b .csl-entry}
[[Roe J.]{.smallcaps} ]{.csl-block}
[1990, *Work B*,.]{.csl-left-margin}
:::
:::

[^1]: 2000

[^2]: *Ibid.*

[^3]: 1990

[^4]: *Ibid.*

[^5]: 2000

[^6]: *Ibid.*

[^7]: 1990

[^8]: *Ibid.*
```
