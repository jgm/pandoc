```
% pandoc --citeproc -t markdown-citations
Blah blah [@item1; @item2; @item3].

# References {-}

---
title: The Title
references:
- id: item1
  type: article-newspaper
  author:
  - family: Doe
    given: J.
  issued:
  - year: 2010
    month: 12
    day: 13
  title: The title
- id: item2
  type: article-newspaper
  author:
  - family: Doe
    given: J.
  issued:
  - year: 2007
    month: 12
    day: 12
  - year: 2007
    month: 12
    day: 13
  title: The title
- id: item3
  type: article-newspaper
  author:
  - family: Doe
    given: J.
  issued:
  - year: 2008
  title: The title
...
^D
Blah blah (Doe 2010, 2007, 2008).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item2 .csl-entry}
Doe, J. 2007. "The Title," December 12--13, 2007.
:::

::: {#ref-item3 .csl-entry}
---------. 2008. "The Title," 2008.
:::

::: {#ref-item1 .csl-entry}
---------. 2010. "The Title," December 13, 2010.
:::
:::
```
