```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
references:
- container-title: Magazine
  id: item1
  issued:
    year: 2012
  title: Title A
  type: 'article-magazine'
- container-title: Magazine
  id: item2
  issued:
    year: 2012
  title: Title B
  type: 'article-magazine'
- container-title: Magazine
  id: item3
  issued:
    year: 2012
  title: Title C
  type: 'article-magazine'
- container-title: Magazine
  id: item4
  issued:
    year: 2012
  title: Title D
  type: 'article-magazine'
- container-title: Newspaper
  id: item5
  issued:
    year: 2012
  title: Title E
  type: 'article-magazine'
- container-title: Newspaper
  id: item6
  issued:
    year: 2012
  title: Title F
  type: 'article-magazine'
---

@item1 [p. 3], @item2, @item3, @item4, @item5, @item6
^D
*Magazine* (2012a, 3), *Magazine* (2012b), *Magazine* (2012c),
*Magazine* (2012d), *Newspaper* (2012a), *Newspaper* (2012b)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
*Magazine*. 2012a. "Title A," 2012.
:::

::: {#ref-item2 .csl-entry}
---------. 2012b. "Title B," 2012.
:::

::: {#ref-item3 .csl-entry}
---------. 2012c. "Title C," 2012.
:::

::: {#ref-item4 .csl-entry}
---------. 2012d. "Title D," 2012.
:::

::: {#ref-item5 .csl-entry}
*Newspaper*. 2012a. "Title E," 2012.
:::

::: {#ref-item6 .csl-entry}
---------. 2012b. "Title F," 2012.
:::
:::
```
