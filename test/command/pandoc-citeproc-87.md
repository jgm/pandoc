```
% pandoc --citeproc -t markdown-citations
---
references:
- URL: 'http://www.example.com'
  author:
  - family: Doe
    given: John
  container-title: The Web Site
  id: item1
  issued:
    date-parts:
    - - 2006
      - 10
      - 26
  title: Title
  type: webpage
- URL: 'http://www.example.com'
  author:
  - family: Doe
    given: John
  container-title: The Web Site
  id: item2
  issued:
    date-parts:
    - - 2006
      - 10
      - 26
    - - 2006
      - 11
      - 27
  title: Title
  type: webpage
- URL: 'http://www.example.com'
  author:
  - family: Doe
    given: John
  container-title: The Web Site
  id: item2a
  issued:
    date-parts:
    - - 2006
      - 10
    - - 2006
      - 11
  title: Title
  type: webpage
- URL: 'http://www.example.com'
  author:
  - family: Doe
    given: John
  container-title: The Web Site
  id: item2b
  issued:
    date-parts:
    - - 2006
      - 12
      - 31
    - - 2007
      - 1
      - 1
  title: Title
  type: webpage
- URL: 'http://www.example.com'
  author:
  - family: Doe
    given: John
  container-title: The Newspaper
  id: item3
  issued:
    date-parts:
    - - 2006
      - 10
      - 26
    - - 2006
      - 11
      - 27
  title: Title
  type: 'article-newspaper'
- URL: 'http://www.example.com'
  author:
  - family: Doe
    given: John
  container-title: The Newspaper
  id: item3b
  issued:
    date-parts:
    - - 2006
      - 10
    - - 2006
      - 11
  title: Title
  type: 'article-newspaper'
---

@item1 -- webpage, date

@item2 -- webpage, date range

@item2a -- webpage, date range YM

@item2b -- webpage, date range across years

@item3 -- article-newspaper

@item3b -- article-newspaper YM

References
==========
^D
Doe (2006c) -- webpage, date

Doe (2006d) -- webpage, date range

Doe (2006a) -- webpage, date range YM

Doe (2006--2007) -- webpage, date range across years

Doe (2006e) -- article-newspaper

Doe (2006b) -- article-newspaper YM

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item2a .csl-entry}
Doe, John. 2006a. "Title." The Web Site. October--November 2006.
<http://www.example.com>.
:::

::: {#ref-item3b .csl-entry}
---------. 2006b. "Title." *The Newspaper*, October--November 2006.
<http://www.example.com>.
:::

::: {#ref-item1 .csl-entry}
---------. 2006c. "Title." The Web Site. October 26, 2006.
<http://www.example.com>.
:::

::: {#ref-item2 .csl-entry}
---------. 2006d. "Title." The Web Site. October 26--November 27, 2006.
<http://www.example.com>.
:::

::: {#ref-item3 .csl-entry}
---------. 2006e. "Title." *The Newspaper*, October 26--November 27,
2006. <http://www.example.com>.
:::

::: {#ref-item2b .csl-entry}
---------. 2006--2007. "Title." The Web Site. December 31, 2006--January
1, 2007. <http://www.example.com>.
:::
:::
```
