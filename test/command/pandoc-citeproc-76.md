```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Author
    given: Al
  id: item1
  issued:
    date-parts:
    - - 1998
  title: 'foo bar baz: bazbaz bar foo'
  type: 'article-journal'
- author:
  - family: Author
    given: Al
  id: item2
  issued:
    date-parts:
    - - 1998
  title: 'foo bar baz: the bazbaz bar foo'
  type: 'article-journal'
- author:
  - family: Author
    given: Al
  id: item3
  issued:
    date-parts:
    - - 1998
  title: 'foo bar baz: a bazbaz bar foo'
  type: 'article-journal'
- author:
  - family: Author
    given: Al
  id: item4
  issued:
    date-parts:
    - - 1998
  title: 'foo bar baz: an abazbaz bar foo'
  type: 'article-journal'
---

@item1, @item2, @item3, @item4
^D
Author (1998c), Author (1998d), Author (1998a), Author (1998b)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item3 .csl-entry}
Author, Al. 1998a. "Foo Bar Baz: A Bazbaz Bar Foo."
:::

::: {#ref-item4 .csl-entry}
---------. 1998b. "Foo Bar Baz: An Abazbaz Bar Foo."
:::

::: {#ref-item1 .csl-entry}
---------. 1998c. "Foo Bar Baz: Bazbaz Bar Foo."
:::

::: {#ref-item2 .csl-entry}
---------. 1998d. "Foo Bar Baz: The Bazbaz Bar Foo."
:::
:::
```
