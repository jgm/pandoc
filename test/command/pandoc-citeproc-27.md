```
% pandoc --citeproc -t markdown-citations
---
csl: command/science.csl
references:
- author:
  - family: AuthorOne
    given: Joe
  - family: AuthorTwo
    given: Jill
  container-title: Some Journal
  id: AuthorOne2014
  issue: X
  issued:
    date-parts:
    - - 2014
  page: 'XXXX-YYYY'
  title: Sample Title
  type: 'article-journal'
  volume: XX
---

Minimal example
===============

Here is some text that needs a citation [@AuthorOne2014].
^D
# Minimal example

Here is some text that needs a citation (*1*).

::: {#refs .references .csl-bib-body}
::: {#ref-AuthorOne2014 .csl-entry}
[1. ]{.csl-left-margin}[J. AuthorOne, J. AuthorTwo, *Some Journal*, in
press.]{.csl-right-inline}
:::
:::
```
