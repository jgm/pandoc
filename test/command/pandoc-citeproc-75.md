```
% pandoc --citeproc -t markdown-citations
---
csl: command/apa.csl
references:
- author:
  - family: Doe
    given: John
  id: test
  issued:
    date-parts:
    - - 2006
  title: Test
  type: 'article-journal'
  volume: 81
---

[@test, p. 6]

[@test, chap. 6]

[@test, n. 6]

[@test, pp. 34-36, 38-39]

[@test, sec. 3]

[@test, p.3]

[@test, 33-35, 38-39]

[@test, 14]

[@test bk. VI]

[@test, no. 6]

[@test, nos. 6 and 7]
^D
(Doe, 2006, p. 6)

(Doe, 2006, Chapter 6)

(Doe, 2006, n. 6)

(Doe, 2006, pp. 34--36, 38--39)

(Doe, 2006, sec. 3)

(Doe, 2006, p. 3)

(Doe, 2006, pp. 33--35, 38--39)

(Doe, 2006, p. 14)

(Doe, 2006, bk. VI)

(Doe, 2006, no. 6)

(Doe, 2006, no. 6 and 7)

::: {#refs .references .csl-bib-body .hanging-indent line-spacing="2"}
::: {#ref-test .csl-entry}
Doe, J. (2006). Test, *81*.
:::
:::
```
