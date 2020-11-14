```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-annotated-bibliography.csl'
references:
- URL: 'https://www.worldcat.org/'
  accessed:
    date-parts:
    - - 2014
      - 9
      - 19
  author:
  - literal: OCLC
  first-reference-note-number: 1
  id: OCLC_i1099
  title: WorldCat
  type: webpage
---

Title
=====

Some text.[^1]

[^1]: Comment regarding text, supported by citation [@OCLC_i1099].
^D
# Title

Some text.[^1]

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-OCLC_i1099 .csl-entry}
OCLC. "WorldCat." Accessed September 19, 2014.
<https://www.worldcat.org/>.
:::
:::

[^1]: Comment regarding text, supported by citation (OCLC, "WorldCat").
```
