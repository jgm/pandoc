```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-author-date-with-original-date-and-status.csl'
references:
- author:
  - family: Faraday
    given: Carry
  container-title: Seven Trips beyond the Asteroid Belt
  editor:
  - family: Oring
    given: James
  id: 'Faraday-forthcoming'
  publisher: Launch Press
  publisher-place: 'Cape Canaveral, FL'
  status: forthcoming
  title: Protean photography
  type: chapter
---

[@Faraday-forthcoming]

References
==========
^D
(Faraday, forthcoming)

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-Faraday-forthcoming .csl-entry}
Faraday, Carry. Forthcoming. "Protean Photography." In *Seven Trips
Beyond the Asteroid Belt*, edited by James Oring. Cape Canaveral, FL:
Launch Press.
:::
:::
```
