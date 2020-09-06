```
% pandoc --citeproc -t markdown-citations
---
references:
- id: test
  title: Essays presented to N.R. Ker (On Art)
- id: test2
  title: '*Test:* An experiment: An abridgement'
---

@test; @test2
^D
"Essays Presented to N.R. Ker (On Art)" (n.d.); "*Test:* An Experiment:
An Abridgement" (n.d.)

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-test .csl-entry}
"Essays Presented to N.R. Ker (On Art)." n.d.
:::

::: {#ref-test2 .csl-entry}
"*Test:* An Experiment: An Abridgement." n.d.
:::
:::
```
