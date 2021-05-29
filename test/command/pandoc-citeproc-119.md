```
% pandoc --citeproc -t markdown-citations
---
bibliography: 'command/averroes.bib'
csl: command/apa.csl
---

@averroes/bland; @averroes/hannes; @averroes/hercz

References {#references .unnumbered}
==========
^D
Averroes (1982); Averroes (1892); Averroes (1869)

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent line-spacing="2"}
::: {#ref-averroes/hercz .csl-entry}
Averroes. (1869). *Drei Abhandlungen über die Conjunction des separaten
Intellects mit dem Menschen: Von Averroes (Vater und Sohn), aus dem
Arabischen übersetzt von Samuel Ibn Tibbon*. (J. Hercz, Ed. & Trans.).
Berlin: S. Hermann.
:::

::: {#ref-averroes/hannes .csl-entry}
Averroes. (1892). *Des Averroës Abhandlung: "Über die Möglichkeit der
Conjunktion" oder "Über den materiellen Intellekt"*. (L. Hannes, Ed. &
Trans.). Halle an der Saale: C. A. Kaemmerer.
:::

::: {#ref-averroes/bland .csl-entry}
Averroes. (1982). *The epistle on the possibility of conjunction with
the active intellect by Ibn Rushd with the commentary of Moses Narboni*.
(K. P. Bland, Ed. & Trans.). New York: Jewish Theological Seminary of
America.
:::
:::
```
