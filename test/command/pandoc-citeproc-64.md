```
% pandoc --citeproc -t markdown-citations --csl command/chicago-fullnote-bibliography.csl
---
bibliography:
- command/biblio.bib
nocite: '[@*]'
---

^D
:::::: {#refs .references .csl-bib-body .hanging-indent entry-spacing="0"}
::: {#ref-item2 .csl-entry}
Doe, John. "Article." *Journal of Generic Studies* 6 (2006): 33--34.
:::

::: {#ref-item1 .csl-entry}
---------. *First Book*. Cambridge: Cambridge University Press, 2005.
:::

::: {#ref-пункт3 .csl-entry}
Doe, John, and Jenny Roe. "Why Water Is Wet." In *Third Book*, edited by
Sam Smith. Oxford: Oxford University Press, 2007.
:::
::::::
```
