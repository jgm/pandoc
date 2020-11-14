```
% pandoc --citeproc -t markdown-citations
---
csl: command/apa.csl
references:
- URL: 'http://geekfeminism.wikia.com/wiki/Geek_Feminism'
  accessed:
    day: 10
    month: 4
    year: 2013
  container-title: Geek Feminism
  custom2: ok.mm
  id: Feminism2013gf
  issued:
    day: 10
    month: 4
    year: 2013
  keyword: gender
  title: Geek Feminism
  type: webpage
- URL: 'http://geekfeminism.wikia.com/wiki/Category:Communities'
  accessed:
    day: 19
    month: 10
    year: 2011
  container-title: Geek Feminism
  custom2: gender.mm
  id: Feminism2011ces
  issued:
    day: 14
    month: 8
    year: 2011
  keyword: gender
  title: Communities
  type: 'entry-encyclopedia'
---

Test
====

I have two citations [@Feminism2013gf; @Feminism2011ces].

References
==========
^D
# Test

I have two citations ("Communities," 2011; "Geek Feminism," 2013).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent line-spacing="2"}
::: {#ref-Feminism2011ces .csl-entry}
Communities. (2011, August 14). In *Geek Feminism*. Retrieved from
<http://geekfeminism.wikia.com/wiki/Category:Communities>
:::

::: {#ref-Feminism2013gf .csl-entry}
Geek Feminism. (2013, April 10). *Geek Feminism*. Retrieved April 10,
2013, from <http://geekfeminism.wikia.com/wiki/Geek_Feminism>
:::
:::
```
