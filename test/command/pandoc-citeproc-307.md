```
% pandoc --citeproc -t markdown-citations
---
lang: 'fr-FR'
references:
- author:
  - family: Bazin
    given: André
  container-title: Cahiers du cinéma
  id: bazin_cybernetique_1954
  issue: 36
  issued:
    date-parts:
    - - 1954
      - 6
  page: '22-27'
  title: 'La Cybernétique d''André Cayatte'
  type: 'article-journal'
---

Bonjour[@bazin_cybernetique_1954] !
^D
Bonjour(Bazin 1954) !

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-bazin_cybernetique_1954 .csl-entry}
Bazin, André. 1954. « La Cybernétique d'André Cayatte ». *Cahiers du
cinéma*, nᵒ 36 (juin): 22‑27.
:::
:::
```
