```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
references:
- director:
    family: Hitchcock
    given: Alfred
  id: nbn
  issued:
    year: 1959
  language: 'en-US'
  publisher: 'Metro-Goldwyn-Mayer'
  publisher-place: USA
  title: North by Northwest
  type: motion_picture
---

[@nbn] is a spy thriller film.
^D
(Hitchcock 1959) is a spy thriller film.

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-nbn .csl-entry}
Hitchcock, Alfred, dir. 1959. *North by Northwest*. USA:
Metro-Goldwyn-Mayer.
:::
:::
```
