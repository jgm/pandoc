```
% pandoc --citeproc -t markdown-citations
---
csl: command/apa.csl
nocite: '@*'
references:
- author:
  - literal: NN
  id: 'Y'
  issued:
  - year: 1950
  title: 'Date: Year'
  title-short: Date
  type: webpage
- author:
  - literal: NN
  id: Y/Y
  issued:
  - year: 1951
  - year: 1952
  title: 'Date range: Year'
  title-short: Date range
  type: webpage
- author:
  - literal: NN
  id: YM
  issued:
  - month: 1
    year: 1953
  title: 'Date: Year+month'
  title-short: Date
  type: webpage
- author:
  - literal: NN
  id: YM/YM
  issued:
  - month: 1
    year: 1954
  - month: 2
    year: 1955
  title: 'Date range: Year+month'
  title-short: Date range
  type: webpage
- author:
  - literal: NN
  id: YM/YM_same
  issued:
  - month: 1
    year: 1956
  - month: 2
    year: 1956
  title: 'Date range: Year+month, same year'
  title-short: Date range
  type: webpage
- author:
  - literal: NN
  id: YMD
  issued:
  - day: 15
    month: 1
    year: 1958
  title: 'Date: Year+month+day'
  title-short: Date
  type: webpage
- author:
  - literal: NN
  id: YMD/YMD
  issued:
  - day: 15
    month: 1
    year: 1959
  - day: 16
    month: 2
    year: 1960
  title: 'Date range: Year+month+day'
  title-short: Date range
  type: webpage
- author:
  - literal: NN
  id: YMD/YMD_same
  issued:
  - day: 15
    month: 1
    year: 1961
  - day: 16
    month: 1
    year: 1962
  title: 'Date range: Year+month+day, same month'
  title-short: Date range
  type: webpage
- author:
  - literal: NN
  id: YS
  issued:
  - season: 3
    year: 1963
  title: 'Date: Year+season'
  title-short: Date
  type: webpage
- author:
  - literal: NN
  id: YS/YS
  issued:
  - season: 1
    year: 1964
  - season: 4
    year: 1965
  title: 'Date range: Year+season'
  title-short: Date range
  type: webpage
- author:
  - literal: NN
  id: YS/YS_same
  issued:
  - season: 2
    year: 1966
  - season: 4
    year: 1966
  title: 'Date range: Year+season, same year'
  title-short: Date range
  type: webpage
---

^D
::: {#refs .references .csl-bib-body .hanging-indent line-spacing="2"}
::: {#ref-Y .csl-entry}
NN. (1950). Date: Year.
:::

::: {#ref-Y/Y .csl-entry}
NN. (1951--1952). Date range: Year.
:::

::: {#ref-YM .csl-entry}
NN. (1953, January). Date: Year+month.
:::

::: {#ref-YM/YM .csl-entry}
NN. (1954--1955, January--February). Date range: Year+month.
:::

::: {#ref-YM/YM_same .csl-entry}
NN. (1956, January--February). Date range: Year+month, same year.
:::

::: {#ref-YMD .csl-entry}
NN. (1958, January 15). Date: Year+month+day.
:::

::: {#ref-YMD/YMD .csl-entry}
NN. (1959--1960, January 15--February 16). Date range: Year+month+day.
:::

::: {#ref-YMD/YMD_same .csl-entry}
NN. (1961--1962, January 15--16). Date range: Year+month+day, same
month.
:::

::: {#ref-YS .csl-entry}
NN. (1963, Autumn). Date: Year+season.
:::

::: {#ref-YS/YS .csl-entry}
NN. (1964--1965, Spring--Winter). Date range: Year+season.
:::

::: {#ref-YS/YS_same .csl-entry}
NN. (1966, Summer--Winter). Date range: Year+season, same year.
:::
:::
```
