```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Shore 1991)

Shore, Bradd. 1991. “Twice-born, Once Conceived: Meaning Construction
and Cultural Cognition.” *American Anthropologist, New Series* 93 (1)
(March): 9–27.


Formatted with pandoc and apa.csl, 2013-10-23:

(Shore, 1991)

Shore, B. (1991). Twice-born, once conceived: Meaning construction and
cultural cognition. *American Anthropologist, new series*, *93*(1),
9–27.


}

@Article{shore,
  author       = {Shore, Bradd},
  title        = {Twice-Born, Once Conceived},
  journaltitle = {American Anthropologist},
  date         = {1991-03},
  subtitle     = {Meaning Construction and Cultural Cognition},
  series       = {newseries},
  volume       = 93,
  number       = 1,
  pages        = {9-27},
  annotation   = {An article entry with series,
                  volume, and number fields. Note the format
                  of the series which is a localization key},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry with series, volume, and number fields. Note
    the format of the series which is a localization key
  author:
  - family: Shore
    given: Bradd
  collection-title: New series
  container-title: American Anthropologist
  id: shore
  issue: 1
  issued: 1991-03
  page: 9-27
  title: "Twice-born, once conceived: Meaning construction and cultural
    cognition"
  title-short: Twice-born, once conceived
  type: article-journal
  volume: 93
---


```
