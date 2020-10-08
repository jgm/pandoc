```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Yoon et al. 2006)

Yoon, Myeong S., Dowook Ryu, Jeongryul Kim, and Kyo Han Ahn. 2006.
“Palladium Pincer Complexes with Reduced Bond Angle Strain: Efficient
Catalysts for the Heck Reaction.” *Organometallics* 25 (10): 2409–2411.


Formatted with pandoc and apa.csl, 2013-10-23:

(Yoon, Ryu, Kim, & Ahn, 2006)

Yoon, M. S., Ryu, D., Kim, J., & Ahn, K. H. (2006). Palladium pincer
complexes with reduced bond angle strain: Efficient catalysts for the
Heck reaction. *Organometallics*, *25*(10), 2409–2411.


}

@Article{yoon,
  author       = {Yoon, Myeong S. and Ryu, Dowook and Kim, Jeongryul and Ahn,
                  Kyo Han},
  title        = {Palladium pincer complexes with reduced bond angle strain:
                  efficient catalysts for the {Heck} reaction},
  journaltitle = {Organometallics},
  date         = 2006,
  volume       = 25,
  number       = 10,
  pages        = {2409-2411},
  indextitle   = {Palladium pincer complexes},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Yoon
    given: Myeong S.
  - family: Ryu
    given: Dowook
  - family: Kim
    given: Jeongryul
  - family: Ahn
    given: Kyo Han
  container-title: Organometallics
  id: yoon
  issue: 10
  issued: 2006
  page: 2409-2411
  title: "Palladium pincer complexes with reduced bond angle strain:
    Efficient catalysts for the Heck reaction"
  title-short: Palladium pincer complexes with reduced bond angle strain
  type: article-journal
  volume: 25
---


```
