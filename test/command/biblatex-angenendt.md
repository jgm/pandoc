```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Angenendt 2002)

Angenendt, Arnold. 2002. “In Honore Salvatoris – Vom Sinn und Unsinn der
Patrozinienkunde.” *Revue d’Histoire Ecclésiastique* 97: 431–456,
791–823.


Formatted with pandoc and apa.csl, 2013-10-23:

(Angenendt, 2002)

Angenendt, A. (2002). In Honore Salvatoris – Vom Sinn und Unsinn der
Patrozinienkunde. *Revue d’Histoire Ecclésiastique*, *97*, 431–456,
791–823.


}

@Article{angenendt,
  author       = {Angenendt, Arnold},
  title        = {In Honore Salvatoris~-- Vom Sinn und Unsinn der
                  Patrozinienkunde},
  journaltitle = {Revue d'Histoire Eccl{\'e}siastique},
  date         = 2002,
  volume       = 97,
  pages        = {431--456, 791--823},
  hyphenation  = {german},
  indextitle   = {In Honore Salvatoris},
  shorttitle   = {In Honore Salvatoris},
  annotation   = {A German article in a French journal. Apart from that, a
                  typical article entry. Note the indextitle
                  field},
}

^D
---
nocite: "[@*]"
references:
- annote: A German article in a French journal. Apart from that, a
    typical article entry. Note the indextitle field
  author:
  - family: Angenendt
    given: Arnold
  container-title: Revue d'Histoire Ecclésiastique
  id: angenendt
  issued: 2002
  language: de-DE
  page: 431-456, 791-823
  title: In Honore Salvatoris -- Vom Sinn und Unsinn der
    Patrozinienkunde
  title-short: In Honore Salvatoris
  type: article-journal
  volume: 97
---


```
