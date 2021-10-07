```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Gillies 1933)

Gillies, Alexander. 1933. “Herder and the Preparation of Goethe’s Idea
of World Literature.” *Publications of the English Goethe Society, New
Series* 9: 46–67.


Formatted with pandoc and apa.csl, 2013-10-23:

(Gillies, 1933)

Gillies, A. (1933). Herder and the preparation of Goethe’s idea of world
literature. *Publications of the English Goethe Society, new series*,
*9*, 46–67.


NOTES:

- biblio2yaml
	- "new series" is not pretty, but it’s the best we can do at the moment, given the limitations of CSL.

}

@Article{gillies,
  author       = {Gillies, Alexander},
  title        = {Herder and the Preparation of {Goethe}'s Idea of World
                  Literature},
  journaltitle = {Publications of the English Goethe Society},
  date         = 1933,
  series       = {newseries},
  volume       = 9,
  pages        = {46-67},
  hyphenation  = {british},
  annotation   = {An article entry with a series and a
                  volume field. Note that format of the series
                  field in the database file},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry with a series and a volume field. Note that
    format of the series field in the database file
  author:
  - family: Gillies
    given: Alexander
  collection-title: New series
  container-title: Publications of the English Goethe Society
  id: gillies
  issued: 1933
  language: en-GB
  page: 46-67
  title: Herder and the preparation of Goethe's idea of world literature
  type: article-journal
  volume: 9
---


```
