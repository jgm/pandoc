```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Reese 1958)

Reese, Trevor R. 1958. “Georgia in Anglo-Spanish Diplomacy, 1736-1739.”
*William and Mary Quarterly, 3* 15: 168–190.


Formatted with pandoc and apa.csl, 2013-10-23:

(Reese, 1958)

Reese, T. R. (1958). Georgia in Anglo-Spanish diplomacy, 1736-1739.
*William and Mary Quarterly, 3*, *15*, 168–190.


NOTES:

- biblio2yaml
	- series field: still not entirely satisfactory.
	  Could we map this to some existing CSL variable, and have the CSL styles handle this? "edition", maybe ??

}

@Article{reese,
  author       = {Reese, Trevor R.},
  title        = {Georgia in {Anglo-Spanish} Diplomacy, 1736-1739},
  journaltitle = {William and Mary Quarterly},
  date         = 1958,
  series       = 3,
  volume       = 15,
  pages        = {168-190},
  hyphenation  = {american},
  annotation   = {An article entry with a series and a
                  volume field. Note the format of the series. If the
                  value of the series field is an integer, this number
                  is printed as an ordinal and the string \enquote*{series} is
                  appended automatically},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry with a series and a volume field. Note the
    format of the series. If the value of the series field is an
    integer, this number is printed as an ordinal and the string
    'series' is appended automatically
  author:
  - family: Reese
    given: Trevor R.
  collection-title: 3rd series
  container-title: William and Mary Quarterly
  id: reese
  issued: 1958
  language: en-US
  page: 168-190
  title: Georgia in Anglo-Spanish diplomacy, 1736-1739
  type: article-journal
  volume: 15
---


```
