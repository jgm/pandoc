```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Kullback 1997)

Kullback, Solomon. 1997. *Information Theory and Statistics*. New York:
Dover Publications.


Formatted with pandoc and apa.csl, 2013-10-23:

(Kullback, 1997)

Kullback, S. (1997). *Information theory and statistics*. New York:
Dover Publications.


NOTES:

Formatted with chicago-author-date-TEST-20131018.csl 

(Kullback [1959] 1997)

Kullback, Solomon. (1959) 1997. *Information Theory and Statistics*. New
York: Dover Publications.

}

@Book{kullback:reprint,
  author       = {Kullback, Solomon},
  title        = {Information Theory and Statistics},
  year         = 1997,
  publisher    = {Dover Publications},
  location     = {New York},
  origyear     = 1959,
  origpublisher= {John Wiley \& Sons},
  hyphenation  = {american},
  annotation   = {A reprint of the kullback entry. Note the format of
                  origyear and origpublisher. These fields are
                  not used by the standard bibliography styles},
}

^D
---
nocite: "[@*]"
references:
- annote: A reprint of the kullback entry. Note the format of origyear
    and origpublisher. These fields are not used by the standard
    bibliography styles
  author:
  - family: Kullback
    given: Solomon
  id: "kullback:reprint"
  issued: 1997
  language: en-US
  original-date: 1959
  original-publisher: John Wiley & Sons
  publisher: Dover Publications
  publisher-place: New York
  title: Information theory and statistics
  type: book
---


```
