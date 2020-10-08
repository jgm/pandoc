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

- related = {kullback}, relatedtype = {origpubin}, -- not possible in CSL

}

@Book{kullback:related,
  author       = {Kullback, Solomon},
  title        = {Information Theory and Statistics},
  year         = 1997,
  publisher    = {Dover Publications},
  location     = {New York},
  hyphenation  = {american},
  related      = {kullback},
  relatedtype  = {origpubin},
  annotation   = {A reprint of the kullback entry. Note the format of
                  the related and relatedtype fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A reprint of the kullback entry. Note the format of the
    related and relatedtype fields
  author:
  - family: Kullback
    given: Solomon
  id: "kullback:related"
  issued: 1997
  language: en-US
  publisher: Dover Publications
  publisher-place: New York
  title: Information theory and statistics
  type: book
---


```
