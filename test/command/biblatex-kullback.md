```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Kullback 1959)

Kullback, Solomon. 1959. *Information Theory and Statistics*. New York:
John Wiley & Sons.


Formatted with pandoc and apa.csl, 2013-10-23:

(Kullback, 1959)

Kullback, S. (1959). *Information theory and statistics*. New York: John
Wiley & Sons.


}

@Book{kullback,
  author       = {Kullback, Solomon},
  title        = {Information Theory and Statistics},
  year         = 1959,
  publisher    = {John Wiley \& Sons},
  location     = {New York},
  hyphenation  = {american},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Kullback
    given: Solomon
  id: kullback
  issued: 1959
  language: en-US
  publisher: John Wiley & Sons
  publisher-place: New York
  title: Information theory and statistics
  type: book
---


```
