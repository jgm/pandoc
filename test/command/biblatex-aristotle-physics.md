```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Aristotle 1929)

Aristotle. 1929. *Physics*. Translated by P. H. Wicksteed and F. M.
Cornford. New York: G. P. Putnam.


Formatted with pandoc and apa.csl, 2013-10-23:

(Aristotle, 1929)

Aristotle. (1929). *Physics*. (P. H. Wicksteed & F. M. Cornford,
Trans.). New York: G. P. Putnam.


}

@Book{aristotle:physics,
  author       = {Aristotle},
  title        = {Physics},
  date         = 1929,
  translator   = {Wicksteed, P. H. and Cornford, F. M.},
  publisher    = {G. P. Putnam},
  location     = {New York},
  keywords     = {primary},
  hyphenation  = {american},
  shorttitle   = {Physics},
  annotation   = {A book entry with a translator field},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry with a translator field
  author:
  - family: Aristotle
  id: "aristotle:physics"
  issued: 1929
  keyword: primary
  language: en-US
  publisher: G. P. Putnam
  publisher-place: New York
  title: Physics
  title-short: Physics
  translator:
  - family: Wicksteed
    given: P. H.
  - family: Cornford
    given: F. M.
  type: book
---


```
