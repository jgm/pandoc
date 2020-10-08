```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Maron 2000)

Maron, Monika. 2000. *Animal Triste*. Translated by Brigitte Goldstein.
Lincoln: University of Nebraska Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(Maron, 2000)

Maron, M. (2000). *Animal triste*. (B. Goldstein, Trans.). Lincoln:
University of Nebraska Press.


NOTES:

-   origlanguage concatenated with translator, e.g. “translated from the German by …” not possible in CSL

}

@Book{maron,
  author       = {Maron, Monika},
  title        = {Animal Triste},
  date         = 2000,
  translator   = {Brigitte Goldstein},
  origlanguage = {german},
  publisher    = {University of Nebraska Press},
  location     = {Lincoln},
  hyphenation  = {american},
  shorttitle   = {Animal Triste},
  annotation   = {An English translation of a German novel with a French title.
                  In other words: a book entry with a
                  translator field.  Note the origlanguage
                  field which is concatenated with the translator},
}

^D
---
nocite: "[@*]"
references:
- annote: "An English translation of a German novel with a French title.
    In other words: a book entry with a translator field. Note the
    origlanguage field which is concatenated with the translator"
  author:
  - family: Maron
    given: Monika
  id: maron
  issued: 2000
  language: en-US
  publisher: University of Nebraska Press
  publisher-place: Lincoln
  title: Animal triste
  title-short: Animal triste
  translator:
  - family: Goldstein
    given: Brigitte
  type: book
---


```
