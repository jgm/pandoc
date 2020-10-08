```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Goossens, Mittelbach, and Samarin 1994)

Goossens, Michel, Frank Mittelbach, and Alexander Samarin. 1994. *The
LaTeX Companion*. 1st ed. Reading, Mass.: Addison-Wesley.


Formatted with pandoc and apa.csl, 2013-10-23:

(Goossens, Mittelbach, & Samarin, 1994)

Goossens, M., Mittelbach, F., & Samarin, A. (1994). *The LaTeX
companion* (1st ed.). Reading, Mass.: Addison-Wesley.


}

@Book{companion,
  author       = {Goossens, Michel and Mittelbach, Frank and Samarin, Alexander},
  title        = {The {LaTeX} Companion},
  date         = 1994,
  edition      = 1,
  publisher    = {Addison-Wesley},
  location     = {Reading, Mass.},
  pagetotal    = 528,
  hyphenation  = {american},
  sorttitle    = {LaTeX Companion},
  indextitle   = {LaTeX Companion, The},
  shorttitle   = {LaTeX Companion},
  annotation   = {A book with three authors. Note the formatting of the author
                  list. By default, only the first name is reversed in the
                  bibliography},
}

^D
---
nocite: "[@*]"
references:
- annote: A book with three authors. Note the formatting of the author
    list. By default, only the first name is reversed in the
    bibliography
  author:
  - family: Goossens
    given: Michel
  - family: Mittelbach
    given: Frank
  - family: Samarin
    given: Alexander
  edition: 1
  id: companion
  issued: 1994
  language: en-US
  number-of-pages: 528
  publisher: Addison-Wesley
  publisher-place: Reading, Mass.
  title: The LaTeX companion
  title-short: LaTeX companion
  type: book
---


```
