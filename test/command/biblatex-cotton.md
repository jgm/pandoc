```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Cotton et al. 1999)

Cotton, Frank Albert, Geoffrey Wilkinson, Carlos A. Murillio, and
Manfred Bochmann. 1999. *Advanced Inorganic Chemistry*. 6th ed.
Chichester: Wiley.


Formatted with pandoc and apa.csl, 2013-10-23:

(Cotton, Wilkinson, Murillio, & Bochmann, 1999)

Cotton, F. A., Wilkinson, G., Murillio, C. A., & Bochmann, M. (1999).
*Advanced inorganic chemistry* (6th ed.). Chichester: Wiley.


}

@Book{cotton,
  author       = {Cotton, Frank Albert and Wilkinson, Geoffrey and Murillio,
                  Carlos A. and Bochmann, Manfred},
  title        = {Advanced inorganic chemistry},
  date         = 1999,
  edition      = 6,
  publisher    = {Wiley},
  location     = {Chichester},
  hyphenation  = {british},
  annotation   = {A book entry with \arabic{author} authors and an
                  edition field. By default, long author and
                  editor lists are automatically truncated. This is
                  configurable},
}

^D
---
nocite: "[@*]"
references:
- annote: A book entry with `\arabic{author}`{=latex} authors and an
    edition field. By default, long author and editor lists are
    automatically truncated. This is configurable
  author:
  - family: Cotton
    given: Frank Albert
  - family: Wilkinson
    given: Geoffrey
  - family: Murillio
    given: Carlos A.
  - family: Bochmann
    given: Manfred
  edition: 6
  id: cotton
  issued: 1999
  language: en-GB
  publisher: Wiley
  publisher-place: Chichester
  title: Advanced inorganic chemistry
  type: book
---


```
