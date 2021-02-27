```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Wilde 1899)

Wilde, Oscar. 1899. *The Importance of Being Earnest: A Trivial Comedy
for Serious People*. English and American Drama of the Nineteenth
Century. Leonard Smithers and Company.


Formatted with pandoc and apa.csl, 2013-10-23:

(Wilde, 1899)

Wilde, O. (1899). *The importance of being earnest: A trivial comedy for
serious people*. Leonard Smithers and Company.


NOTES:

- biblio2yaml
	- From "eprint = {4HIWAAAAYAAJ}, eprinttype = {googlebooks}", a url could be reconstructed, shortest form: http://books.google.com?id=4HIWAAAAYAAJ

}

@Book{wilde,
  author       = {Wilde, Oscar},
  title        = {The Importance of Being Earnest: {A} Trivial Comedy for Serious
                  People},
  year         = 1899,
  series       = {English and American drama of the Nineteenth Century},
  publisher    = {Leonard Smithers {and} Company},
  eprint       = {4HIWAAAAYAAJ},
  eprinttype   = {googlebooks},
  annotation   = {A book with eprint and eprinttype
                  fields.},
}

^D
---
nocite: "[@*]"
references:
- annote: A book with eprint and eprinttype fields.
  author:
  - family: Wilde
    given: Oscar
  collection-title: English and american drama of the nineteenth century
  id: wilde
  issued: 1899
  publisher: Leonard Smithers and Company
  title: "The importance of being earnest: A trivial comedy for serious
    people"
  title-short: The importance of being earnest
  type: book
  url: "https://books.google.com?id=4HIWAAAAYAAJ"
---


```
