```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Malinowski 1972)

Malinowski, Bronisław. 1972. *Argonauts of the Western Pacific: An
Account of Native Enterprise and Adventure in the Archipelagoes of
Melanesian New Guinea*. 8th ed. London: Routledge and Kegan Paul.


Formatted with pandoc and apa.csl, 2013-10-23:

(Malinowski, 1972)

Malinowski, B. (1972). *Argonauts of the Western Pacific: An account of
native enterprise and adventure in the Archipelagoes of Melanesian New
Guinea* (8th ed.). London: Routledge and Kegan Paul.


}

@Book{malinowski,
  author       = {Malinowski, Bronis{\l}aw},
  title        = {Argonauts of the {Western Pacific}},
  date         = 1972,
  edition      = 8,
  publisher    = {Routledge {and} Kegan Paul},
  location     = {London},
  hyphenation  = {british},
  subtitle     = {An account of native enterprise and adventure in the
                  {Archipelagoes of Melanesian New Guinea}},
  shorttitle   = {Argonauts},
  annotation   = {This is a book entry. Note the format of the
                  publisher and edition fields as well as the
                  subtitle field},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a book entry. Note the format of the publisher and
    edition fields as well as the subtitle field
  author:
  - family: Malinowski
    given: Bronisław
  edition: 8
  id: malinowski
  issued: 1972
  language: en-GB
  publisher: Routledge and Kegan Paul
  publisher-place: London
  title: "Argonauts of the Western Pacific: An account of native
    enterprise and adventure in the [Archipelagoes of Melanesian New
    Guinea]{.nocase}"
  title-short: Argonauts
  type: book
---


```
