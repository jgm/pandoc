```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2015-03-08:

(Coleridge 1983)

Coleridge, Samuel Taylor. 1983. *The Collected Works of Samuel Taylor
Coleridge*. Edited by Kathleen Coburn, James Engell, and W. Jackson
Bate. Vol. 7.2. Bollingen Series 75. London: Routledge and Kegan Paul.

Formatted with pandoc and apa.csl, 2015-03-08:

(Coleridge, 1983)

Coleridge, S. T. (1983). *The collected works of Samuel Taylor
Coleridge*. (K. Coburn, J. Engell, & W. J. Bate, Eds.) (Vol. 7.2).
London: Routledge and Kegan Paul.

NOTES:

- volume-title currently not implemented by chicago-author-date.csl and apa.csl.

}

@Book{coleridge,
  author       = {Coleridge, Samuel Taylor},
  title        = {Biographia literaria, or {Biographical} sketches of my literary
                  life and opinions},
  date         = 1983,
  editor       = {Coburn, Kathleen and Engell, James and Bate, W. Jackson},
  maintitle    = {The collected works of {Samuel Taylor Coleridge}},
  volume       = 7,
  part         = 2,
  series       = {Bollingen Series},
  number       = 75,
  publisher    = {Routledge {and} Kegan Paul},
  location     = {London},
  hyphenation  = {british},
  indextitle   = {Biographia literaria},
  shorttitle   = {Biographia literaria},
  annotation   = {One (partial) volume of a multivolume book. This is a
                  book entry with a volume and a part
                  field which explicitly refers to the second (physical) part of
                  the seventh (logical) volume. Also note the series
                  and number fields},
}

^D
---
nocite: "[@*]"
references:
- annote: One (partial) volume of a multivolume book. This is a book
    entry with a volume and a part field which explicitly refers to the
    second (physical) part of the seventh (logical) volume. Also note
    the series and number fields
  author:
  - family: Coleridge
    given: Samuel Taylor
  collection-number: 75
  collection-title: Bollingen series
  editor:
  - family: Coburn
    given: Kathleen
  - family: Engell
    given: James
  - family: Bate
    given: W. Jackson
  id: coleridge
  issued: 1983
  language: en-GB
  publisher: Routledge and Kegan Paul
  publisher-place: London
  title: The collected works of Samuel Taylor Coleridge
  type: book
  volume: 7.2
  volume-title: Biographia literaria, or Biographical sketches of my
    literary life and opinions
---


```
