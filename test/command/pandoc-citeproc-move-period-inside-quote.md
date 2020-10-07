```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-fullnote-bibliography.csl'
references:
- DOI: 10.1038/171737a0
  URL: 'http://www.nature.com/nature/journal/v171/n4356/abs/171737a0.html'
  accessed:
    day: 17
    month: 6
    year: 2008
  author:
  - family: Watson
    given: J. D.
  - family: Crick
    given: F. H. C.
  container-title: Nature
  custom4: custom4
  id: WatsonCrick1953
  issue: 4356
  issued:
    date-parts:
    - - 1953
      - 4
      - 25
  language: 'en-US'
  note: this is a note
  original-date:
    year: 1951
  page: '737-738'
  title: 'Molecular structure of nucleic acids: a structure for
    deoxyribose nucleic acid'
  title-short: Molecular structure of nucleic acids
  type: 'article-journal'
  volume: 171
suppress-bibliography: true
---

Here is a "test citation" [@WatsonCrick1953].

Here is a test citation [@WatsonCrick1953].
^D
Here is a "test citation."[^1]

Here is a test citation.[^2]

[^1]: J. D. Watson and F. H. C. Crick, "Molecular Structure of Nucleic
    Acids: A Structure for Deoxyribose Nucleic Acid," *Nature* 171, no.
    4356 (April 25, 1953): 737--38, <https://doi.org/10.1038/171737a0>.

[^2]: Watson and Crick.
```
