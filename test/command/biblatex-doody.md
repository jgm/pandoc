```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Doody 1974) (Matuz 1990)

Doody, Terrence. 1974. “Hemingway’s Style and Jake’s Narration.” *The
Journal of Narrative Technique* 4 (3): 212–225.

Matuz, Roger, ed. 1990. *Contemporary Literary Criticism*. Vol. 61.
Detroit: Gale.


Formatted with pandoc and apa.csl, 2013-10-23:

(Doody, 1974) (Matuz, 1990)

Doody, T. (1974). Hemingway’s style and Jake’s narration. *The Journal
of Narrative Technique*, *4*(3), 212–225.

Matuz, R. (Ed.). (1990). *Contemporary literary criticism* (Vol. 61, pp.
204–208). Detroit: Gale.


NOTES

- biblio2yaml
	- contains fields “related” and “relatedstring”. In principle, these could be appended to CSL "note", if citeproc can handle citations in the bibliography ...

}

@Article{doody,
  author       = {Doody, Terrence},
  title        = {Hemingway's Style and {Jake}'s Narration},
  year         = 1974,
  volume       = 4,
  number       = 3,
  pages        = {212-225},
  hyphenation  = {american},
  related      = {matuz:doody},
  relatedstring= {\autocap{e}xcerpt in},
  journal      = {The Journal of Narrative Technique},
  annotation   = {An article entry cited as an excerpt from a
                  collection entry. Note the format of the
                  related and relatedstring fields},
}

@Collection{matuz:doody,
  editor       = {Matuz, Roger},
  title        = {Contemporary Literary Criticism},
  year         = 1990,
  volume       = 61,
  publisher    = {Gale},
  location     = {Detroit},
  pages        = {204-208},
  hyphenation  = {american},
  annotation   = {A collection entry providing the excerpt information
                  for the doody entry. Note the format of the
                  pages field},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry cited as an excerpt from a collection entry.
    Note the format of the related and relatedstring fields
  author:
  - family: Doody
    given: Terrence
  container-title: The Journal of Narrative Technique
  id: doody
  issue: 3
  issued: 1974
  language: en-US
  page: 212-225
  title: Hemingway's style and Jake's narration
  type: article-journal
  volume: 4
- annote: A collection entry providing the excerpt information for the
    doody entry. Note the format of the pages field
  editor:
  - family: Matuz
    given: Roger
  id: "matuz:doody"
  issued: 1990
  language: en-US
  page: 204-208
  publisher: Gale
  publisher-place: Detroit
  title: Contemporary literary criticism
  type: book
  volume: 61
---


```
