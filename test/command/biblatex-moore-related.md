```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Moore 1998)

Moore, Gordon E. 1998. “Cramming More Components onto Integrated
Circuits.” *Proceedings of the IEEE* 86 (1): 82–85.


Formatted with pandoc and apa.csl, 2013-10-23:

(Moore, 1998)

Moore, G. E. (1998). Cramming more components onto integrated circuits.
*Proceedings of the IEEE*, *86*(1), 82–85.


NOTES:

- "related = {moore}, relatedtype = {reprintfrom}," – no equivalent implemented in CSL

}

@Article{moore:related,
  author       = {Moore, Gordon E.},
  title        = {Cramming more components onto integrated circuits},
  journaltitle = {Proceedings of the {IEEE}},
  year         = 1998,
  volume       = 86,
  number       = 1,
  pages        = {82-85},
  hyphenation  = {american},
  related      = {moore},
  relatedtype  = {reprintfrom},
  annotation   = {A reprint of Moore's law. Note the related and
                  relatedtype fields},
}

^D
---
nocite: "[@*]"
references:
- annote: A reprint of Moore's law. Note the related and relatedtype
    fields
  author:
  - family: Moore
    given: Gordon E.
  container-title: Proceedings of the IEEE
  id: "moore:related"
  issue: 1
  issued: 1998
  language: en-US
  page: 82-85
  title: Cramming more components onto integrated circuits
  type: article-journal
  volume: 86
---


```
