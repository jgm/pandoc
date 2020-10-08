```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(“Semantic 3D Media and Content” 2011)

“Semantic 3D Media and Content.” 2011. *Computers and Graphics* 35 (4).


Formatted with pandoc and apa.csl, 2013-10-23:

(“Semantic 3D media and content,” 2011)

Semantic 3D media and content. (2011). *Computers and Graphics*,
*35*(4).


NOTES:
	- output looks OK even if indistinguishable from article without page numbers

}

@Periodical{jcg,
  title        = {Computers and Graphics},
  year         = 2011,
  issuetitle   = {Semantic {3D} Media and Content},
  volume       = 35,
  number       = 4,
  issn         = {0097-8493},
  annotation   = {This is a periodical entry with an issn
                  field.},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a periodical entry with an issn field.
  container-title: Computers and Graphics
  id: jcg
  issn: 0097-8493
  issue: 4
  issued: 2011
  title: Semantic 3D media and content
  type: article-journal
  volume: 35
---


```
