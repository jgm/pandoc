```
% pandoc -f biblatex -t markdown -s
@comment{excerpt from
http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

@periodical{jcg,
	Annotation = {This is a periodical entry with an issn field.},
	Issn = {0097-8493},
	Issuetitle = {Semantic {3D} Media and Content},
	Number = 4,
	Title = {Computers and Graphics},
	Volume = 35,
	Year = 2011}

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
