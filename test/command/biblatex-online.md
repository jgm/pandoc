```
% pandoc -f biblatex -t markdown -s
@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

@online{markey,
	Annotation = {An online entry for a tutorial. Note the format of the date field (yyyy-mm-dd) in the database file.},
	Author = {Markey, Nicolas},
	Date = {2005-10-16},
	Hyphenation = {american},
	Sorttitle = {Tame the Beast},
	Subtitle = {The {B} to {X} of {BibTeX}},
	Title = {Tame the {BeaST}},
	Url = {http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf},
	Urldate = {2006-10-01},
	Version = {1.3},
}

@online{CTAN,
	Annotation = {This is an online entry. The \textsc{url}, which is given in the url field, is transformed into a clickable link if hyperref support has been enabled. Note the format of the urldate field (yyyy-mm-dd) in the database file. Also note the label field which may be used as a fallback by citation styles which need an author and\slash or a year},
	Date = 2006,
	Hyphenation = {american},
	Label = {CTAN},
	Subtitle = {The {Comprehensive TeX Archive Network}},
	Title = {{CTAN}},
	Url = {http://www.ctan.org},
	Urldate = {2006-10-01},
}

^D
---
nocite: "[@*]"
references:
- accessed: 2006-10-01
  annote: An online entry for a tutorial. Note the format of the date
    field (yyyy-mm-dd) in the database file.
  author:
  - family: Markey
    given: Nicolas
  id: markey
  issued: 2005-10-16
  language: en-US
  title: "Tame the BeaST: The B to X of BibTeX"
  title-short: Tame the BeaST
  type: webpage
  url: "http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf"
  version: 1.3
- accessed: 2006-10-01
  annote: This is an online entry. The [url]{.smallcaps}, which is given
    in the url field, is transformed into a clickable link if hyperref
    support has been enabled. Note the format of the urldate field
    (yyyy-mm-dd) in the database file. Also note the label field which
    may be used as a fallback by citation styles which need an author
    and/or a year
  id: CTAN
  issued: 2006
  language: en-US
  title: "CTAN: The Comprehensive TeX Archive Network"
  title-short: CTAN
  type: webpage
  url: "http://www.ctan.org"
---


```
