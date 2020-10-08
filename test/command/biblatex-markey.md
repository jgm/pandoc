```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Markey 2005)

Markey, Nicolas. 2005. “Tame the BeaST: The B to X of BibTeX” (version
1.3). October 16.
<http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf>.


Formatted with pandoc and apa.csl, 2013-10-23:

(Markey, 2005)

Markey, N. (2005, October 16). Tame the BeaST: The B to X of BibTeX.
Retrieved October 01, 2006, from
<http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf>


}

@Online{markey,
  author       = {Markey, Nicolas},
  title        = {Tame the {BeaST}},
  date         = {2005-10-16},
  url          =
                  {http://tug.ctan.org/tex-archive/info/bibtex/tamethebeast/ttb_en.pdf},
  subtitle     = {The {B} to {X} of {BibTeX}},
  version      = {1.3},
  urldate      = {2006-10-01},
  hyphenation  = {american},
  sorttitle    = {Tame the Beast},
  annotation   = {An online entry for a tutorial. Note the format of
                  the date field (yyyy-mm-dd) in the database
                  file.},
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
---


```
