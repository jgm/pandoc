```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(“CTAN: The Comprehensive TeX Archive Network” 2006)

“CTAN: The Comprehensive TeX Archive Network.” 2006.
<http://www.ctan.org>.


Formatted with pandoc and apa.csl, 2013-10-23:

(“CTAN: The Comprehensive TeX Archive Network,” 2006)

CTAN: The Comprehensive TeX Archive Network. (2006). Retrieved October
01, 2006, from <http://www.ctan.org>


NOTES:

- biblio2yaml
	- if there is no shorttitle, but title and subtitle, the title alone should also be mapped to title-short

- citeproc
	- citeproc should use title-short (if available) instead of title for in-text citations when there is no author

}

@Online{ctan,
  title        = {{CTAN}},
  date         = 2006,
  url          = {http://www.ctan.org},
  subtitle     = {{The Comprehensive TeX Archive Network}},
  urldate      = {2006-10-01},
  label        = {CTAN},
  hyphenation  = {american},
  annotation   = {This is an online entry. The \textsc{url}, which is
                  given in the url field, is transformed into a
                  clickable link if hyperref support has been
                  enabled. Note the format of the urldate field
                  (yyyy-mm-dd) in the database file. Also note the
                  label field which may be used as a fallback by
                  citation styles which need an author and\slash or a
                  year},
}

^D
---
nocite: "[@*]"
references:
- accessed: 2006-10-01
  annote: This is an online entry. The [url]{.smallcaps}, which is given
    in the url field, is transformed into a clickable link if hyperref
    support has been enabled. Note the format of the urldate field
    (yyyy-mm-dd) in the database file. Also note the label field which
    may be used as a fallback by citation styles which need an author
    and/or a year
  id: ctan
  issued: 2006
  language: en-US
  title: "CTAN: The Comprehensive TeX Archive Network"
  title-short: CTAN
  type: webpage
  url: "http://www.ctan.org"
---


```
