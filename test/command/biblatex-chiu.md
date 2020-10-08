```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Chiu and Chow 1978)

Chiu, Willy W., and We Min Chow. 1978. “A Hybrid Hierarchical Model of a
Multiple Virtual Storage (MVS) Operating System.” Research report
RC-6947. IBM.


Formatted with pandoc and apa.csl, 2013-10-23:

(Chiu & Chow, 1978)

Chiu, W. W., & Chow, W. M. (1978). *A hybrid hierarchical model of a
multiple virtual storage (MVS) operating system* (research report No.
RC-6947). IBM.


NOTES:

- biblio2yaml
	- "MVS", when not wrapped in {}, gives "mVS", which is probably never intended, or useful (latex converts the whole word to lowercase if unprotected ("MVS" -> "mvs"))

}

@Report{chiu,
  author       = {Chiu, Willy W. and Chow, We Min},
  title        = {A Hybrid Hierarchical Model of a Multiple Virtual Storage
                  ({MVS}) Operating System},
  type         = {resreport},
  institution  = {IBM},
  date         = 1978,
  number       = {RC-6947},
  hyphenation  = {american},
  sorttitle    = {Hybrid Hierarchical Model of a Multiple Virtual Storage (MVS)
                  Operating System},
  indextitle   = {Hybrid Hierarchical Model, A},
  annotation   = {This is a report entry for a research report. Note
                  the format of the type field in the database file
                  which uses a localization key. The number of the report is
                  given in the number field. Also note the
                  sorttitle and indextitle fields},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a report entry for a research report. Note the format
    of the type field in the database file which uses a localization
    key. The number of the report is given in the number field. Also
    note the sorttitle and indextitle fields
  author:
  - family: Chiu
    given: Willy W.
  - family: Chow
    given: We Min
  genre: research report
  id: chiu
  issued: 1978
  language: en-US
  number: RC-6947
  publisher: IBM
  title: A hybrid hierarchical model of a multiple virtual storage (MVS)
    operating system
  type: report
---


```
