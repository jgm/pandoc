```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(*The Chicago Manual of Style: The Essential Guide for Writers, Editors,
and Publishers* 2003)

*The Chicago Manual of Style: The Essential Guide for Writers, Editors,
and Publishers*. 2003. 15th ed. Chicago, Ill.: University of Chicago
Press.


Formatted with pandoc and apa.csl, 2013-10-23:

(*Chicago manual of style*, 2003)

*The Chicago manual of style: The essential guide for writers, editors,
and publishers*. (2003) (15th ed.). Chicago, Ill.: University of Chicago
Press.


NOTES:

- chicago-author-date.csl should have as in-text citation: 
  (*Chicago Manual of Style* 2003)
  Same behaviour in Zotero; most probably a style file issue.

}

@Manual{cms,
  title        = {The {Chicago} Manual of Style},
  date         = 2003,
  subtitle     = {The Essential Guide for Writers, Editors, and Publishers},
  edition      = 15,
  publisher    = {University of Chicago Press},
  location     = {Chicago, Ill.},
  isbn         = {0-226-10403-6},
  label        = {CMS},
  hyphenation  = {american},
  sorttitle    = {Chicago Manual of Style},
  indextitle   = {Chicago Manual of Style, The},
  shorttitle   = {Chicago Manual of Style},
  annotation   = {This is a manual entry without an author or
                  editor. Note the label field in the database
                  file which is provided for author-year citation styles. Also
                  note the sorttitle and indextitle fields. By
                  default, all entries without an author or
                  editor are alphabetized by title but we want
                  this entry to be alphabetized under \enquote*{C} rather than
                  \enquote*{T}. There's also an isbn field},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a manual entry without an author or editor. Note the
    label field in the database file which is provided for author-year
    citation styles. Also note the sorttitle and indextitle fields. By
    default, all entries without an author or editor are alphabetized by
    title but we want this entry to be alphabetized under 'C' rather
    than 'T'. There's also an isbn field
  edition: 15
  id: cms
  isbn: 0-226-10403-6
  issued: 2003
  language: en-US
  publisher: University of Chicago Press
  publisher-place: Chicago, Ill.
  title: "The Chicago manual of style: The essential guide for writers,
    editors, and publishers"
  title-short: Chicago manual of style
  type: book
---


```
