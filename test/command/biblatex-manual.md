```
% pandoc -f biblatex -t markdown -s
@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib}

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
