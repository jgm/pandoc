```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Preece 2003)

Preece, Warren E., ed. 2003. *The New EncyclopæDia Britannica*. 15th ed.
32. Chicago, Ill.: Encyclopædia Britannica.


Formatted with pandoc and apa.csl, 2013-10-23:

(Preece, 2003)

Preece, W. E. (Ed.). (2003). *The new encyclopædia Britannica* (15th
ed., 1-32). Chicago, Ill.: Encyclopædia Britannica.


NOTES:

- biblio2yaml
	- spurious <span> in Encyclopædia
	- options = {useeditor=false} has no equivalent in CSL, so citing and alphabetizing by title even though there is an editor does not seem to be possible

- citeproc
	- incorrect camel case: "EncyclopæDia"
	- term "vols." missing

}

@Collection{britannica,
  editor       = {Preece, Warren E.},
  title        = {The New Encyclop{\ae}dia {Britannica}},
  date         = 2003,
  edition      = 15,
  volumes      = 32,
  publisher    = {Encyclop{\ae}dia Britannica},
  location     = {Chicago, Ill.},
  options      = {useeditor=false},
  label        = {EB},
  hyphenation  = {british},
  sorttitle    = {Encyclop{\ae}dia Britannica},
  indextitle   = {Encyclop{\ae}dia Britannica, The New},
  shorttitle   = {Encyclop{\ae}dia {Britannica}},
  annotation   = {This is a collection entry for an encyclopedia. Note
                  the useeditor option in the options field as
                  well as the sorttitle field. We want this entry to be
                  cited and alphabetized by title even though there is an
                  editor. In addition to that, we want the title to be
                  alphabetized under \enquote*{E} rather than \enquote*{T}. Also
                  note the label field which is provided for
                  author-year citation styles},
}

^D
---
nocite: "[@*]"
references:
- annote: This is a collection entry for an encyclopedia. Note the
    useeditor option in the options field as well as the sorttitle
    field. We want this entry to be cited and alphabetized by title even
    though there is an editor. In addition to that, we want the title to
    be alphabetized under 'E' rather than 'T'. Also note the label field
    which is provided for author-year citation styles
  edition: 15
  editor:
  - family: Preece
    given: Warren E.
  id: britannica
  issued: 2003
  language: en-GB
  number-of-volumes: 32
  publisher: Encyclopædia Britannica
  publisher-place: Chicago, Ill.
  title: The new encyclopædia Britannica
  title-short: Encyclopædia Britannica
  type: book
---


```
