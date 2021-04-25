```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Jaffé 1885–1888)

Jaffé, Philipp, ed. 1885–1888. *Regesta Pontificum Romanorum ab condita
ecclesia ad annum post Christum natum \<span
Style="font-variant:small-caps;"\>mcxcviii\</span\>*. 2nd ed. 2.
Leipzig.


Formatted with pandoc and apa.csl, 2013-10-23:

(Jaffé, 1885–1888)

Jaffé, P. (Ed.). (1885–1888). *Regesta Pontificum Romanorum ab condita
ecclesia ad annum post Christum natum \<span
style="font-variant:small-caps;"\>mcxcviii\</span\>* (2nd ed., 1-2).
Leipzig.


NOTES:

- biblatex conversion:
	- hyphenation = {latin}
- citeproc:
	- "vols." is missing
		- works in Zotero
		- This does not show up in the tests from the citeproc test suite that currently fail.
	- "\<span ...\> needs to be fixed.
		- maybe add markdown syntax ^^small caps^^ ?
		- in pandoc "plain" output, small caps could be converted to uppercase chars: "MCXCVIII" would definitely look better here.

}

@Collection{jaffe,
  editor       = {Jaff{\'e}, Philipp},
  title        = {Regesta Pontificum Romanorum ab condita ecclesia ad annum post
                  Christum natum \textsc{mcxcviii}},
  date         = {1885/1888},
  editora      = {Loewenfeld, Samuel and Kaltenbrunner, Ferdinand and Ewald,
                  Paul},
  edition      = 2,
  volumes      = 2,
  location     = {Leipzig},
  editoratype  = {redactor},
  indextitle   = {Regesta Pontificum Romanorum},
  shorttitle   = {Regesta Pontificum Romanorum},
  annotation   = {A collection entry with edition and
                  volumes fields. Note the editora and
                  editoratype fields},
  hyphenation  = {latin},
}

^D
---
nocite: "[@*]"
references:
- annote: A collection entry with edition and volumes fields. Note the
    editora and editoratype fields
  edition: 2
  editor:
  - family: Jaffé
    given: Philipp
  id: jaffe
  issued: 1885/1888
  language: la
  number-of-volumes: 2
  publisher-place: Leipzig
  title: Regesta Pontificum Romanorum ab condita ecclesia ad annum post
    Christum natum [mcxcviii]{.smallcaps}
  title-short: Regesta Pontificum Romanorum
  type: book
---


```
