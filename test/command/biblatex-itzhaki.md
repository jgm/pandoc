```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Itzhaki 1996)

Itzhaki, Nissan. 1996. “Some Remarks on ’t Hooft’s S-matrix for Black
Holes” (version 1). March 11.


Formatted with pandoc and apa.csl, 2013-10-23:

(Itzhaki, 1996)

Itzhaki, N. (1996, March 11). Some remarks on ’t Hooft’s S-matrix for
black holes.


NOTES:

- biblio2yaml:
	- eprinttype = {arxiv}, eprint = {hep-th/9603067}, 
	  should be converted to a url: http://arxiv.org/abs/hep-th/9603067
	  (prefix http://arxiv.org/abs/ seems to work for all arxiv material)

- citeproc:
	- obtaining correct case of "'t Hooft's" in title is possible but awkward:
	  '{t Hooft's} works; {'t Hooft}'s or '{t Hooft}'s do not

}

@Online{itzhaki,
  author       = {Itzhaki, Nissan},
  title        = {Some remarks on '{t Hooft's} {S}-matrix for black holes},
  date         = {1996-03-11},
  version      = 1,
  hyphenation  = {american},
  eprinttype   = {arxiv},
  eprint       = {hep-th/9603067},
  annotation   = {An online reference from arXiv. Note the
                  eprint and eprinttype fields. Also note that
                  the arXiv reference is transformed into a clickable link if
                  hyperref support has been enabled},
  abstract     = {We discuss the limitations of 't Hooft's proposal for the
                  black hole S-matrix. We find that the validity of the S-matrix
                  implies violation of the semi-classical approximation at
                  scales large compared to the Planck scale. We also show that
                  the effect of the centrifugal barrier on the S-matrix is
                  crucial even for large transverse distances.},
}

^D
---
nocite: "[@*]"
references:
- abstract: We discuss the limitations of 't Hooft's proposal for the
    black hole S-matrix. We find that the validity of the S-matrix
    implies violation of the semi-classical approximation at scales
    large compared to the Planck scale. We also show that the effect of
    the centrifugal barrier on the S-matrix is crucial even for large
    transverse distances.
  annote: An online reference from arXiv. Note the eprint and eprinttype
    fields. Also note that the arXiv reference is transformed into a
    clickable link if hyperref support has been enabled
  author:
  - family: Itzhaki
    given: Nissan
  id: itzhaki
  issued: 1996-03-11
  language: en-US
  title: Some remarks on '[t Hooft's]{.nocase} S-matrix for black holes
  type: webpage
  url: "https://arxiv.org/abs/hep-th/9603067"
  version: 1
---


```
