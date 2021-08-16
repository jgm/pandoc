```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Sigfridsson and Ryde 1998)

Sigfridsson, Emma, and Ulf Ryde. 1998. “Comparison of Methods for
Deriving Atomic Charges from the Electrostatic Potential and Moments.”
*Journal of Computational Chemistry* 19 (4): 377–395.
doi:[10.1002/(SICI)1096-987X(199803)19:4\<377::AID-JCC1\>3.0.CO;2-P](https://doi.org/10.1002/(SICI)1096-987X(199803)19:4<377::AID-JCC1>3.0.CO;2-P "10.1002/(SICI)1096-987X(199803)19:4<377::AID-JCC1>3.0.CO;2-P").


Formatted with pandoc and apa.csl, 2013-10-23:

(Sigfridsson & Ryde, 1998)

Sigfridsson, E., & Ryde, U. (1998). Comparison of methods for deriving
atomic charges from the electrostatic potential and moments. *Journal of
Computational Chemistry*, *19*(4), 377–395.
doi:[10.1002/(SICI)1096-987X(199803)19:4\<377::AID-JCC1\>3.0.CO;2-P](https://doi.org/10.1002/(SICI)1096-987X(199803)19:4<377::AID-JCC1>3.0.CO;2-P "10.1002/(SICI)1096-987X(199803)19:4<377::AID-JCC1>3.0.CO;2-P")


NOTES:

- biblio2xaml
	- the string "doi:" should not appear as part of the content of the "doi" field 

}

@Article{sigfridsson,
  author       = {Sigfridsson, Emma and Ryde, Ulf},
  title        = {Comparison of methods for deriving atomic charges from the
                  electrostatic potential and moments},
  journaltitle = {Journal of Computational Chemistry},
  date         = 1998,
  volume       = 19,
  number       = 4,
  pages        = {377-395},
  doi          = {10.1002/(SICI)1096-987X(199803)19:4<377::AID-JCC1>3.0.CO;2-P},
  hyphenation  = {american},
  indextitle   = {Methods for deriving atomic charges},
  annotation   = {An article entry with volume,
                  number, and doi fields. Note that the
                  \textsc{doi} is transformed into a clickable link if
                  hyperref support has been enabled},
  abstract     = {Four methods for deriving partial atomic charges from the
                  quantum chemical electrostatic potential (CHELP, CHELPG,
                  Merz-Kollman, and RESP) have been compared and critically
                  evaluated. It is shown that charges strongly depend on how and
                  where the potential points are selected. Two alternative
                  methods are suggested to avoid the arbitrariness in the
                  point-selection schemes and van der Waals exclusion radii:
                  CHELP-BOW, which also estimates the charges from the
                  electrostatic potential, but with potential points that are
                  Boltzmann-weighted after their occurrence in actual
                  simulations using the energy function of the program in which
                  the charges will be used, and CHELMO, which estimates the
                  charges directly from the electrostatic multipole
                  moments. Different criteria for the quality of the charges are
                  discussed.},
}

^D
---
nocite: "[@*]"
references:
- abstract: "Four methods for deriving partial atomic charges from the
    quantum chemical electrostatic potential (CHELP, CHELPG,
    Merz-Kollman, and RESP) have been compared and critically evaluated.
    It is shown that charges strongly depend on how and where the
    potential points are selected. Two alternative methods are suggested
    to avoid the arbitrariness in the point-selection schemes and van
    der Waals exclusion radii: CHELP-BOW, which also estimates the
    charges from the electrostatic potential, but with potential points
    that are Boltzmann-weighted after their occurrence in actual
    simulations using the energy function of the program in which the
    charges will be used, and CHELMO, which estimates the charges
    directly from the electrostatic multipole moments. Different
    criteria for the quality of the charges are discussed."
  annote: An article entry with volume, number, and doi fields. Note
    that the [doi]{.smallcaps} is transformed into a clickable link if
    hyperref support has been enabled
  author:
  - family: Sigfridsson
    given: Emma
  - family: Ryde
    given: Ulf
  container-title: Journal of Computational Chemistry
  doi: "10.1002/(SICI)1096-987X(199803)19:4\\<377::AID-JCC1>3.0.CO;2-P"
  id: sigfridsson
  issue: 4
  issued: 1998
  language: en-US
  page: 377-395
  title: Comparison of methods for deriving atomic charges from the
    electrostatic potential and moments
  type: article-journal
  volume: 19
---


```
