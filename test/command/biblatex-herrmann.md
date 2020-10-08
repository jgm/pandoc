```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Herrmann et al. 2006)

Herrmann, Wolfgang A., Karl Öfele, Sabine K. Schneider, Eberhardt
Herdtweck, and Stephan D. Hoffmann. 2006. “A Carbocyclic Carbene as an
Efficient Catalyst Ligand for C–C Coupling Reactions.” *Angew. Chem.
Int. Ed.* 45 (23): 3859–3862.


Formatted with pandoc and apa.csl, 2013-10-23:

(Herrmann, Öfele, Schneider, Herdtweck, & Hoffmann, 2006)

Herrmann, W. A., Öfele, K., Schneider, S. K., Herdtweck, E., & Hoffmann,
S. D. (2006). A carbocyclic carbene as an efficient catalyst ligand for
C–C coupling reactions. *Angew. Chem. Int. Ed.*, *45*(23), 3859–3862.


}

@string{ anch-ie = {Angew.~Chem. Int.~Ed.} }

@Article{herrmann,
  author       = {Herrmann, Wolfgang A. and {\"O}fele, Karl and Schneider,
                  Sabine K.  and Herdtweck, Eberhardt and Hoffmann, Stephan D.},
  title        = {A carbocyclic carbene as an efficient catalyst ligand for {C--C}
                  coupling reactions},
  journaltitle = anch-ie,
  date         = 2006,
  volume       = 45,
  number       = 23,
  pages        = {3859-3862},
  indextitle   = {Carbocyclic carbene as an efficient catalyst, A},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Herrmann
    given: Wolfgang A.
  - family: Öfele
    given: Karl
  - family: Schneider
    given: Sabine K.
  - family: Herdtweck
    given: Eberhardt
  - family: Hoffmann
    given: Stephan D.
  container-title: Angew. Chem. Int. Ed.
  id: herrmann
  issue: 23
  issued: 2006
  page: 3859-3862
  title: A carbocyclic carbene as an efficient catalyst ligand for C--C
    coupling reactions
  type: article-journal
  volume: 45
---


```
