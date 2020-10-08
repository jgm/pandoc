```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Aksin et al. 2006)

Aksin, Özge, Hayati Türkmen, Levent Artok, Bekir Çetinkaya, Chaoying Ni,
Orhan Büyükgüngör, and Erhan Özkal. 2006. “Effect of Immobilization on
Catalytic Characteristics of Saturated Pd-N-heterocyclic Carbenes in
Mizoroki-Heck Reactions.” *J. Organomet. Chem.* 691 (13): 3027–3036.


Formatted with pandoc and apa.csl, 2013-10-23:

(Aksin et al., 2006)

Aksin, Ö., Türkmen, H., Artok, L., Çetinkaya, B., Ni, C., Büyükgüngör,
O., & Özkal, E. (2006). Effect of immobilization on catalytic
characteristics of saturated Pd-N-heterocyclic carbenes in Mizoroki-Heck
reactions. *J. Organomet. Chem.*, *691*(13), 3027–3036.


}

@string{ jomch   = {J.~Organomet. Chem.} }

@Article{aksin,
  author       = {Aks{\i}n, {\"O}zge and T{\"u}rkmen, Hayati and Artok, Levent
                  and {\c{C}}etinkaya, Bekir and Ni, Chaoying and
                  B{\"u}y{\"u}kg{\"u}ng{\"o}r, Orhan and {\"O}zkal, Erhan},
  title        = {Effect of immobilization on catalytic characteristics of
                  saturated {Pd-N}-heterocyclic carbenes in {Mizoroki-Heck}
                  reactions},
  journaltitle = jomch,
  date         = 2006,
  volume       = 691,
  number       = 13,
  pages        = {3027-3036},
  indextitle   = {Effect of immobilization on catalytic characteristics},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Aksın
    given: Özge
  - family: Türkmen
    given: Hayati
  - family: Artok
    given: Levent
  - family: Çetinkaya
    given: Bekir
  - family: Ni
    given: Chaoying
  - family: Büyükgüngör
    given: Orhan
  - family: Özkal
    given: Erhan
  container-title: J. Organomet. Chem.
  id: aksin
  issue: 13
  issued: 2006
  page: 3027-3036
  title: Effect of immobilization on catalytic characteristics of
    saturated Pd-N-heterocyclic carbenes in Mizoroki-Heck reactions
  type: article-journal
  volume: 691
---


```
