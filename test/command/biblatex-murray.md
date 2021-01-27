```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Hostetler et al. 1998)

Hostetler, Michael J., Julia E. Wingate, Chuan-Jian Zhong, Jay E.
Harris, Richard W. Vachet, Michael R. Clark, J. David Londono, et al.
1998. “Alkanethiolate Gold Cluster Molecules with Core Diameters from
1.5 to 5.2 nm: Core and Monolayer Properties as a Function of Core
Size.” *Langmuir* 14 (1): 17–30.


Formatted with pandoc and apa.csl, 2013-10-23:

(Hostetler et al., 1998)

Hostetler, M. J., Wingate, J. E., Zhong, C.-J., Harris, J. E., Vachet,
R. W., Clark, M. R., … Murray, R. W. (1998). Alkanethiolate gold cluster
molecules with core diameters from 1.5 to 5.2 nm: Core and monolayer
properties as a function of core size. *Langmuir*, *14*(1), 17–30.


}

@Article{murray,
  author       = {Hostetler, Michael J. and Wingate, Julia E. and Zhong,
                  Chuan-Jian and Harris, Jay E. and Vachet, Richard W. and
                  Clark, Michael R.  and Londono, J. David and Green, Stephen
                  J. and Stokes, Jennifer J.  and Wignall, George D. and Glish,
                  Gary L. and Porter, Marc D.  and Evans, Neal D. and Murray,
                  Royce W.},
  title        = {Alkanethiolate gold cluster molecules with core diameters from
                  1.5 to 5.2~{nm}},
  journaltitle = {Langmuir},
  date         = 1998,
  subtitle     = {Core and monolayer properties as a function of core size},
  volume       = 14,
  number       = 1,
  pages        = {17-30},
  hyphenation  = {american},
  indextitle   = {Alkanethiolate gold cluster molecules},
  shorttitle   = {Alkanethiolate gold cluster molecules},
  annotation   = {An article entry with \arabic{author} authors. By
                  default, long author and editor lists are automatically
                  truncated. This is configurable},
}

^D
---
nocite: "[@*]"
references:
- annote: An article entry with `\arabic{author}`{=latex} authors. By
    default, long author and editor lists are automatically truncated.
    This is configurable
  author:
  - family: Hostetler
    given: Michael J.
  - family: Wingate
    given: Julia E.
  - family: Zhong
    given: Chuan-Jian
  - family: Harris
    given: Jay E.
  - family: Vachet
    given: Richard W.
  - family: Clark
    given: Michael R.
  - family: Londono
    given: J. David
  - family: Green
    given: Stephen J.
  - family: Stokes
    given: Jennifer J.
  - family: Wignall
    given: George D.
  - family: Glish
    given: Gary L.
  - family: Porter
    given: Marc D.
  - family: Evans
    given: Neal D.
  - family: Murray
    given: Royce W.
  container-title: Langmuir
  id: murray
  issue: 1
  issued: 1998
  language: en-US
  page: 17-30
  title: "Alkanethiolate gold cluster molecules with core diameters from
    1.5 to 5.2 [nm]{.nocase}: Core and monolayer properties as a
    function of core size"
  title-short: Alkanethiolate gold cluster molecules
  type: article-journal
  volume: 14
---


```
