```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Kastenholz and Hünenberger 2006)

Kastenholz, M. A., and Philippe H. Hünenberger. 2006. “Computation of
Methodologyindependent Ionic Solvation Free Energies from Molecular
Simulations: I. the Electrostatic Potential in Molecular Liquids.”
*J. Chem. Phys.* 124.
doi:[10.1063/1.2172593](https://doi.org/10.1063/1.2172593 "10.1063/1.2172593").


Formatted with pandoc and apa.csl, 2013-10-23:

(Kastenholz & Hünenberger, 2006)

Kastenholz, M. A., & Hünenberger, P. H. (2006). Computation of
methodologyindependent ionic solvation free energies from molecular
simulations: I. the electrostatic potential in molecular liquids.
*J. Chem. Phys.*, *124*.
doi:[10.1063/1.2172593](https://doi.org/10.1063/1.2172593 "10.1063/1.2172593")


NOTES:

- biblio2xaml
	- fix conversion of "\hyphen”
	- the string "doi:" should not appear as part of the content of the "doi" field 

}

@string{ jchph   = {J.~Chem. Phys.} }

@Article{kastenholz,
  author       = {Kastenholz, M. A. and H{\"u}nenberger, Philippe H.},
  title        = {Computation of methodology\hyphen independent ionic solvation
                  free energies from molecular simulations},
  journaltitle = jchph,
  date         = 2006,
  subtitle     = {I. {The} electrostatic potential in molecular liquids},
  volume       = 124,
  eid          = 124106,
  doi          = {10.1063/1.2172593},
  hyphenation  = {american},
  indextitle   = {Computation of ionic solvation free energies},
  annotation   = {An article entry with an eid and a
                  doi field. Note that the \textsc{doi} is transformed
                  into a clickable link if hyperref support has been
                  enabled},
  abstract     = {The computation of ionic solvation free energies from
                  atomistic simulations is a surprisingly difficult problem that
                  has found no satisfactory solution for more than 15 years. The
                  reason is that the charging free energies evaluated from such
                  simulations are affected by very large errors. One of these is
                  related to the choice of a specific convention for summing up
                  the contributions of solvent charges to the electrostatic
                  potential in the ionic cavity, namely, on the basis of point
                  charges within entire solvent molecules (M scheme) or on the
                  basis of individual point charges (P scheme). The use of an
                  inappropriate convention may lead to a charge-independent
                  offset in the calculated potential, which depends on the
                  details of the summation scheme, on the quadrupole-moment
                  trace of the solvent molecule, and on the approximate form
                  used to represent electrostatic interactions in the
                  system. However, whether the M or P scheme (if any) represents
                  the appropriate convention is still a matter of on-going
                  debate. The goal of the present article is to settle this
                  long-standing controversy by carefully analyzing (both
                  analytically and numerically) the properties of the
                  electrostatic potential in molecular liquids (and inside
                  cavities within them).},
}

^D
---
nocite: "[@*]"
references:
- abstract: The computation of ionic solvation free energies from
    atomistic simulations is a surprisingly difficult problem that has
    found no satisfactory solution for more than 15 years. The reason is
    that the charging free energies evaluated from such simulations are
    affected by very large errors. One of these is related to the choice
    of a specific convention for summing up the contributions of solvent
    charges to the electrostatic potential in the ionic cavity, namely,
    on the basis of point charges within entire solvent molecules (M
    scheme) or on the basis of individual point charges (P scheme). The
    use of an inappropriate convention may lead to a charge-independent
    offset in the calculated potential, which depends on the details of
    the summation scheme, on the quadrupole-moment trace of the solvent
    molecule, and on the approximate form used to represent
    electrostatic interactions in the system. However, whether the M or
    P scheme (if any) represents the appropriate convention is still a
    matter of on-going debate. The goal of the present article is to
    settle this long-standing controversy by carefully analyzing (both
    analytically and numerically) the properties of the electrostatic
    potential in molecular liquids (and inside cavities within them).
  annote: An article entry with an eid and a doi field. Note that the
    [doi]{.smallcaps} is transformed into a clickable link if hyperref
    support has been enabled
  author:
  - family: Kastenholz
    given: M. A.
  - family: Hünenberger
    given: Philippe H.
  container-title: J. Chem. Phys.
  doi: 10.1063/1.2172593
  id: kastenholz
  issued: 2006
  language: en-US
  title: "Computation of methodology-independent ionic solvation free
    energies from molecular simulations: I. The electrostatic potential
    in molecular liquids"
  title-short: Computation of methodology-independent ionic solvation
    free energies from molecular simulations
  type: article-journal
  volume: 124
---


```
