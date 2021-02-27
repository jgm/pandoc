```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib

Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Wassenberg and Sanders 2010)

Wassenberg, Jan, and Peter Sanders. 2010. “Faster Radix Sort via Virtual
Memory and Write-combining” (version 1). August 17.


Formatted with pandoc and apa.csl, 2013-10-23:

(Wassenberg & Sanders, 2010)

Wassenberg, J., & Sanders, P. (2010, August 17). Faster radix sort via
virtual memory and write-combining.


NOTES:

- biblio2yaml
	- "eprinttype = {arxiv}, eprintclass = {cs.DS}, eprint = {1008.2849v1}" should be used to reconstruct a Url: http://arxiv.org/abs/1008.2849v1 ("cs.DS" does not seem to be essential)

}

@Online{wassenberg,
  author       = {Wassenberg, Jan and Sanders, Peter},
  title        = {Faster Radix Sort via Virtual Memory and Write-Combining},
  date         = {2010-08-17},
  version      = 1,
  hyphenation  = {american},
  eprinttype   = {arxiv},
  eprintclass  = {cs.DS},
  eprint       = {1008.2849v1},
  annotation   = {A recent online reference from arXiv using the new
                  (April 2007 onward) identifier format. Note the
                  eprint, eprinttype, and eprintclass
                  fields. Also note that the arXiv reference is transformed into
                  a clickable link if hyperref support has been
                  enabled},
  abstract     = {Sorting algorithms are the deciding factor for the performance
                  of common operations such as removal of duplicates or database
                  sort-merge joins. This work focuses on 32-bit integer keys,
                  optionally paired with a 32-bit value. We present a fast radix
                  sorting algorithm that builds upon a microarchitecture-aware
                  variant of counting sort},
}

^D
---
nocite: "[@*]"
references:
- abstract: Sorting algorithms are the deciding factor for the
    performance of common operations such as removal of duplicates or
    database sort-merge joins. This work focuses on 32-bit integer keys,
    optionally paired with a 32-bit value. We present a fast radix
    sorting algorithm that builds upon a microarchitecture-aware variant
    of counting sort
  annote: A recent online reference from arXiv using the new (April 2007
    onward) identifier format. Note the eprint, eprinttype, and
    eprintclass fields. Also note that the arXiv reference is
    transformed into a clickable link if hyperref support has been
    enabled
  author:
  - family: Wassenberg
    given: Jan
  - family: Sanders
    given: Peter
  id: wassenberg
  issued: 2010-08-17
  language: en-US
  title: Faster radix sort via virtual memory and write-combining
  type: webpage
  url: "https://arxiv.org/abs/1008.2849v1"
  version: 1
---


```
