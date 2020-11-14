```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: Doe
    given: A.
  id: doe
  issued:
    date-parts:
    - - 2000
  title: Title
  type: book
- author:
  - family: Doe
    given: A.
  - family: Poe
    given: A.
  id: doepoe
  issued:
    date-parts:
    - - 2000
  title: Title
  type: book
- editor:
  - family: Doe
    given: A.
  id: 'doe-ed'
  issued:
    date-parts:
    - - 2000
  title: Title
  type: book
- author:
  - family: Doe
    given: A.
  - family: Loe
    given: A.
  - family: Toe
    given: A.
  id: doeloetoe
  issued:
    date-parts:
    - - 2000
  title: Title
  type: book
---

Foo [@doe]. Bar [@doepoe]. Foo [@doe-ed]. Bar [@doeloetoe].

Expected output:

> Doe, A. 2000a. Title.
>
> ---------, ed. 2000b. Title.
>
> Doe, A., A. Loe, and A. Toe. 2000. Title.
>
> Doe, A., and A. Poe. 2000. Title.

(See CMoS, 16e, 15.16, "Single author versus several authors---reference
list order": "Successive entries by two or more authors in which only
the first author's name is the same are alphabetized according to the
coauthors' last names (regardless of how many coauthors there are)." and
15.18, "The 3-em dash with edited, translated, or compiled works": "The
chronological order is maintained, regardless of the added abbreviation.
\[ed., trans., comp., or whatever\]"

References {#references .unnumbered}
==========
^D
Foo (Doe 2000a). Bar (Doe and Poe 2000). Foo (Doe 2000b). Bar (Doe, Loe,
and Toe 2000).

Expected output:

> Doe, A. 2000a. Title.
>
> ---------, ed. 2000b. Title.
>
> Doe, A., A. Loe, and A. Toe. 2000. Title.
>
> Doe, A., and A. Poe. 2000. Title.

(See CMoS, 16e, 15.16, "Single author versus several authors---reference
list order": "Successive entries by two or more authors in which only
the first author's name is the same are alphabetized according to the
coauthors' last names (regardless of how many coauthors there are)." and
15.18, "The 3-em dash with edited, translated, or compiled works": "The
chronological order is maintained, regardless of the added abbreviation.
\[ed., trans., comp., or whatever\]"

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-doe .csl-entry}
Doe, A. 2000a. *Title*.
:::

::: {#ref-doe-ed .csl-entry}
---------, ed. 2000b. *Title*.
:::

::: {#ref-doeloetoe .csl-entry}
Doe, A., A. Loe, and A. Toe. 2000. *Title*.
:::

::: {#ref-doepoe .csl-entry}
Doe, A., and A. Poe. 2000. *Title*.
:::
:::
```
