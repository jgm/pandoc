```
% pandoc -f biblatex -t markdown -s
@comment{
    - bibtex and biblatex
        - expect titles in title case
        - styles use titles as is, or convert them to sentence case
        - strings wrapped {} are not converted
    - all CSL styles at <http://github.com/citation-style-language/styles>
      and <https://www.zotero.org/styles/>
        - expect titles in sentence case
        - styles use titles as is, or convert them to title case
        - except for (hardcoded) list of stop words, see
          <http://citationstyles.org/downloads/specification.html#title-case-conversion>
        - citeproc-js (MLZ only?) also recognizes a markup syntax for
          suppressing title-case changes on a range of text (see
          <https://forums.zotero.org/discussion/21991/excessive-capitalization-of-citation-titles/#Item_22>):
            - `<span class="nocase"/>lowercase</span>`
    - Proposal:
        - When converting to yaml, convert English titles to sentence case,
            - for all strings wrapped in {} where {} is not part of a latex
              command, ...
            - ... when starting with an uppercase letter: suppress
conversion, remove the {}
            - ... when starting with a lowercase letter ("nm", "iPod"):
              suppress conversion, replace the {} with
              <span class="nocase"/></span>
            - Note: Camel case ("iPod") needs to be protected in
              bibtex/biblatex anyway; the only "extension" (wrt bibtex/biblatex
              specs) we'd be introducing is wrapping lowercase-only strings in
              {}, something that is never necessary on the latex side but
              won't break anything there either.
        - citeproc-hs/pandoc-citeproc should be modified to honour this new
          syntax and suppress conversion to title case for strings wrapped
          in `<span class="nocase"/></span>`.
        - Expected output, using one of the title-case CSL styles, here
          chicago-author-date.csl:

            Author, Ann. 2013. “A Title, in English, with a Proper Name and
an
            ACRONYM and a camelCase Word and Some Units, 400 nm, 3 cm, and
a Quote,
            *Alea iacta est*.” *Journal*.
    }

@article{item1,
    Author = {Author, Ann},
    Date = {2013},
    Hyphenation = {english},
    Journaltitle = {Journal},
    Title = {A Title, in {English}, with a {Proper Name} and an {ACRONYM}
and a {camelCase} Word and Some Units, 400~{nm}, 3~{cm}, and a Quote,
\textit{{Alea} {iacta est}}}
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Author
    given: Ann
  container-title: Journal
  id: item1
  issued: 2013
  language: en-US
  title: A title, in English, with a Proper Name and an ACRONYM and a
    [camelCase]{.nocase} word and some units, 400 [nm]{.nocase},
    3 [cm]{.nocase}, and a quote, *Alea [iacta est]{.nocase}*
  type: article-journal
---


```
