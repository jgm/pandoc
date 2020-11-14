```
% pandoc --citeproc -t markdown-citations
---
bibliography: command/biblio.bib
csl: command/ieee.csl
link-citations: true
---

Pandoc with citeproc-hs
=======================

[@nonexistent]

@nonexistent

@item1 says blah.

@item1 [p. 30] says blah.

@item1 [p. 30, with suffix] says blah.

@item1 [-@item2 p. 30; see also @пункт3] says blah.

In a note.[^1]

A citation group [see @item1 chap. 3; also @пункт3 p. 34-35].

Another one [see @item1 p. 34-35].

And another one in a note.[^2]

Citation with a suffix and locator [@item1 pp. 33, 35-37, and nowhere
else].

Citation with suffix only [@item1 and nowhere else].

Now some modifiers.[^3]

With some markup [*see* @item1 p. **32**].

References {#references .unnumbered}
==========

[^1]: @пункт3 [p. 12] and a citation without locators [@пункт3].

[^2]: Some citations [see @item1 chap. 3; @пункт3; @item2].

[^3]: Like a citation without author: [-@item1], and now Doe with a
    locator [-@item2 p. 44].
^D
[WARNING] Citeproc: citation nonexistent not found
# Pandoc with citeproc-hs

[**nonexistent?**](#ref-nonexistent)

[**nonexistent?**](#ref-nonexistent)

[\[1\]](#ref-item1) says blah.

[\[1, p. 30\]](#ref-item1) says blah.

[\[1, p. 30\]](#ref-item1), with suffix says blah.

[\[1\]](#ref-item1), [\[2, p. 30\]](#ref-item2), see also
[\[3\]](#ref-пункт3) says blah.

In a note.[^1]

A citation group see [\[1, Ch. 3\]](#ref-item1), also [\[3, pp.
34--35\]](#ref-пункт3).

Another one see [\[1, pp. 34--35\]](#ref-item1).

And another one in a note.[^2]

Citation with a suffix and locator [\[1, pp. 33, 35--37\]](#ref-item1),
and nowhere else.

Citation with suffix only [\[1\]](#ref-item1) and nowhere else.

Now some modifiers.[^3]

With some markup *see* [\[1, p. 32\]](#ref-item1).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body}
::: {#ref-item1 .csl-entry}
[\[1\] ]{.csl-left-margin}[J. Doe, *First book*. Cambridge: Cambridge
University Press, 2005.]{.csl-right-inline}
:::

::: {#ref-item2 .csl-entry}
[\[2\] ]{.csl-left-margin}[J. Doe, "Article," *Journal of Generic
Studies*, vol. 6, pp. 33--34, 2006.]{.csl-right-inline}
:::

::: {#ref-пункт3 .csl-entry}
[\[3\] ]{.csl-left-margin}[J. Doe and J. Roe, "Why water is wet," in
*Third book*, S. Smith, Ed. Oxford: Oxford University Press,
2007.]{.csl-right-inline}
:::
:::

[^1]: [\[3, p. 12\]](#ref-пункт3) and a citation without locators
    [\[3\]](#ref-пункт3).

[^2]: Some citations see [\[1, Ch. 3\]](#ref-item1),
    [\[2\]](#ref-item2), [\[3\]](#ref-пункт3).

[^3]: Like a citation without author: [\[1\]](#ref-item1), and now Doe
    with a locator [\[2, p. 44\]](#ref-item2).
```
