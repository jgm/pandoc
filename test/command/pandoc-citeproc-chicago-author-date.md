```
% pandoc --citeproc -t markdown-citations
---
bibliography: command/biblio.bib
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

([**nonexistent?**](#ref-nonexistent))

([**nonexistent?**](#ref-nonexistent))

Doe ([2005](#ref-item1)) says blah.

Doe ([2005, 30](#ref-item1)) says blah.

Doe ([2005, 30](#ref-item1), with suffix) says blah.

Doe ([2005](#ref-item1); [2006, 30](#ref-item2); see also [Doe and Roe
2007](#ref-пункт3)) says blah.

In a note.[^1]

A citation group (see [Doe 2005, chap. 3](#ref-item1); also [Doe and Roe
2007, 34--35](#ref-пункт3)).

Another one (see [Doe 2005, 34--35](#ref-item1)).

And another one in a note.[^2]

Citation with a suffix and locator ([Doe 2005, 33, 35--37](#ref-item1),
and nowhere else).

Citation with suffix only ([Doe 2005](#ref-item1) and nowhere else).

Now some modifiers.[^3]

With some markup (*see* [Doe 2005, 32](#ref-item1)).

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item1 .csl-entry}
Doe, John. 2005. *First Book*. Cambridge: Cambridge University Press.
:::

::: {#ref-item2 .csl-entry}
---------. 2006. "Article." *Journal of Generic Studies* 6: 33--34.
:::

::: {#ref-пункт3 .csl-entry}
Doe, John, and Jenny Roe. 2007. "Why Water Is Wet." In *Third Book*,
edited by Sam Smith. Oxford: Oxford University Press.
:::
:::

[^1]: Doe and Roe ([2007, 12](#ref-пункт3)) and a citation without
    locators ([Doe and Roe 2007](#ref-пункт3)).

[^2]: Some citations (see [Doe 2005, chap. 3](#ref-item1);
    [2006](#ref-item2); [Doe and Roe 2007](#ref-пункт3)).

[^3]: Like a citation without author: ([2005](#ref-item1)), and now Doe
    with a locator ([2006, 44](#ref-item2)).
```
