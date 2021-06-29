```
% pandoc --citeproc -t markdown-citations
---
bibliography: command/biblio.bib
csl: 'command/chicago-fullnote-bibliography.csl'
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

[^3]: Like a citation without author: [-@item1], and again
    [-@item1], and now Doe with a locator [-@item2 p. 44].
^D
[WARNING] Citeproc: citation nonexistent not found
# Pandoc with citeproc-hs

[^1]

[^2]

John Doe[^3] says blah.

Doe[^4] says blah.

Doe[^5] says blah.

Doe[^6] says blah.

In a note.[^7]

A citation group.[^8]

Another one.[^9]

And another one in a note.[^10]

Citation with a suffix and locator.[^11]

Citation with suffix only.[^12]

Now some modifiers.[^13]

With some markup.[^14]

# References {#references .unnumbered}

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item2 .csl-entry}
Doe, John. "Article." *Journal of Generic Studies* 6 (2006): 33--34.
:::

::: {#ref-item1 .csl-entry}
---------. *First Book*. Cambridge: Cambridge University Press, 2005.
:::

::: {#ref-пункт3 .csl-entry}
Doe, John, and Jenny Roe. "Why Water Is Wet." In *Third Book*, edited by
Sam Smith. Oxford: Oxford University Press, 2007.
:::
:::

[^1]: [**Nonexistent?**](#ref-nonexistent)

[^2]: [**Nonexistent?**](#ref-nonexistent)

[^3]: [*First Book* (Cambridge: Cambridge University Press,
    2005)](#ref-item1).

[^4]: [30](#ref-item1).

[^5]: [30](#ref-item1), with suffix.

[^6]: [*First Book*](#ref-item1); ["Article," *Journal of Generic
    Studies* 6 (2006): 30](#ref-item2); see also [John Doe and Jenny
    Roe, "Why Water Is Wet," in *Third Book*, ed. Sam Smith (Oxford:
    Oxford University Press, 2007)](#ref-пункт3).

[^7]: Doe and Roe, ["Why Water Is Wet," 12](#ref-пункт3) and a citation
    without locators ([Doe and Roe, "Why Water Is Wet"](#ref-пункт3)).

[^8]: See [Doe, *First Book*, chap. 3](#ref-item1); also [Doe and Roe,
    "Why Water Is Wet," 34--35](#ref-пункт3).

[^9]: See [Doe, *First Book*, 34--35](#ref-item1).

[^10]: Some citations (see [Doe, chap. 3](#ref-item1); [Doe and Roe,
    "Why Water Is Wet"](#ref-пункт3); [Doe, "Article"](#ref-item2)).

[^11]: [Doe, *First Book*, 33, 35--37](#ref-item1), and nowhere else.

[^12]: [Doe, *First Book*](#ref-item1) and nowhere else.

[^13]: Like a citation without author: (), and again (), and now Doe
    with a locator (["Article," 44](#ref-item2)).

[^14]: *See* [Doe, *First Book*, 32](#ref-item1).
```
