---
title: Org-mode features and differences
author: Albert Krewinkel
---

Pandoc handles org files very similarly to Emacs org-mode.
However, there are differences worth highlighting.


Citations
=========

Emacs org-mode lacks an official citation syntax, leading to
multiple syntaxes coexisting. Pandoc recognizes four different
syntaxes for citations.

Berkeley-style citations
------------------------

The semi-offical Org-mode citation syntax is based on John
MacFarlane's Pandoc syntax and org-oriented enhancements
contributed by Richard Lawrence and others. It's dubbed Berkeley
syntax due the place of activity of its main contributors.

Example:

    See @john_doe_2006.
    [cite: See; @Mandelkern1981; and @Watson1953]
    [(cite): See; @Mandelkern1981; and @Watson1953]


org-ref citations
-----------------

The [org-ref] package is in wide use to handle citations and has
excellent tooling support in Emacs. Its citation syntax is
geared towards users in the natural sciences but still very
flexible regardless.

    cite:doe_john_2000
    citep:doe_jane_1989
    [[citep:Dominik201408][See page 20 of::, for example]]


Pandoc-Markdown-like syntax
---------------------------

Historically, Markdown-style citations syntax was the first that
was added to pandoc's org reader. It is almost identical to
Markdown's citation syntax.

Example:

    [prefix @citekey suffix]
    [see @doe2000 p. 23-42]


LaTeX-Syntax
------------

Use normal latex citation commands like `\cite{x}` or
`\citet{y}`.

[org-ref]: https://github.com/jkitchin/org-ref


Emphasis rules
==============

Org-mode uses complex rules to decide whether a string
represents emphasized text. In Emacs, this can be customized via
the variable `org-emphasis-regexp-components`. A variable like
this doesn't fit well with pandoc's model. Instead, it is
possible to use special lines to change these values:

    #+pandoc-emphasis-pre: "-\t ('\"{"
    #+pandoc-emphasis-post: "-\t\n .,:!?;'\")}["
    
The above describes the default values of these variables. The
arguments must be valid (Haskell) strings. If interpretation of
the argument as string fails, the default is restored.

Changing emphasis rules only affect the part of the document
following the special lines. They must be some of the first
lines to alter parsing behavior for the whole document. It is
also possible to change the values temporarily for selected
sections only. The string `test` in the following snippet will
be read as emphasized text, while the rest of the document will
be parsed using default emphasis rules:

    #+pandoc-emphasis-pre: "["
    #+pandoc-emphasis-post: "]"
    [/test/]
    #+pandoc-emphasis-pre:
    #+pandoc-emphasis-post:
