---
title: Org-mode features and differences
author: Albert Krewinkel
---

Pandoc's handling of org files is similar to that of Emacs
org-mode. This document aims to highlight the cases where this is
not possible or just not the case yet.

Export options
==============

The following export keywords are supported:

- AUTHOR: comma-separated list of author(s); fully supported.

- CREATOR: output generator; passed as metadata entry, but
  ignored by most output formats.

- DATE: creation or publication date; well supported by pandoc.

- EMAIL: author email address; passed as metadata entry, but not
  included in most output formats.

- LANGUAGE: currently unsupported; use `#+LANG:` instead.

- SELECT_TAGS: tags which select a tree for export. Currently
  *unsupported*.

- EXCLUDE\_TAGS: tags which prevent a subtree from being
  exported. Fully supported.

- TITLE: document title; fully supported.

- EXPORT\_FILE\_NAME: target filename; *unsupported*, the output
  defaults to stdout unless a target has to be given as a command
  line option.


Citations
=========

Emacs org-mode lacks an official citation syntax, leading to
multiple syntaxes coexisting. Pandoc recognizes four different
syntaxes for citations.

Citation support for org-mode is enabled by default. Support can
be toggled off by disabling the `citation` extension; e.g.
`pandoc --from=org-citations`.

Berkeley-style citations
------------------------

The semi-official Org-mode citation syntax was designed by Richard
Lawrence with additions by contributors on the [emacs-orgmode
mailing list]. It is based on John MacFarlane's pandoc Markdown
syntax. It's dubbed Berkeley syntax due the place of activity of
its creators, both philosophers at UC Berkeley.

### Simple in-text citation

This is the simplest form of citation. It consists of the citation
ID prefixed by '@'.

Example:

    @WatsonCrick1953 showed that DNA forms a double-helix.

### In-text citation list

Citations presented in the text unparenthesized are called
*in-text citations*. The syntax for these citations is

    [cite: PREFIX; INDIVIDUAL-REFERENCE; ... INDIVIDUAL-REFERENCE; SUFFIX]

where the initial PREFIX and final SUFFIX are optional. At least
one INDIVIDUAL-REFERENCE must be present. The colon and
semicolons here are literal and indicate the end of the TAG and
the end of a PREFIX or INDIVIDUAL-REFERENCE respectively.

An INDIVIDUAL-REFERENCE has the format:

    PREFIX KEY SUFFIX

The KEY is obligatory, and the prefix and suffix are optional.

A PREFIX or SUFFIX is arbitrary text (except `;`, `]`, and
citation keys).

Example:

    [cite: See; @Mandelkern1981; and @Watson1953]

### Parenthetical citation

Citations surrounded by parantheses. The syntax is identical to
in-text citations, except for the additional parentheses enclosing
the initial `cite` tag.

    [(cite): See; @Mandelkern1981; and @Watson1953]

[emacs-orgmode mailing list]: https://lists.gnu.org/archive/html/emacs-orgmode/2015-02/msg00932.html

org-ref citations
-----------------

The [org-ref] package by [John Kitchen] is in wide use to handle
citations and has excellent tooling support in Emacs. Its
citation syntax is geared towards users in the natural sciences
but still very flexible regardless.

    cite:doe_john_2000
    citep:doe_jane_1989
    [[citep:Dominik201408][See page 20 of::, for example]]


Pandoc-Markdown-like syntax
---------------------------

Historically, Markdown-style citations syntax was the first that
was added to pandoc's org reader. It is close to Markdown's
citation syntax.

Citations go inside square brackets and are separated by
semicolons. Each citation must have a key, composed of '@' plus
the citation identifier from the database, and may optionally
have a prefix, a locator, and a suffix. The citation key must
begin with a letter, digit, or `_`, and may contain
alphanumerics, `_`, and internal punctuation characters
(`:.#$%&-+?<>~/`). If you want to use a citation key that does not
conform to these constraints, enclose the key in curly braces
(`{` and `}`). Inside the curly braces, the characters `}` and
`\\` must be backslash-escaped.

### Simple citation

The simplest method to insert a citation is to write the citation
ID prefixed by '@'.


Example:

    [prefix @citekey suffix]
    [see @doe2000 pp. 23-42]
    [@doe2000 p. 5; to a lesser extend @doe2005]
    [@{more complex citekey}]


LaTeX-Syntax
------------

Use normal latex citation commands like `\cite{x}` or
`\citet{y}`.

[org-ref]: https://github.com/jkitchin/org-ref
[John Kitchen]: https://kitchingroup.cheme.cmu.edu/


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


Currently unsupported features
==============================

Library of babel
----------------

The library of babel translates between various programming
languages. This is out-of-scope for pandoc. Use Emacs to run
code, then feed the resulting org file to pandoc.
