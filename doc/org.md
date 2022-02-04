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

- CREATOR: output generator; passed as plain-text metadata entry
  `creator`, but not used by any default templates.

- DATE: creation or publication date; well supported by pandoc.

- EMAIL: author email address; passed as plain-text metadata
  field `email`, but not used by any default templates.

- LANGUAGE: document language; included as plain-text metadata
  field `lang`. The value should be a [BCP47 language tag].

- SELECT_TAGS: tags which select a tree for export.

- EXCLUDE\_TAGS: tags which prevent a subtree from being
  exported. Fully supported.

- TITLE: document title; fully supported.

- EXPORT\_FILE\_NAME: target filename; *unsupported*, the output
  defaults to stdout unless a target has to be given as a command
  line option.

::: {.alert .alert-info}
Pandoc tries to be compatible with org-mode when exporting an org document. If
you find some behavior confusing, please do refer to org-mode
[Export-Settings](https://orgmode.org/manual/Export-Settings.html)
documentation. For example, a common confusion
([#3214](https://github.com/jgm/pandoc/issues/3214 "Problem with headers lower
then 3 in org-mode reader"), [#5169](https://github.com/jgm/pandoc/issues/5169
"org mode headings past level three converted to numbered outline list"),
[#6145](https://github.com/jgm/pandoc/issues/6145 "Headers 4 levels deep render
differently"), [#7236](https://github.com/jgm/pandoc/issues/7236 "In Org mode,
Header with level > 3 are not recognized as headers")) is treatment of headers
with level > 3 differently because org-mode sets `org-export-headline-levels`
(configurable with `#+OPTIONS: H:3`) to 3 by default.
:::

[BCP47 language tag]: https://tools.ietf.org/html/bcp47

Format-specific options
-----------------------

Emacs Org-mode supports additional export options which work for
specific export formats. Some of these options' behavior differs
in Org-mode depending on the output format, while pandoc is
format-agnostic when parsing; differences are noted where they
occur.

- DESCRIPTION: the document's description; pandoc parses this
  option as text with markup into the `description` metadata
  field. The field is not used in default templates.

  Pandoc follows the LaTeX exporter in that it allows markup in
  the description. In contrast, the Org-mode HTML exporter treats
  the description as plain text.

- LATEX\_HEADER and LATEX_HEADER_EXTRA: arbitrary lines to add to
  the document's preamble. Contrary to Org-mode, these lines are
  not inserted before the hyperref settings, but close to the end
  of the preamble.

  The contents of this option are stored as a list of raw LaTeX
  lines in the `header-includes` metadata field.

- LATEX\_CLASS: the LaTeX document class; like Org-mode, pandoc
  uses `article` as the default class.

  The contents of this option are stored as plain text in the
  `documentclass` metadata field.

- LATEX\_CLASS\_OPTIONS: Options for the LaTeX document class;
  fully supported.

  The contents of this option are stored as plain text in the
  `classoption` metadata field.

- SUBTITLE: the document's subtitle; fully supported.

  The content of this option is stored as inlines in the
  `subtitle` metadata field.

- HTML\_HEAD and HTML\_HEAD\_EXTRA: arbitrary lines to add to the
  HTML document's head; fully supported.

  The contents of these options are stored as a list of raw HTML
  lines in the `header-includes` metadata field.

Pandoc-specific options
-----------------------

Pandoc recognizes some export options not used by Emacs Org.

- NOCITE: this field adds the listed citations to the
  bibliography, without the need to mention them to the text. The
  special value `@*` causes all available references to be added
  the bibliography.

- HEADER-INCLUDES: like HTML_HEAD and, LATEX_HEADER, but treats
  the option's value as normal text with markup.

- INSTITUTE: Affiliation of the author; the value is read as text
  with markup and is stored in the `institute` metadata field. The
  field is included by default on the title slide of beamer
  presentations.

Other options
-------------

Any export option or directive not listed above has no effect when
parsing with pandoc. However, the information is retained as a
*raw block*. It can be accessed through a
[filter](https://pandoc.org/filters.html) and will be included in
org output.

### Directives as metadata

As an example, we will restore an old behavior of pandoc versions
prior to 2.10. Unknown keywords were treated as variable
definitions, and were added the document's metadata. Typing
`#+key: value` in the org-file used to have the same effect as
running pandoc with the `--metadata key=value` option.

Since pandoc 2.10, each unhandled line starting with `#+` is kept
internally as a raw block with format `org`. This block can be
inspected and processed by a filter. Below is a [Lua
filter](https://pandoc.org/lua-filters.html) which converts these
unhandled lines into metadata key-value pairs.

``` lua
-- intermediate store for variables and their values
local variables = {}

--- Function called for each raw block element.
function RawBlock (raw)
  -- Don't do anything unless the block contains *org* markup.
  if raw.format ~= 'org' then return nil end

  -- extract variable name and value
  local name, value = raw.text:match '#%+(%w+):%s*(.+)$'
  if name and value then
    variables[name] = value
  end
end

-- Add the extracted variables to the document's metadata.
function Meta (meta)
  for name, value in pairs(variables) do
    meta[name] = value
  end
  return meta
end
```

Tables
======

Pandoc supports normal org tables (sometimes called "pipe tables")
and grid tables (tables created by [table.el]).

Column widths
-------------

Org mode tables don't allow line-breaks within cells, and lines
which contain text can get very long. This often leads to tables
which run off the page when exporting, especially when exporting
to PDF via LaTeX. Overlong lines in the source text are this is
usually hidden by setting a [column width], but the default Emacs
exporters ignore that setting. Pandoc deviates from Emacs's
behavior and uses this information to resize the table columns
when exporting.

Limitations
-----------

There is no support yet for cells spanning multiple columns or
rows. The table.el grid tables allows rowspans and colspans and so
does pandoc's internal structure since 2.10, but the parser has
not been updated yet.

[table.el]: http://table.sourceforge.net/
[column width]: https://orgmode.org/manual/Column-Width-and-Alignment.html

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

`smart` extension
=================

Org-mode allows to insert certain characters via special character
sequences. For example, instead of typing the Unicode /HORIZONTAL
ELLISPIS/ character `…` by hand, one can instead type tree dots
`...`. En dashes and em dashes can be written as `--` and `---`
respectively. Furthermore, quotation marks (`"`) and
apostrophe-quotes (`'`) can be treated in a "smart" way,
potentially replacing them with proper, language specific unicode
quotation characters.

Like in Markdown, these behaviors can be turned on all-at-once by
enabling the `smart` extension. However, disabling `smart` (the
default) will *not* necessarily disable smart quotes and special
strings. Instead, it will just result in the default Org mode
behavior.

The special string feature can be turned off via the `#+OPTIONS:
-:nil` [export setting]. There are currently no command line flags
which control these features. As a workaround, one can use process
substitution, a feature supported by most shells. It allows to
provide the options line on the command line:

    pandoc -f org <(printf "#+OPTIONS: -:nil\n") …

[export setting]: https://orgmode.org/manual/Export-Settings.html

`fancy_lists` extension
=======================

Org-mode has a variable `org-list-allow-alphabetical` that when
set to `t`, allows ordered lists with single-character
alphabetical markers. Since this variable is `nil` by default,
alphabetical markers can be optionally enabled in Pandoc by
enabling the `fancy_lists` extension.

When `fancy_lists` is enabled, Pandoc will also parse list
markers starting with one lowercase or uppercase alphabetical
character, like `a.` and `D)`. Countrary to the use of this
extension in markdown, roman numerals or the `#` placeholder
can't be used as markers as they are not allowed in Org-mode.

One additional behavior that is enabled by the `fancy_lists`
extension is that the `.` and `)` delimiters for list markers
will be distinguished by Pandoc. In essence, this means that when
converting Org into formats like LaTeX, Pandoc will respect the
type of delimiter that you used in your Org file, instead of
always using the default delimiter for the exported format.

Currently unsupported features
==============================

Library of babel
----------------

The library of babel translates between various programming
languages. This is out-of-scope for pandoc. Use Emacs to run
code, then feed the resulting org file to pandoc.
