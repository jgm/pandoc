% Using the pandoc API
% John MacFarlane

Pandoc can be used as a Haskell library, to write your own
conversion tools or power a web application.  This document
offers an introduction to using the pandoc API.

Detailed API documentation at the level of individual functions
and types is available at
<https://hackage.haskell.org/package/pandoc>.

# Pandoc's architecture, and a simple example

Pandoc structure, readers, writers.

example of using a reader.

example of using a writer.

chaining them together.

# The PandocMonad class

Pandoc's functions define computations that can be run in
any instance of the `PandocMonad` typeclass.  Two instances
are provided: `PandocIO` and `PandocPure`. The difference is
that computations run in `PandocIO` are allowed to do IO
(for example, read a file), while computations in `PandocPure`
are free of any side effects.  `PandocPure` is useful when
you want to prevent users from doing anything malicious.

Here's an example of such a computation, from the module
`Text.Pandoc.Class`:

```haskell
-- | Get the verbosity level.
getVerbosity :: PandocMonad m => m Verbosity
```

motivations
Class.

# The Pandoc structure

blocks/inlines

# Readers and writers

getReader, getWriter

# Options

various reader and writer options you can set.
templates
extensions

# Builder

Inlines vs Inline, etc.
Monoid

example: report from CSV data

# Templates and other data files

# Handling errors and warnings

# Generic transformations

Walk and syb for AST transformations

# Filters

just the basic idea of toJSONFilter
the rest can be left to filters.md

# Self-contained


# PDF

# Custom PandocMonad instances

# Creating a front-end

Text.Pandoc.App

