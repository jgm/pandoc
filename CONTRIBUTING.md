Contributing to pandoc
======================

Have a question?
----------------

Ask on [pandoc-discuss].


Found a bug?
------------

Bug reports are welcome!  Please report all bugs on pandoc's github
[issue tracker].

Before you submit a bug report, search the [open issues] *and* [closed issues]
to make sure the issue hasn't come up before. Also, check the [User's Guide] and
[FAQs] for anything relevant.

Make sure you can reproduce the bug with the [latest released
version] of pandoc---or, even better, the development version,
since the bug may have been fixed since the last release.
[Nightly builds] are available, so you don't need to compile
from source to test againt the development version.
(To fetch a nightly, visit the link, click the topmost "Nightly"
in the table, then choose your platform under "Artifacts."  Note
that you must be logged in with a GitHub account.)

Your report should give detailed, *reproducible* instructions, including

  * the pandoc version (check using `pandoc -v`)
  * the exact command line used
  * the exact input used
  * the output received
  * the output you expected instead

A small test case (just a few lines) is ideal.  If your input is large,
try to whittle it down to a *minimum working example*.

Out of scope?
-------------

A less than perfect conversion does not necessarily mean there's
a bug in pandoc.  Quoting from the MANUAL:

> Because pandoc's intermediate representation of a document is less
> expressive than many of the formats it converts between, one should
> not expect perfect conversions between every format and every other.
> Pandoc attempts to preserve the structural elements of a document, but
> not formatting details such as margin size.  And some document elements,
> such as complex tables, may not fit into pandoc's simple document
> model.  While conversions from pandoc's Markdown to all formats aspire
> to be perfect, conversions from formats more expressive than pandoc's
> Markdown can be expected to be lossy.

For example, both `docx` and `odt` formats can represent margin size, but
because pandoc's internal document model does not contain a representation of
margin size, this information will be lost on converting from docx
to `odt`.  (You can, however, customize margin size using `--reference-doc`.)

So before submitting a bug report, consider whether it might be
"out of scope." If it concerns a feature of documents that isn't
representable in pandoc's Markdown, then it very likely is.
(If in doubt, you can always ask on [pandoc-discuss].)

Fixing bugs from the issue tracker
----------------------------------

Almost all the bugs on the issue tracker have one or more associated
tags. These are used to indicate the *complexity* and *nature* of a
bug. There is not yet a way to indicate priority. An up to date
summary of issues can be found on [GitHub labels].

* [good first issue] — The perfect starting point for new contributors. The
  issue is generic and can be resolved without deep knowledge of the code
  base.
* [enhancement] — A feature which would be desirable. We recommend
  you discuss any proposed enhancement on pandoc-discuss before
  writing code.
* [bug] — A problem which needs to be fixed.
* [complexity:low] — The fix should only be a couple of lines.
* [complexity:high] — The fix might require structural changes or in depth
  knowledge of the code base.
* [new:reader] — A request to add a new input format.
* [new:writer] — A request to add a new output format.
* [docs] — A discrepancy,  or ambiguity in the documentation.
* [status:in-progress] — Someone is actively working on or planning to work on the
  ticket.
* [status:more-discussion-needed] — It is unclear what the correct approach
  to solving the ticket is. Before starting on tickets such as this it
  would be advisable to post on the ticket.
* [status:more-info-needed] — We require more information from a user before
  we can classify a report properly.

Issues related to a specific format are tagged accordingly, e.g. feature request
or bug reports related to Markdown are labelled with [format:markdown].

Have an idea for a new feature?
-------------------------------

First, search [pandoc-discuss] and the issue tracker (both [open issues] *and*
[closed issues]) to make sure that the idea has not been discussed before.

Explain the rationale for the feature you're requesting.  Why would this
feature be useful?  Consider also any possible drawbacks, including backwards
compatibility, new library dependencies, and performance issues.

It is best to discuss a potential new feature on [pandoc-discuss]
before opening an issue.

Patches and pull requests
-------------------------

Patches and pull requests are welcome.  Before you put time into a nontrivial
patch, it is a good idea to discuss it on [pandoc-discuss], especially if it is
for a new feature (rather than fixing a bug).

Please follow these guidelines:

1.  Each patch (commit) should make a single logical change (fix a bug, add
    a feature, clean up some code, add documentation).  Everything
    related to that change should be included (including tests and
    documentation), and nothing unrelated should be included.

2.  The first line of the commit message should be a short description
    of the whole commit (ideally <= 50 characters).  Then there should
    be a blank line, followed by a more detailed description of the
    change.

3.  Follow the stylistic conventions you find in the existing
    pandoc code.  Use spaces, not tabs, and wrap code to 80 columns.
    Always include type signatures for top-level functions.
    Consider installing [EditorConfig], this will help you to follow the
    coding style prevalent in pandoc.

4.  Your code should compile without warnings (`-Wall` clean).

5.  Run the tests to make sure your code does not introduce new bugs.
    (See below under [Tests](#tests).)  All tests should pass.

6.  It is a good idea to add test cases for the bug you are fixing.  (See
    below under [Tests](#tests).)  If you are adding a new writer or reader,
    you must include tests.

7.  If you are adding a new feature, include updates to `MANUAL.txt`.

8.  All code must be released under the general license governing pandoc
    (GPL v2).

9.  It is better not to introduce new dependencies.  Dependencies on
    external C libraries should especially be avoided.

10. We aim for compatibility with ghc versions from 8.0 to the
    latest release.  All pull requests and commits are tested
    automatically on CircleCI.

Tests
-----

Tests can be run as follows:

    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build
    cabal test

or, if you're using [stack],

    stack setup
    stack test

The test program is `test/test-pandoc.hs`.

To run particular tests (pattern-matching on their names), use
the `-p` option:

    cabal install pandoc --enable-tests
    cabal test --test-options='-p markdown'

Or with stack:

    stack test --test-arguments='-p markdown'

It is often helpful to add `-j4` (run tests in parallel)
and `--hide-successes` (don't clutter output with successes)
to the test arguments as well.

If you add a new feature to pandoc, please add tests as well, following
the pattern of the existing tests. The test suite code is in
`test/test-pandoc.hs`. If you are adding a new reader or writer, it is
probably easiest to add some data files to the `test` directory, and
modify `test/Tests/Old.hs`. Otherwise, it is better to modify the module
under the `test/Tests` hierarchy corresponding to the pandoc module you
are changing.  Alternatively, you may add a "command test" to
the `/test/command/` hierarchy, following the pattern of the tests there.
These test files should have a meaningful name, which can include the issue
number and/or the feature that's being tested. For example, `5474-tables.md`
refers to both issue and feature.

You can rebuild the golden tests in `tests/` by passing
`--accept` to the test script. (If you're using stack, `stack
test --test-arguments "--accept"`; or `make TESTARGS=--accept`).
Then check the changed golden files for accuracy, and
commit the changes.  For docx or pptx tests, open the files in Word
or Powerpoint to ensure that they weren't corrupted and that
they had the expected result, and mention the Word/Powerpoint
version and OS in your commit comment.

Benchmarks
----------

To run benchmarks with cabal:

    cabal configure --enable-benchmarks
    cabal build
    cabal bench

With stack:

    stack bench

You can also build `weigh-pandoc` (`stack build pandoc:weigh-pandoc`)
to get some statistics on memory usage.  (Eventually this should
be incorporated into the benchmark suite.)

Using the REPL
--------------

With a recent version of cabal, you can do `cabal repl` and get
a ghci REPL for working with pandoc.  With [stack], use
`stack ghci`.

We recommend using the following `.ghci` file (which can be
placed in the source directory):

	:set -fobject-code
	:set -XTypeSynonymInstances
	:set -XScopedTypeVariables
	:set -XOverloadedStrings

Profiling
---------

To diagnose a performance issue with parsing, first try using
the `--trace` option.  This will give you a record of when block
parsers succeed, so you can spot backtracking issues.

To use the GHC profiler with cabal:

    cabal clean
    cabal install --enable-library-profiling --enable-executable-profiling
    pandoc +RTS -p -RTS [file]...
    less pandoc.prof

With stack:

    stack clean
    stack install --profile
    pandoc +RTS -p -RTS [file]...
    less pandoc.prof

Templates
---------

The default templates live in `data/templates`, which is a git
subtree linked to <https://github.com/jgm/pandoc-templates.git>.
The purpose of maintaining a separate repository is to allow
people to maintain variant templates as a fork.

You can modify the templates and submit patches without worrying
much about this: when these patches are merged, we will
push them to the main templates repository by doing

    git subtree push --prefix=data/templates templates master

where `templates` is a remote pointing to the templates
repository.

The code
--------

Pandoc has a publicly accessible git repository on
github: <http://github.com/jgm/pandoc>.  To get a local copy of the source:

    git clone https://github.com/jgm/pandoc.git

The source for the main pandoc program is `pandoc.hs`.  The source for
the pandoc library is in `src/`, the source for the tests is in
`test/`, and the source for the benchmarks is in `benchmark/`.

The modules `Text.Pandoc.Definition`, `Text.Pandoc.Builder`, and
`Text.Pandoc.Generic` are in a separate library `pandoc-types`.  The code can
be found in <http://github.com/jgm/pandoc-types>.

To build pandoc, you will need a working installation of the
[Haskell platform].

The library is structured as follows:

  - `Text.Pandoc` is a top-level module that exports what is needed
    by most users of the library.  Any patches that add new readers
    or writers will need to make changes here, too.
  - `Text.Pandoc.Definition` (in `pandoc-types`) defines the types
    used for representing a pandoc document.
  - `Text.Pandoc.Builder` (in `pandoc-types`) provides functions for
    building pandoc documents programmatically.
  - `Text.Pandoc.Generics` (in `pandoc-types`) provides functions allowing
    you to promote functions that operate on parts of pandoc documents
    to functions that operate on whole pandoc documents, walking the
    tree automatically.
  - `Text.Pandoc.Readers.*` are the readers, and `Text.Pandoc.Writers.*`
    are the writers.
  - `Text.Pandoc.Biblio` is a utility module for formatting citations
    using citeproc-hs.
  - `Text.Pandoc.Data` is used to embed data files when the `embed_data_files`
    cabal flag is used.  It is generated from `src/Text/Pandoc/Data.hsb` using
    the preprocessor [hsb2hs].
  - `Text.Pandoc.Highlighting` contains the interface to the
    skylighting library, which is used for code syntax highlighting.
  - `Text.Pandoc.ImageSize` is a utility module containing functions for
    calculating image sizes from the contents of image files.
  - `Text.Pandoc.MIME` contains functions for associating MIME types
    with extensions.
  - `Text.Pandoc.Options` defines reader and writer options.
  - `Text.Pandoc.PDF` contains functions for producing a PDF from a
    LaTeX source.
  - `Text.Pandoc.Parsing` contains parsing functions used in multiple readers.
  - `Text.Pandoc.Pretty` is a pretty-printing library specialized to
    the needs of pandoc.
  - `Text.Pandoc.SelfContained` contains functions for making an HTML
    file "self-contained," by importing remotely linked images, CSS,
    and JavaScript and turning them into `data:` URLs.
  - `Text.Pandoc.Shared` is a grab-bag of shared utility functions.
  - `Text.Pandoc.Writers.Shared` contains utilities used in writers only.
  - `Text.Pandoc.Slides` contains functions for splitting a markdown document
    into slides, using the conventions described in the MANUAL.
  - `Text.Pandoc.Templates` defines pandoc's templating system.
  - `Text.Pandoc.UTF8` contains functions for converting text to and from
    UTF8 bytestrings (strict and lazy).
  - `Text.Pandoc.Asciify` contains functions to derive ascii versions of
    identifiers that use accented characters.
  - `Text.Pandoc.UUID` contains functions for generating UUIDs.
  - `Text.Pandoc.XML` contains functions for formatting XML.

Lua filters
-----------

If you've written a useful pandoc [lua filter](lua-filters.html),
you may want to consider submitting a pull request to the
[lua-filters repository](https://github.com/pandoc/lua-filters).

[open issues]: https://github.com/jgm/pandoc/issues
[closed issues]: https://github.com/jgm/pandoc/issues?q=is%3Aissue+is%3Aclosed
[latest released version]: https://github.com/jgm/pandoc/releases/latest
[Nightly builds]: https://github.com/jgm/pandoc/actions?query=workflow%3ANightly
[pandoc-discuss]: http://groups.google.com/group/pandoc-discuss
[issue tracker]: https://github.com/jgm/pandoc/issues
[User's Guide]: http://pandoc.org/MANUAL.html
[FAQs]:  http://pandoc.org/faqs.html
[EditorConfig]: http://editorconfig.org/
[Haskell platform]: http://www.haskell.org/platform/
[hsb2hs]: http://hackage.haskell.org/package/hsb2hs
[GitHub labels]: https://github.com/jgm/pandoc/labels
[good first issue]:https://github.com/jgm/pandoc/labels/good%20first%20issue
[enhancement]: https://github.com/jgm/pandoc/labels/enhancement
[bug]: https://github.com/jgm/pandoc/labels/bug
[complexity:low]: https://github.com/jgm/pandoc/labels/complexity:low
[complexity:high]: https://github.com/jgm/pandoc/labels/complexity:high
[docs]: https://github.com/jgm/pandoc/labels/docs
[format:markdown]: https://github.com/jgm/pandoc/labels/format:markdown
[new:reader]: https://github.com/jgm/pandoc/labels/new:reader
[new:writer]: https://github.com/jgm/pandoc/labels/new:writer
[status:in-progress]: https://github.com/jgm/pandoc/labels/status:in-progress
[status:more-discussion-needed]: https://github.com/jgm/pandoc/labels/status:more-discussion-needed
[status:more-info-needed]: https://github.com/jgm/pandoc/labels/status:more-info-needed
[stack]: https://github.com/commercialhaskell/stack

