Contributing to pandoc
======================

Found a bug?
------------

Bug reports are welcome!  Please report all bugs on pandoc's github
[issue tracker].

Before you submit a bug report, search the (open *and* closed) issues to make
sure the issue hasn't come up before. Also, check the [User's Guide] and [FAQs]
for anything relevant.

Make sure you can reproduce the bug with the latest released version of pandoc
(or, even better, the development version).

Your report should give detailed instructions for how to reproduce the problem,
including

  * the exact command line used
  * the exact input used
  * the output received
  * the output you expected instead

A small test case (just a few lines) is ideal.  If your input is large,
try to whittle it down to the minimum necessary to illustrate the problem.

Have an idea for a new feature?
-------------------------------

First, search [pandoc-discuss] and the issue tracker (both open and closed
issues) to make sure that the idea has not been discussed before.

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

7.  If you are adding a new feature, include updates to the README.

8.  All code must be released under the general license governing pandoc
    (GPL v2).

9.  It is better not to introduce new dependencies.  Dependencies on
    external C libraries should especially be avoided.

Tests
-----

Tests can be run as follows:

    cabal configure --enable-tests
    cabal build
    cabal test

The test program is `tests/test-pandoc.hs`.

Benchmarks can be enabled by passing the `--enable-benchmarks` flag
to `cabal configure`, and run using `cabal bench`.

The code
--------

Pandoc has a publicly accessible git repository on
github: <http://github.com/jgm/pandoc>.  To get a local copy of the source:

    git clone git://github.com/jgm/pandoc.git

Note:  after cloning the repository (and in the future after pulling from it),
you should do

    git submodule update --init

to pull in changes to the templates (`data/templates/`).  You can automate this
by creating a file `.git/hooks/post-merge` with the contents:

    #!/bin/sh
    git submodule update --init

and making it executable:

    chmod +x .git/hooks/post-merge

The source for the main pandoc program is `pandoc.hs`.  The source for
the pandoc library is in `src/`, the source for the tests is in
`tests/`, and the source for the benchmarks is in `benchmark/`.

The modules `Text.Pandoc.Definition`, `Text.Pandoc.Builder`, and
`Text.Pandoc.Generics` are in a separate library `pandoc-types`.  The code can
be found in a <http://github.com/jgm/pandoc-types>.

To build pandoc, you will need a working installation of the
[Haskell platform].

The library is structured as follows:

  - `Text.Pandoc` is a top-level module that exports what is needed
    by most users of the library.  Any patches that add new readers
    or writers will need to make changes here, too.
  - `Text.Pandoc.Definition` (in `pandoc-types`) defines the types
    used for representing a pandoc document.
  - `Text.Pandoc.Builder` (in `pandoc-types`) provides functions for
    building pandoc documents programatically.
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
    highlighting-kate library, which is used for code syntax highlighting.
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
    and javascript and turning them into `data:` URLs.
  - `Text.Pandoc.Shared` is a grab-bag of shared utility functions.
  - `Text.Pandoc.Writers.Shared` contains utilities used in writers only.
  - `Text.Pandoc.Slides` contains functions for splitting a markdown document
    into slides, using the conventions described in the README.
  - `Text.Pandoc.Templates` defines pandoc's templating system.
  - `Text.Pandoc.UTF8` contains functions for converting text to and from
    UTF8 bytestrings (strict and lazy).
  - `Text.Pandoc.Asciify` contains functions to derive ascii versions of
    identifiers that use accented characters.
  - `Text.Pandoc.UUID` contains functions for generating UUIDs.
  - `Text.Pandoc.XML` contains functions for formatting XML.

[pandoc-discuss]: http://groups.google.com/group/pandoc-discuss
[issue tracker]: https://github.com/jgm/pandoc/issues
[User's Guide]: http://johnmacfarlane.net/pandoc/README.html
[FAQs]:  http://johnmacfarlane.net/pandoc/faqs.html
[EditorConfig]: http://editorconfig.org/
[Haskell platform]: http://www.haskell.org/platform/
[hsb2hs]: http://hackage.haskell.org/package/hsb2hs
