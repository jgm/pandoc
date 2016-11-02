<!-- Before you create a pull request, make sure you read through https://github.com/jgm/pandoc/blob/master/CONTRIBUTING.md -->

<!-- The following checklist is for your reference, but not mandatory:

- [ ] Each patch (commit) should make a single logical change (fix a bug, add
    a feature, clean up some code, add documentation).  Everything
    related to that change should be included (including tests and
    documentation), and nothing unrelated should be included.

- [ ] The first line of the commit message should be a short description
    of the whole commit (ideally <= 50 characters).  Then there should
    be a blank line, followed by a more detailed description of the
    change.

- [ ] Follow the stylistic conventions you find in the existing
    pandoc code.  Use spaces, not tabs, and wrap code to 80 columns.
    Always include type signatures for top-level functions.
    Consider installing [EditorConfig], this will help you to follow the
    coding style prevalent in pandoc.

- [ ] Your code should compile without warnings (`-Wall` clean).

- [ ] Run the tests to make sure your code does not introduce new bugs.
    All tests should pass.

- [ ] It is a good idea to add test cases for the bug you are fixing.
    If you are adding a new writer or reader, you must include tests.

- [ ] If you are adding a new feature, include updates to MANUAL.txt.

- [ ] All code must be released under the general license governing pandoc
    (GPL v2).

- [ ] It is better not to introduce new dependencies.  Dependencies on
    external C libraries should especially be avoided.

- [ ] We aim for compatibility with ghc versions from 7.8.3 to the
    latest release.  All pull requests and commits are tested
    automatically on travis-ci.org, using GHC versions in the
    `Tested-With` stanza of `pandoc.cabal`.  We currently relax
    the "`-Wall` clean" requirement for GHC 7.10.x, because
    there are so many warnings relating to the addition of type
    classes to the Prelude.

-->
