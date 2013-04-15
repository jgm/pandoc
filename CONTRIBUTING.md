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
spend some time trying to whittle it down to the minimum necessary to
illustrate the problem.

Have an idea for a new feature?
-------------------------------

Lay out the rationale for the feature you're requesting.  Why would this
feature be useful?  Consider also any possible drawbacks, including breaking
old documents.

It is best to discuss a potential new feature on [pandoc-discuss]
before opening an issue.

Patches and pull requests
-------------------------

Patches and pull requests are welcome.  Please follow these guidelines:

1.  Each patch should make a single logical change (fix a bug, add
    a feature, clean up some code, add documentation).  Everything
    related to that change should be included (including tests and
    documentation), and nothing unrelated should be included.

2.  Follow the stylistic conventions you find in the existing
    panadoc code.  Use spaces, not tabs, and wrap code to 80 columns.
    Always include type signatures for top-level functions.

3.  Your code should compile without warnings (`-Wall` clean).

4.  Run the tests to make sure your code does not introduce new bugs.
    (See below under [Tests](#tests).)

5.  It is a good idea to add test cases for the bug you are fixing.  (See below
    under [Tests](#tests).)  If you are adding a new writer or reader,
    you must include tests.

6.  If you are adding a new feature, include updates to the README.

7.  Before you put time into a nontrivial patch, it is a good idea to
    discuss it on [pandoc-discuss], especially if it is for a new feature
    (rather than fixing a bug).

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


[pandoc-discuss]: http://groups.google.com/group/pandoc-discuss
[issue tracker]: https://github.com/jgm/pandoc/issues
[User's Guide]: http://johnmacfarlane.net/pandoc/README.html
[FAQs]:  http://johnmacfarlane.net/pandoc/faqs.html

