---
title: Creating an ebook with pandoc
author: John MacFarlane
---

Starting with version 1.6, pandoc can produce output in the [EPUB]
electronic book format. EPUB books can be viewed on iPads, Nooks, and
other electronic book readers, including many smart phones. (They can
also be converted to Kindle books using [KindleGen].)

This means that it's now very easy to produce an electronic book!
Let's try it.

# A toy example

Use your text editor to create a file `mybook.txt`, with the following
contents:

    % My Book
    % Sam Smith

    This is my book!

    # Chapter One

    Chapter one is over.

    # Chapter Two

    Chapter two has just begun.

To make this into an ebook takes only one command:

    pandoc mybook.txt -o mybook.epub

You can upload `mybook.epub` to your ebook reader and try it out.

Note that if your markdown file contains links to local images, for
example

    ![Juliet](images/sun.jpg)

pandoc will automatically include the images in the generated
epub.

# A real book

To see what this would look like for a real book, let's convert Scott
Chacon's book [Pro Git], which he wrote using pandoc's markdown variant
and released under a [Creative Commons] license. (If you use the book,
please consider [buying a copy] to help support his excellent work.)

You can find the markdown source for the book on its
[github site].  Let's get a copy of the whole repository:[^1]

    git clone https://github.com/progit/progit.git

[^1]: If you don't have [git], you can browse to the [github site] and
click "Download Source" to get the same files in a zip or tar archive.

This command will create a working directory called `progit` on
your machine.  The actual markdown sources for the English version
of the book are in the `en` subdirectory, so start by changing
to that directory:

    cd progit/en

As you can see, each chapter is a single text file in its own directory.
Chacon does some postprocessing on these files, for example, to insert images.
This is a placeholder for Figure 1-1, for example:

    Insert 18333fig0101.png
    Figure 1-1. Local version control diagram.

The actual image file is called `18333fig0101-tn.png` and lives in
the `figures` subdirectory of the repository, as you can verify.

For demonstration purposes, we want pure markdown files, so let's
change this placeholder into a markdown image link.  Pandoc will
treat a paragraph containing a single image as a figure with
a caption, which is what we want:

    ![Figure 1-1. Local version control diagram.](../figures/18333fig0101-tn.png)

We can make this change in all the files with a perl one-liner:

    perl -i -0pe \
    's/^Insert\s*(.*)\.png\s*\n([^\n]*)$/!\[\2](..\/figures\/\1-tn.png)/mg' \
    */*.markdown

This will modify the files in place. (We won't worry about backing
them up; if we mess up, we can get the original files back with
`git reset --hard`.)

OK! Now we're almost ready to make an ebook.  We have the chapters,
each in its own file, but we still need a title. Create a file,
`title.txt`, with a pandoc YAML metadata block:

```
---
title: Pro Git
author: Scott Chacon
rights:  Creative Commons Non-Commercial Share Alike 3.0
language: en-US
...
```

See the [User's Guide](https://pandoc.org/MANUAL.html#epub-metadata) for more information
above these fields.

Now run pandoc to make the ebook, using our title page and modified
chapter files as sources:

    pandoc -o progit.epub title.txt \
      01-introduction/01-chapter1.markdown \
      02-git-basics/01-chapter2.markdown \
      03-git-branching/01-chapter3.markdown \
      04-git-server/01-chapter4.markdown \
      05-distributed-git/01-chapter5.markdown \
      06-git-tools/01-chapter6.markdown \
      07-customizing-git/01-chapter7.markdown \
      08-git-and-other-scms/01-chapter8.markdown \
      09-git-internals/01-chapter9.markdown

That's it!  The ebook, `progit.epub`, is ready to be uploaded to your reader.

## Changing the format

You can use the `--css` option to specify a CSS file
for the book.  The default CSS is minimal and can be found
[on GitHub](https://github.com/jgm/pandoc/blob/master/data/epub.css)
or in the `epub.css` file in your data directory
(see `--data-dir` in the [User's Guide]).

You can even embed fonts in the EPUB if you want; see the [User's Guide]
under `--epub-embed-font` for instructions.

## Math

Pandoc has an EPUB3 writer.  It renders LaTeX math into MathML, which
EPUB3 readers are supposed to support (but unfortunately few do).

Of course, this isn't much help if you want EPUB2 output (`pandoc -t epub2`)
or target readers that don't support MathML. Then you should try using the
`--webtex` option, which will use a web service to convert the TeX to an image.

[KindleGen]: https://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765211
[EPUB]:  https://en.wikipedia.org/wiki/EPUB
[Pro Git]: https://git-scm.com/book/en/v2
[Creative Commons]: https://creativecommons.org/
[buying a copy]: https://git-scm.com/book/en/v2
[github site]: https://github.com/progit/progit
[git]: https://git-scm.com
[Dublin Core metadata elements]: https://dublincore.org/documents/dces/
[User's Guide]: https://pandoc.org/MANUAL.html

