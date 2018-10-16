This document provides a quick overview over the various ways to
customize pandoc's output. Follow the links to learn how to use each
approach.

[Templates](/MANUAL.html#templates)

:   Pandoc comes with a template for (almost) every output format. A
    template is a plain text file, that contains for example the line
    `$body$`. That variable is replaced by the document's body text on
    output.

    There are many other variables, like `title`, `header-includes`,
    etc. that are either set automatically, or that you can set using
    [YAML metadata blocks](/MANUAL.html#extension-yaml_metadata_block),
    [`--metadata`](/MANUAL.html#option--metadata) (which properly escape
    things), or `--variable` (which does no escaping). You can also
    generate your own template (e.g. `pandoc -D html > myletter.html`)
    and customize that file, for example by introducing new variables.

[reference.docx/pptx/odt](/MANUAL.html#option--reference-doc)

:   To output a `docx`, `pptx` or `odt` document, which is a ZIP of
    several files, things are a bit more complicated. Instead of a
    single template file, you need to provide a customized
    `reference.docx/pptx/odt`.

[Lua filters](lua-filters.html) and [filters](filters.html)

:   Templates are very powerful, but they are only a sort of scaffold to
    place your document's body text in. You cannot directly change the
    body text using the template (beyond e.g. adding CSS for HTML
    output, or `\renewcommand` for LaTeX output).

    If you need to affect the output of the actual body text, you
    probably need a pandoc filter. A filter is a small program, that
    transforms the document, between the parsing and the writing phase,
    while it is still in pandoc's native format -- an abstract syntax
    tree (AST), not unlike the HTML DOM. As can be seen in the [AST
    definition](https://hackage.haskell.org/package/pandoc-types/docs/Text-Pandoc-Definition.html)
    `Pandoc Meta [Block]`, a pandoc document is a chunk of metadata and
    a list of `Block`s.

    - There's a [list of third party filters on the
      wiki](https://github.com/jgm/pandoc/wiki/Pandoc-Filters).
    - Unless you have a good reason not to, it's best to write your
      own filter in the Lua scripting language. Since pandoc ships
      with a Lua interpreter, Lua filters are very portable and
      efficient. See [Lua filters](lua-filters.html).
    - For a gentle introduction into filters and writing them in any
      programming language, see [filters](filters.html).

Furthecustomizations

:   - [Custom Styles in Docx](/MANUAL.html#custom-styles-in-docx)
    - If you're converting from Markdown, see
      - [Generic raw attributes](/MANUAL.html#generic-raw-attribute):
        to include raw snippets
      - [Divs and Spans](/MANUAL.html#divs-and-spans): generic blocks
        that can be transformed with filters
    - [Custom syntax highlighting](/MANUAL.html#syntax-highlighting),
      provided by the [skylighting
      library](https://github.com/jgm/skylighting)
    - [Custom writers](/MANUAL.html#custom-writers)
    - [Pandoc Extras wiki page](https://github.com/jgm/pandoc/wiki/Pandoc-Extras)
