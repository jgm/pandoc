% MARKDOWN2PDF(1) Pandoc User Manuals
% John MacFarlane, Paulo Tanimoto, and Recai Oktas
% January 29, 2011

# NAME

markdown2pdf - converts markdown-formatted text to PDF, using pdflatex 

# SYNOPSIS

markdown2pdf [*options*] [*input-file*]...

# DESCRIPTION

`markdown2pdf` converts *input-file* (or text from standard 
input) from markdown-formatted plain text to PDF, using `pandoc`
and `pdflatex`. If no output filename is specified (using the `-o`
option), the name of the output file is derived from the input file;
thus, for example, if the input file is *hello.txt*, the output file
will be *hello.pdf*. If the input is read from STDIN and no output
filename is specified, the output file will be named *stdin.pdf*. If
multiple input files are specified, they will be concatenated before
conversion, and the name of the output file will be derived from the
first input file.

Input is assumed to be in the UTF-8 character encoding.  If your
local character encoding is not UTF-8, you should pipe input
through `iconv`:

    iconv -t utf-8 input.txt | markdown2pdf

`markdown2pdf` assumes that the `unicode`, `array`, `fancyvrb`,
`graphicx`, and `ulem` packages are in latex's search path. If these
packages are not included in your latex setup, they can be obtained from
<http://ctan.org>.

# OPTIONS

-o *FILE*, \--output=*FILE*
:   Write output to *FILE*.

\--strict
:   Use strict markdown syntax, with no extensions or variants.

-N, \--number-sections
:   Number section headings in LaTeX output.  (Default is not to number them.)

\--listings
:   Use listings package for LaTeX code blocks

\--template=*FILE*
:   Use *FILE* as a custom template for the generated document. Implies
    `-s`. See the section TEMPLATES in `pandoc`(1) for information about
    template syntax.  Use `pandoc -D latex` to print the default LaTeX
    template.

-V KEY=VAL, \--variable=*KEY:VAL*
:   Set the template variable KEY to the value VAL when rendering the
    document in standalone mode.  Use this to set the font size when
    using the default LaTeX template: `-V fontsize=12pt`.

-H *FILE*, \--include-in-header=*FILE*
:   Include (LaTeX) contents of *FILE* at the end of the header.  Implies
    `-s`.

-B *FILE*, \--include-before-body=*FILE*
:   Include (LaTeX) contents of *FILE* at the beginning of the document body.

-A *FILE*, \--include-after-body=*FILE*
:   Include (LaTeX) contents of *FILE* at the end of the document body.

\--bibliography=*FILE*
:   Specify bibliography database to be used in resolving
    citations. The database type will be determined from the
    extension of *FILE*, which may be `.xml` (MODS format),
    `.bib` (BibTeX format), or `.json` (citeproc JSON).

\--csl=*FILE*
:   Specify [CSL] style to be used in formatting citations and
    the bibliography. If *FILE* is not found, pandoc will look
    for it in

        $HOME/.csl

    in unix and

        C:\Documents And Settings\USERNAME\Application Data\csl

    in Windows. If the `--csl` option is not specified, pandoc
    will use a default style:  either `default.csl` in the
    user data directory (see `--data-dir`), or, if that is
    not present, the Chicago author-date style.

\--data-dir*=DIRECTORY*
:   Specify the user data directory to search for pandoc data files.
    If this option is not specified, the default user data directory
    will be used:

        $HOME/.pandoc

    in unix and

        C:\Documents And Settings\USERNAME\Application Data\pandoc

    in Windows. A `reference.odt`, `epub.css`, `templates` directory,
    or `s5` directory placed in this directory will override pandoc's
    normal defaults.

\--xetex
:   Use xelatex instead of pdflatex to create the PDF.

\--luatex
:   Use lualatex instead of pdflatex to create the PDF.

# SEE ALSO

`pandoc`(1), `pdflatex`(1)

[CSL]: CitationStyles.org

