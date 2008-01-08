% MARKDOWN2PDF(1) Pandoc User Manuals
% John MacFarlane and Recai Oktas
% January 8, 2008

# NAME

markdown2pdf - converts markdown-formatted text to PDF, using pdflatex 

# SYNOPSIS

markdown2pdf [*options*] [*input-file*]...

# DESCRIPTION

`markdown2pdf` converts *input-file* (or text from standard 
input) from markdown-formatted plain text to PDF, using `pdflatex`.
If no output filename is specified (using the `-o` option),
the name of the output file is derived from the input file; thus, for
example, if the input file is *hello.txt*, the output file will be
*hello.pdf*.  If the input is read from STDIN and no output filename
is specified, the output file will be named *stdin.pdf*.  If multiple
input files are specified, they will be concatenated before conversion,
and the name of the output file will be derived from the first input file.

Input is assumed to be in the UTF-8 character encoding.  If your
local character encoding is not UTF-8, you should pipe input and
output through `iconv`:

    iconv -t utf-8 input.txt | pandoc | iconv -f utf-8

`markdown2pdf` assumes that the `unicode`, `array`, `fancyvrb`,
`graphicx`, and `ulem` packages are in latex's search path. If these
packages are not included in your latex setup, they can be obtained from
<http://ctan.org>.

# OPTIONS

`markdown2pdf` is a wrapper around `pandoc`, so all of
`pandoc`'s options can be used with `markdown2pdf` as well.
See `pandoc`(1) for a complete list.
The following options are most relevant:

-o *FILE*, \--output=*FILE*
:   Write output to *FILE*.

\--strict
:   Use strict markdown syntax, with no extensions or variants.

-N, \--number-sections
:   Number section headings in LaTeX output.  (Default is not to number them.)

-H *FILE*, \--include-in-header=*FILE*
:   Include (LaTeX) contents of *FILE* at the end of the header.  Implies
    `-s`.

-B *FILE*, \--include-before-body=*FILE*
:   Include (LaTeX) contents of *FILE* at the beginning of the document body.

-A *FILE*, \--include-after-body=*FILE*
:   Include (LaTeX) contents of *FILE* at the end of the document body.

-C *FILE*, \--custom-header=*FILE*
:   Use contents of *FILE*
    as the LaTeX document header (overriding the default header, which can be
    printed using `pandoc -D latex`).  Implies `-s`.

# SEE ALSO

`pandoc`(1), `pdflatex`(1)
