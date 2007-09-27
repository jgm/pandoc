% PANDOC(1) Pandoc User Manuals
% John MacFarlane
% June 30, 2007

# NAME

pandoc - general markup converter

# SYNOPSIS

pandoc [*options*] [*input-file*]...

# DESCRIPTION

Pandoc converts files from one markup format to another. It can
read markdown and (subsets of) reStructuredText, HTML, and LaTeX, and
it can write markdown, reStructuredText, HTML, LaTeX, ConTeXt, groff man,
RTF, DocBook XML, and S5 HTML slide shows.

If no *input-file* is specified, input is read from STDIN.
Otherwise, the *input-files* are concatenated (with a blank
line between each) and used as input.  Output goes to STDOUT by
default.  For output to a file, use the `-o` option:

    pandoc -o output.html input.txt

The input and output formats may be specified using command-line options
(see **OPTIONS**, below, for details).  If these formats are not
specified explicitly, Pandoc will attempt to determine them
from the extensions of the input and output filenames.  If input comes
from STDIN or from a file with an unknown extension, the input is assumed
to be markdown.  If no output filename is specified using the `-o`
option, or if a filename is specified but its extension is unknown,
the output will default to HTML.  Thus, for example,

    pandoc -o chap1.tex chap1.txt

converts *chap1.txt* from markdown to LaTeX.  And

    pandoc README

converts *README* from markdown to HTML.

Pandoc's version of markdown is an extended variant of standard
markdown: the differences are described in the *README* file in
the user documentation.  If standard markdown syntax is desired, the
`--strict` option may be used.

Pandoc uses the UTF-8 character encoding for both input and output.
If your local character encoding is not UTF-8, you should pipe input
and output through `iconv`:

    iconv -t utf-8 input.txt | pandoc | iconv -f utf-8

Pandoc's HTML parser is not very forgiving.  If your input is
HTML, consider running it through `tidy`(1) before passing it
to Pandoc.  Or use `html2markdown`(1), a wrapper around `pandoc`.

# OPTIONS

-f *FORMAT*, -r *FORMAT*, \--from=*FORMAT*, \--read=*FORMAT*
:   Specify input format.  *FORMAT* can be
    `native` (native Haskell), `markdown` (markdown or plain text),
    `rst` (reStructuredText), `html` (HTML), or `latex` (LaTeX).

-t *FORMAT*, -w *FORMAT*, \--to=*FORMAT*, \--write=*FORMAT*
:   Specify output format.  *FORMAT* can be `native` (native Haskell),
    `man` (groff man page),
    `markdown` (markdown or plain text), `rst` (reStructuredText),
    `html` (HTML), `latex` (LaTeX), `context` (ConTeXt), `man` (groff man), 
    `docbook` (DocBook XML), `s5` (S5 HTML and javascript slide show),
     or `rtf` (rich text format).

-s, \--standalone
:   Produce output with an appropriate header and footer (e.g. a
    standalone HTML, LaTeX, or RTF file, not a fragment).

-o *FILE*, \--output=*FILE*
:   Write output to *FILE* instead of STDOUT.  If *FILE* is
    \``-`', output will go to STDOUT.

-p, \--preserve-tabs
:   Preserve tabs instead of converting them to spaces.

\--tab-stop=*TABSTOP*
:   Specify tab stop (default is 4).

\--strict
:   Use strict markdown syntax, with no extensions or variants.

\--reference-links
:   Use reference-style links, rather than inline links, in writing markdown
    or reStructuredText.

-R, \--parse-raw
:   Parse untranslatable HTML codes and LaTeX environments as raw HTML
    or LaTeX, instead of ignoring them.

-S, \--smart
:   Use smart quotes, dashes, and ellipses.  (This option is significant
    only when the input format is `markdown`.  It is selected automatically
    when the output format is `latex` or `context`.)

-m*URL*, \--asciimathml=*URL*
:   Use ASCIIMathML to display embedded LaTeX math in HTML output.
    To insert a link to a local copy of the `ASCIIMathML.js` script,
    provide a *URL*. If no *URL* is provided, the contents of the
    script will be inserted directly into the HTML header.

-i, \--incremental
:   Make list items in S5 display incrementally (one by one).

-N, \--number-sections
:   Number section headings in LaTeX output.  (Default is not to number
    them.)

\--no-wrap
:   Disable text wrapping in Markdown, reStructuredText, DocBook, and man
    output.  (Default is to wrap text.)

\--toc, \--table-of-contents
:   Include an automatically generated table of contents (HTML, markdown,
    RTF) or an instruction to create one (LaTeX, reStructuredText).
    This option has no effect on man, DocBook, or S5 output.

-c *CSS*, \--css=*CSS*
:   Link to a CSS style sheet.  *CSS* is the pathname of the style sheet.

-H *FILE*, \--include-in-header=*FILE*
:   Include contents of *FILE* at the end of the header.  Implies `-s`.

-B *FILE*, \--include-before-body=*FILE*
:   Include contents of *FILE* at the beginning of the document body.

-A *FILE*, \--include-after-body=*FILE*
:   Include contents of *FILE* at the end of the document body.

-C *FILE*, \--custom-header=*FILE*
:   Use contents of *FILE* as the document header (overriding the
    default header, which can be printed by using the `-D` option).
    Implies `-s`.

-D *FORMAT*, \--print-default-header=*FORMAT*
:   Print the default header for *FORMAT* (`html`, `s5`, `latex`,
    `context`, `docbook`, `man`, `markdown`, `rst`, `rtf`).

-T *STRING*, \--title-prefix=*STRING*
:   Specify *STRING* as a prefix to the HTML window title.

\--dump-args
:   Print information about command-line arguments to STDOUT, then exit.
    The first line of output contains the name of the output file specified
    with the `-o` option, or \``-`' (for STDOUT) if no output file was
    specified.  The remaining lines contain the command-line arguments,
    one per line, in the order they appear.  These do not include regular
    Pandoc options and their arguments, but do include any options appearing
    after a \``--`' separator at the end of the line.
    This option is intended primarily for use in wrapper scripts.

\--ignore-args
:   Ignore command-line arguments (for use in wrapper scripts).
    Regular Pandoc options are not ignored.  Thus, for example,

:       pandoc --ignore-args -o foo.html -s foo.txt -- -e latin1

:   is equivalent to

:       pandoc -o foo.html -s

-v, \--version
:   Print version.

-h, \--help
:   Show usage message.

# SEE ALSO

`html2markdown`(1),
`markdown2pdf`(1).
The *README* file distributed with Pandoc contains full documentation.

The Pandoc source code and all documentation may be downloaded from
<http://johnmacfarlane.net/pandoc/>.

