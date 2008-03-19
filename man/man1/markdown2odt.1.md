% MARKDOWN2ODT(1) Pandoc User Manuals
% John MacFarlane and Recai Oktas
% March 14, 2008

# NAME

markdown2odt - converts markdown-formatted text to ODT

# SYNOPSIS

markdown2odt [*options*] [*input-file*]...

# DESCRIPTION

`markdown2odt` converts *input-file* (or text from standard 
input) from markdown-formatted plain text to ODT (OpenDocument
Text) format. If no output filename is specified (using the `-o`
option), the name of the output file is derived from the input file;
thus, for example, if the input file is *hello.txt*, the output file
will be *hello.odt*. If the input is read from STDIN and no output
filename is specified, the output file will be named *stdin.odt*. If
multiple input files are specified, they will be concatenated before
conversion, and the name of the output file will be derived from the
first input file.

Input is assumed to be in the UTF-8 character encoding.  If your
local character encoding is not UTF-8, you should pipe input
through `iconv`:

    iconv -t utf-8 input.txt | markdown2odt

# OPTIONS

`markdown2odt` is a wrapper around `pandoc`, so all of
`pandoc`'s options can be used with `markdown2odt` as well.
See `pandoc`(1) for a complete list.
The following options are most relevant:

-o *FILE*, \--output=*FILE*
:   Write output to *FILE*.

\--strict
:   Use strict markdown syntax, with no extensions or variants.

-S, \--smart
:   Use smart quotes, dashes, and ellipses.  (This option is significant
    only when the input format is `markdown`.  It is selected automatically
    when the output format is `latex` or `context`.)

# SEE ALSO

`pandoc`(1)
