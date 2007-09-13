% HSMARKDOWN(1) Pandoc User Manuals
% John MacFarlane
% June 30, 2007

# NAME

hsmarkdown - convert markdown-formatted text to HTML

# SYNOPSIS

hsmarkdown [*input-file*]...

# DESCRIPTION

`hsmarkdown` converts markdown-formatted text to HTML. It is designed
to be usable as a drop-in replacement for John Gruber's `Markdown.pl`.

If no *input-file* is specified, input is read from STDIN.
Otherwise, the *input-files* are concatenated (with a blank
line between each) and used as input.  Output goes to STDOUT by
default.  For output to a file, use shell redirection:

    hsmarkdown input.txt > output.html

`hsmarkdown` uses the UTF-8 character encoding for both input and output.
If your local character encoding is not UTF-8, you should pipe input
and output through `iconv`:

    iconv -t utf-8 input.txt | hsmarkdown | iconv -f utf-8

`hsmarkdown` is implemented as a wrapper around `pandoc`(1).  It
calls `pandoc` with the options `--from markdown --to html
--strict` and disables all other options.  (Command-line options
will be interpreted as filenames, as they are by `Markdown.pl`.)

# SEE ALSO

`pandoc`(1).  The *README*
file distributed with Pandoc contains full documentation.

The Pandoc source code and all documentation may be downloaded from
<http://johnmacfarlane.net/pandoc/>.
