% HTML2MARKDOWN
% John MacFarlane and Recai Oktas
% June 30, 2007

# NAME

html2markdown - converts HTML to markdown-formatted text

# SYNOPSIS

**html2markdown [*pandoc-options*] [-- *special-options*] [*input-file* or
*URL*]**

# DESCRIPTION

`html2markdown` converts *input-file* or *URL* (or text
from STDIN) from HTML to markdown-formatted plain text. 
If a URL is specified, `html2markdown` uses an available program
(e.g. wget, w3m, lynx or curl) to fetch its contents.  Output is sent
to STDOUT unless an output file is specified using the `-o`
option.

`html2markdown` uses the character encoding specified in the
"Content-type" meta tag.  If this is not present, or if input comes
from STDIN, UTF-8 is assumed.  A character encoding may be specified
explicitly using the `-e` special option.

# OPTIONS

`html2markdown` is a wrapper for `pandoc`, so all of
`pandoc`'s options may be used.  See `pandoc`(1) for
a complete list.  The following options are most relevant:

-s, --standalone
:   Include title, author, and date information (if present) at the
    top of markdown output.

-o *FILE*, --output=*FILE*
:   Write output to *FILE* instead of STDOUT. 

--strict
:   Use strict markdown syntax, with no extensions or variants.

--reference-links
:   Use reference-style links, rather than inline links, in writing markdown
    or reStructuredText.

-R, --parse-raw
:   Parse untranslatable HTML codes as raw HTML.

-H *FILE*, --include-in-header=*FILE*
:   Include contents of *FILE* at the end of the header.  Implies
    `-s`.

-B *FILE*, --include-before-body=*FILE*
:   Include contents of *FILE* at the beginning of the document body.

-A *FILE*, --include-after-body=*FILE*
:   Include contents of *FILE* at the end of the document body.

-C *FILE*, --custom-header=*FILE*
Use contents of *FILE*
as the document header (overriding the default header, which can be
printed using `pandoc -D markdown`).  Implies
`-s`.

# SPECIAL OPTIONS

In addition, the following special options may be used.  The special
options must be separated from the `html2markdown` command and any
regular `pandoc` options by the delimiter '`--`', as in

    html2markdown -o foo.txt -- -g 'curl -u bar:baz' -e latin1  \
    www.foo.com

-e *encoding*, --encoding=*encoding* 
:   Assume the character encoding *encoding* in reading HTML.
    (Note: *encoding* will be passed to `iconv`; a list of
    available encodings may be obtained using `iconv -l`.)
    If this option is not specified and input is not from
    STDIN, `html2markdown` will try to extract the character encoding
    from the "Content-type" meta tag.  If no character encoding is
    specified in this way, or if input is from STDIN, UTF-8 will be
    assumed.

-g *command*, --grabber=*command*
:   Use *command* to fetch the contents of a URL.  (By default,
    `html2markdown` searches for an available program or text-based
    browser to fetch the contents of a URL.)

# SEE ALSO

`pandoc`(1), `iconv`(1)
