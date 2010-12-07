% PANDOC(1) Pandoc User Manuals
% John MacFarlane
% January 8, 2008

# NAME

pandoc - general markup converter

# SYNOPSIS

pandoc [*options*] [*input-file*]...

# DESCRIPTION

Pandoc converts files from one markup format to another. It can
read markdown and (subsets of) Textile, reStructuredText, HTML, and LaTeX,
and it can write plain text, markdown, reStructuredText, HTML, LaTeX,
ConTeXt, Texinfo, groff man, MediaWiki markup, Textile, RTF,
OpenDocument XML, ODT, DocBook XML, EPUB, Emacs Org-Mode, and
Slidy or S5 HTML slide shows.

If no *input-file* is specified, input is read from *stdin*.
Otherwise, the *input-files* are concatenated (with a blank
line between each) and used as input.  Output goes to *stdout* by
default (though output to *stdout* is disabled for the `odt` and
`epub` output formats).  For output to a file, use the `-o` option:

    pandoc -o output.html input.txt

Instead of a file, an absolute URI may be given.  In this case
pandoc will fetch the content using HTTP:

    pandoc -f html -t markdown http://www.fsf.org

The input and output formats may be specified using command-line options
(see **OPTIONS**, below, for details).  If these formats are not
specified explicitly, Pandoc will attempt to determine them
from the extensions of the input and output filenames.  If input comes
from *stdin* or from a file with an unknown extension, the input is assumed
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
If your local character encoding is not UTF-8, you
should pipe input and output through `iconv`:

    iconv -t utf-8 input.txt | pandoc | iconv -f utf-8

# OPTIONS

`-f` *FORMAT*, `-r` *FORMAT*, `--from=`*FORMAT*, `--read=`*FORMAT*
:   Specify input format.  *FORMAT* can be
    `native` (native Haskell), `markdown` (markdown or plain text),
    `textile` (Textile), `rst` (reStructuredText), `html` (HTML),
    or `latex` (LaTeX).  If `+lhs` is appended to `markdown`, `rst`,
    or `latex`, the input will be treated as literate Haskell source:
    see [Literate Haskell support](#literate-haskell-support),
    below.

`-t` *FORMAT*, `-w` *FORMAT*, `--to=`*FORMAT*, `--write=`*FORMAT*
:   Specify output format.  *FORMAT* can be `native` (native Haskell),
    `plain` (plain text), `markdown` (markdown), `rst` (reStructuredText),
    `html` (HTML), `latex` (LaTeX), `context` (ConTeXt), `man` (groff man), 
    `mediawiki` (MediaWiki markup), `textile` (Textile), `org` (Emacs
    Org-Mode), `texinfo` (GNU Texinfo), `docbook` (DocBook XML),
    `opendocument` (OpenDocument XML), `odt` (OpenOffice text document),
    `epub` (EPUB book), `slidy` (Slidy HTML and javascript slide show),
    `s5` (S5 HTML and javascript slide show), or `rtf` (rich text
    format). Note that `odt` and `epub` output will not be directed to
    *stdout*; an output filename must be specified using the `-o/--output`
    option.  If `+lhs` is appended to `markdown`, `rst`, `latex`, or `html`,
    the output will be rendered as literate Haskell source:
    see [Literate Haskell support](#literate-haskell-support),
    below.

`-s`, `--standalone`
:   Produce output with an appropriate header and footer (e.g. a
    standalone HTML, LaTeX, or RTF file, not a fragment).

`-o` *FILE*, `--output=`*FILE*
:   Write output to *FILE* instead of *stdout*.  If *FILE* is
    `-`, output will go to *stdout*.  (Exception: if the output
    format is `odt` or `epub`, output to stdout is disabled.)

`-p`, `--preserve-tabs`
:   Preserve tabs instead of converting them to spaces (the default).

`--tab-stop=`*TABSTOP*
:   Specify the number of spaces per tab (default is 4).

`--strict`
:   Use strict markdown syntax, with no pandoc extensions or variants.
    When the input format is HTML, this means that constructs that have no
    equivalents in standard markdown (e.g. definition lists or strikeout
    text) will be parsed as raw HTML.

`--reference-links`
:   Use reference-style links, rather than inline links, in writing markdown
    or reStructuredText.  By default inline links are used.

`-R`, `--parse-raw`
:   Parse untranslatable HTML codes and LaTeX environments as raw HTML
    or LaTeX, instead of ignoring them.  Affects only HTML and LaTeX
    input. Raw HTML can be printed in markdown, reStructuredText, HTML, Slidy,
    and S5 output; raw LaTeX can be printed in markdown, reStructuredText,
    LaTeX, and ConTeXt output. The default is for the readers to omit
    untranslatable HTML codes and LaTeX environments. (The LaTeX reader
    does pass through untranslatable LaTeX *commands*, even if `-R` is not
    specified.)

`-S`, `--smart`
:   Produce typographically correct output, converting straight quotes
    to curly quotes, `---` and `--` to dashes, ande `...` to ellipses.
    Nonbreaking spaces are inserted after certain abbreviations, such
    as "Mr." (Note: This option is significant only when the input format is
    `markdown` or `textile`.  It is selected automatically when the input
    format is `textile` or the output format is `latex` or `context`.)

`-m` *URL*, `--latexmathml=`*URL*
:   Use the [LaTeXMathML] script to display embedded TeX math in HTML output.
    To insert a link to a local copy of the `LaTeXMathML.js` script,
    provide a *URL*. If no *URL* is provided, the contents of the
    script will be inserted directly into the HTML header, preserving
    portability at the price of efficiency. If you plan to use math on
    several pages, it is much better to link to a copy of the script,
    so it can be cached.

`--mathml`
:   Convert TeX math to MathML.  In standalone mode, a small javascript
    will be inserted that allows the MathML to be viewed on some browsers.

`--jsmath=`*URL*
:   Use [jsMath] to display embedded TeX math in HTML output.
    The *URL* should point to the jsMath load script (e.g.
    `jsMath/easy/load.js`); if provided, it will be linked to in
    the header of standalone HTML documents.

`--mathjax=`*URL*
:   Use [MathJax] to display embedded TeX math in HTML output.
    The *URL* should point to the `MathJax.js` load script.

`--gladtex`
:   Enclose TeX math in `<eq>` tags in HTML output.  These can then
    be processed by [gladTeX] to produce links to images of the typeset
    formulas. 

`--mimetex=`*URL*
:   Render TeX math using the [mimeTeX] CGI script.  If *URL* is not
    specified, it is assumed that the script is at `/cgi-bin/mimetex.cgi`.

`--webtex=`*URL*
:   Render TeX formulas using an external script that converts TeX
    formulas to images. The formula will be concatenated with the URL
    provided. If *URL* is not specified, the Google Chart API will be used.

`-i`, `--incremental`
:   Make list items in Slidy or S5 display incrementally (one by one).
    The default is for lists to be displayed all at once.

`--offline`
:   Include all the CSS and javascript needed for a Slidy or S5 slide
    show in the output, so that the slide show will work even when no
    internet connection is available.

`--xetex`
:   Create LaTeX outut suitable for processing by XeTeX.

`-N`, `--number-sections`
:   Number section headings in LaTeX, ConTeXt, or HTML output.
    By default, sections are not numbered.

`--section-divs`
:   Wrap sections in `<div>` tags, and attach identifiers to the
    enclosing `<div>` rather than the header itself.
    See [Section identifiers](#header-identifiers-in-html), below.

`--no-wrap`
:   Disable text wrapping in output. By default, text is wrapped
    appropriately for the output format.

`--sanitize-html`
:   Sanitizes HTML (in markdown or HTML input) using a whitelist.
    Unsafe tags are replaced by HTML comments; unsafe attributes
    are omitted.  URIs in links and images are also checked against a
    whitelist of URI schemes.

`--email-obfuscation=`*none|javascript|references*
:   Specify a method for obfuscating `mailto:` links in HTML documents.
    *none* leaves `mailto:` links as they are.  *javascript* obfuscates
    them using javascript. *references* obfuscates them by printing their
    letters as decimal or hexadecimal character references.
    If `--strict` is specified, *references* is used regardless of the
    presence of this option.

`--id-prefix`=*STRING*
:   Specify a prefix to be added to all automatically generated identifiers
    in HTML output.  This is useful for preventing duplicate identifiers
    when generating fragments to be included in other pages.

`--indented-code-classes=`*CLASSES*
:   Specify classes to use for indented code blocks--for example,
    `perl,numberLines` or `haskell`. Multiple classes may be separated
    by spaces or commas.

`--toc`, `--table-of-contents`
:   Include an automatically generated table of contents (or, in
    the case of `latex`, `context`, and `rst`, an instruction to create
    one) in the output document. This option has no effect on `man`,
    `docbook`, `slidy`, or `s5` output.

`--base-header-level=`*LEVEL*
:   Specify the base level for headers (defaults to 1).

`--template=`*FILE*
:   Use *FILE* as a custom template for the generated document. Implies
    `--standalone`. See [Templates](#templates) below for a description
    of template syntax. If this option is not used, a default
    template appropriate for the output format will be used. See also
    `-D/--print-default-template`.

`-V` *KEY=VAL*, `--variable=`*KEY:VAL*
:   Set the template variable *KEY* to the value *VAL* when rendering the
    document in standalone mode. This is only useful when the
    `--template` option is used to specify a custom template, since
    pandoc automatically sets the variables used in the default
    templates.

`-c` *URL*, `--css=`*URL*
:   Link to a CSS style sheet.

`-H` *FILE*, `--include-in-header=`*FILE*
:   Include contents of *FILE*, verbatim, at the end of the header.
    This can be used, for example, to include special
    CSS or javascript in HTML documents.  This option can be used
    repeatedly to include multiple files in the header.  They will be
    included in the order specified.  Implies `--standalone`.

`-B` *FILE*, `--include-before-body=`*FILE*
:   Include contents of *FILE*, verbatim, at the beginning of the
    document body (e.g. after the `<body>` tag in HTML, or the
    `\begin{document}` command in LaTeX). This can be used to include
    navigation bars or banners in HTML documents. This option can be
    used repeatedly to include multiple files. They will be included in
    the order specified.  Implies `--standalone`.

`-A` *FILE*, `--include-after-body=`*FILE*
:   Include contents of *FILE*, verbatim, at the end of the document
    body (before the `</body>` tag in HTML, or the
    `\end{document}` command in LaTeX). This option can be be used
    repeatedly to include multiple files. They will be included in the
    order specified.  Implies `--standalone`.

`-C` *FILE*, `--custom-header=`*FILE*
:   Use contents of *FILE* as the document header. Implies `--standalone`.
    *Note: This option is deprecated. Users should transition to using
    `--template` instead.*

`--reference-odt=`*FILE*
:   Use the specified file as a style reference in producing an ODT.
    For best results, the reference ODT should be a modified version
    of an ODT produced using pandoc.  The contents of the reference ODT
    are ignored, but its stylesheets are used in the new ODT. If no
    reference ODT is specified on the command line, pandoc will look
    for a file `reference.odt` in the user data directory (see
    `--data-dir`). If this is not found either, sensible defaults will be
    used.

`--epub-stylesheet=`*FILE*
:   Use the specified CSS file to style the EPUB.  If no stylesheet
    is specified, pandoc will look for a file `epub.css` in the
    user data directory (see `--data-dir`, below).  If it is not
    found there, sensible defaults will be used.

`--epub-metadata=`*FILE*
:   Look in the specified XML file for metadata for the EPUB.
    The file should contain a series of Dublin Core elements,
    as documented at <http://dublincore.org/documents/dces/>.
    For example:

         <dc:rights>Creative Commons</dc:rights>
         <dc:language>es-AR</dc:language>

    By default, pandoc will include the following metadata elements:
    `<dc:title>` (from the document title), `<dc:creator>` (from the
    document authors), `<dc:language>` (from the locale), and
    `<dc:identifier id="BookId">` (a randomly generated UUID). Any of
    these may be overridden by elements in the metadata file.

`-D` *FORMAT*, `--print-default-template=`*FORMAT*
:   Print the default template for an output *FORMAT*. (See `-t`
    for a list of possible *FORMAT*s.)

`-T` *STRING*, `--title-prefix=*STRING*
:   Specify *STRING* as a prefix at the beginning of the title
    that appears in the HTML header (but not in the title as it
    appears at the beginning of the HTML body).  Implies
    `--standalone`.

`--bibliography=`*FILE*
:   Specify bibliography database to be used in resolving
    citations. The database type will be determined from the
    extension of *FILE*, which may be `.mods` (MODS format),
    `.bib` (BibTeX format), `.bbx` (BibLaTeX format),
    `.ris` (RIS format), `.enl` (EndNote format),
    `.xml` (EndNote XML format), `.wos` (ISI format),
    `.medline` (MEDLINE format), `.copac` (Copac format),
    or `.json` (citeproc JSON).  If you want to use multiple
    bibliographies, just use this option repeatedly.

`--csl=`*FILE*
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

`--data-dir=`*DIRECTORY*
:   Specify the user data directory to search for pandoc data files.
    If this option is not specified, the default user data directory
    will be used:

        $HOME/.pandoc

    in unix and

        C:\Documents And Settings\USERNAME\Application Data\pandoc

    in Windows. A `reference.odt`, `epub.css`, `templates` directory,
    or `s5` directory placed in this directory will override pandoc's
    normal defaults.

`--dump-args`
:   Print information about command-line arguments to *stdout*, then exit.
    This option is intended primarily for use in wrapper scripts.
    The first line of output contains the name of the output file specified
    with the `-o` option, or `-` (for *stdout*) if no output file was
    specified.  The remaining lines contain the command-line arguments,
    one per line, in the order they appear.  These do not include regular
    Pandoc options and their arguments, but do include any options appearing
    after a `--` separator at the end of the line.

`--ignore-args`
:   Ignore command-line arguments (for use in wrapper scripts).
    Regular Pandoc options are not ignored.  Thus, for example,

        pandoc --ignore-args -o foo.html -s foo.txt -- -e latin1

    is equivalent to

        pandoc -o foo.html -s

`-v`, `--version`
:   Print version.

`-h`, `--help`
:   Show usage message.

[LaTeXMathML]: http://math.etsu.edu/LaTeXMathML/
[jsMath]:  http://www.math.union.edu/~dpvc/jsmath/
[MathJax]: http://www.mathjax.org/
[gladTeX]:  http://www.math.uio.no/~martingu/gladtex/index.html
[mimeTeX]: http://www.forkosh.com/mimetex.html 
[CSL]: http://CitationStyles.org

# TEMPLATES

When the `-s/--standalone` option is used, pandoc uses a template to
add header and footer material that is needed for a self-standing
document.  To see the default template that is used, just type

    pandoc --print-default-template=FORMAT

where `FORMAT` is the name of the output format. A custom template
can be specified using the `--template` option.  You can also override
the system default templates for a given output format `FORMAT`
by putting a file `templates/FORMAT.template` in the user data
directory (see `--data-dir`, below).

Templates may contain *variables*.  Variable names are sequences of
alphanumerics, `-`, and `_`, starting with a letter.  A variable name
surrounded by `$` signs will be replaced by its value.  For example,
the string `$title$` in

    <title>$title$</title>

will be replaced by the document title.

To write a literal `$` in a template, use `$$`.

Some variables are set automatically by pandoc.  These vary somewhat
depending on the output format, but include:

`legacy-header`
:   contents specified by `-C/--custom-header`
`header-includes`
:   contents specified by `-H/--include-in-header` (may have multiple
    values)
`toc`
:   non-null value if `--toc/--table-of-contents` was specified
`include-before`
:   contents specified by `-B/--include-before-body` (may have
    multiple values)
`include-after`
:   contents specified by `-A/--include-after-body` (may have
    multiple values)
`body`
:   body of document
`title`
:   title of document, as specified in title block
`author`
:   author of document, as specified in title block (may have
    multiple values)
`date`
:   date of document, as specified in title block

Variables may be set at the command line using the `-V/--variable`
option. This allows users to include custom variables in their
templates.

Templates may contain conditionals.  The syntax is as follows:

    $if(variable)$
    X 
    $else$
    Y
    $endif$

This will include `X` in the template if `variable` has a non-null
value; otherwise it will include `Y`. `X` and `Y` are placeholders for
any valid template text, and may include interpolated variables or other
conditionals. The `$else$` section may be omitted.

When variables can have multiple values (for example, `author` in
a multi-author document), you can use the `$for$` keyword:

    $for(author)$
    <meta name="author" content="$author$" />
    $endfor$

You can optionally specify a separator to be used between
consecutive items:

    $for(author)$$author$$sep$, $endfor$

# SEE ALSO

`markdown2pdf` (1).
The *README* file distributed with Pandoc contains full documentation.

The Pandoc source code and all documentation may be downloaded from
<http://johnmacfarlane.net/pandoc/>.

[CSL]: CitationStyles.org

