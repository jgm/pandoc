# Revision history for pandoc

## pandoc 2.14.1 (2021-07-18)

  * Text.Pandoc.ImageSize: Add Tiff constructor for ImageType (#7405)
    [Minor API change].  This allows pandoc to get size information from
    tiff images.

  * Markdown reader:  don't try to read contents in self-closing HTML tag.
    Previously we had problems parsing raw HTML with self-closing
    tags like `<col/>`. The problem was that pandoc would look
    for a closing tag to close the markdown contents, but the
    closing tag had, in effect, already been parsed by `htmlTag`.

  * LaTeX reader:

    + Avoid trailing hyphen in translating languages (#7447).
      Previously `\foreignlanguage{english}` turned into `<span lang="en-">`.
      The same issue affected Arabic.
    + Support `\cline` in LaTeX tables (#7442).
    + Improved parsing of raw LaTeX from Text streams (`rawLaTeXParser`,
      used to read LaTeX in Markdown files, #7434).  We now use source
      positions from the token stream to tell us how much of the text stream
      to consume.  Getting this to work required a few other changes to
      make token source positions accurate.

  * DocBook reader:

    + Handle images with imageobjectco elements (#7440).
    + Add support for citerefentry (#7437, Jan Tojnar).

  * RST reader: fix regression with code includes (#7436).
    With the recent changes to include infrastructure,
    included code blocks were getting an extra newline.

  * HTML reader:

    + Recognize data-external when reading HTML img tags (#7429,
      Michael Hoffmann).  Preserve all attributes in img tags.  If attributes
      have a `data-` prefix, it will be stripped.  In particular, this
      preserves a `data-external` attribute as an `external` attribute in
      the pandoc AST.
    + Add col, colgroup to 'closes' definitions

  * HTML writer:

    + Remove duplicated alt text in HTML output (Aner Lucero).
    + Remove `aria-hidden` when explicit alt text is provided (Aner Lucero).
    + Set boolean values for reveal.js variables.

  * Docx writer:

    + Add table numbering for captioned tables.
      The numbers are added using fields, so that Word can
      create a list of tables that will update automatically.
    + Support figure numbers.  These are set up in such a way that they
      will work with Word's automatic table of figures (#7392).

  * Markdown writer: put space between Plain and following fenced Div
    (#4465).

  * EPUB writer: Don't incorporate externally linked images in EPUB documents
    (#7430, Michael Hoffmann).  Just as it is possible to avoid incorporating
    an image in EPUB by passing `data-external="1"` to a raw HTML snippet,
    this makes the same possible for native Images, by looking for an
    associated `external` attribute.

  * Text.Pandoc.PDF:

    + Fix `svgIn` path error (#7431).  We were duplicating
      the temp directory; this didn't cause problems on macOS or linux
      because there we use absolute paths for the temp directory.
      But on Windows it caused errors converting SVG files.
    + `convertImage`: normalize paths (#7431).  This will avoid paths
      on Windows with mixed path separators.

  * Text.Pandoc.Class: Always use / when adding directory to image destination
    with `extractMedia`, even on Windows.

  * Text.Pandoc.Citeproc:

    + Allow `$` characters in bibtex keys (#7409).
    + Set proper initial source name in parsing BibTeX (for better error
      messages.)
    + Revamp note citation handling (#7394).  Use latest
      citeproc, which uses a Span with a class rather than a Note for notes.
      This helps us distinguish between user notes and citation notes.  Don't
      put citations at the beginning of a note in parentheses.  Fix small bug
      in handling of citations in notes, which led to commas at the end of
      sentences in some cases.
    + Cleanup and efficiency improvement in `deNote`.
    + Improve punctuation moving with `--citeproc`.  Previously, using
      `--citeproc` could cause punctuation to move in quotes even when
      there aer no citations. This has been changed; punctuation moving
      is now limited to citations.  In addition, we only move footnotes
      around punctuation if the style is a note style, even if
      `notes-after-punctuation` is `true`.

  * Use citeproc 0.10. This helps improve note citations (see above)
    and eliminates double hyperlinks in author-in-text citations.
    Author-only citations are no longer hyperlinked.  See jgm/citeproc#77.
    It also fixes moving of punctuation inside quotes to conform to
    the CSL spec: only comma and period are moved, not question
    mark or exclamation point.

  * Text.Pandoc.Error: fix line calculations in reporting parsec errors.
    Also remove a spurious initial newline in the error report.

  * Use doctemplates 0.4.1, which gives us better support for boolean
    variable values.  Previously `$if(foo)$` would evaluate to true
    for variables with boolean `false` values, because it cared only
    about the string rendering (#7402).

  * Require commonmark-pandoc >= 0.2.2.1.
    This fixes task lists with multiple paragraphs.

  * Use skylighting 0.11.

  * CSS in HTML template: reset overflow-wrap on code blocks
    (Mauro Bieg, #7423).

  * LaTeX template: Revert change in PR #7295: "move title, author, date up
    to top of preamble." The change caused problem for people who used
    LaTeX commands defined defined later in the preamble in the title
    or author fields (#7422).

  * Add `doc/faqs.md`.  This is imported from the website; in the future the
    website version will be drawn from here.
    Added a FAQ on the use of `\AtEndPreamble` for cases when the contents of
    `header-includes` need to refer to definitions that come later in the
    preamble.  See #7422.

  * Upgrade Debian 10 AMI for build-arm.sh.

  * CircleCI: change to using xcode 11.1.0 (macOS 10.14.4).
    We previously built on 10.13, but 10.13 no longer gets
    security updates and CirclCI is deprecating.

## pandoc 2.14.0.3 (2021-06-22)

  * Text.Pandoc.MediaBag `insertMediaBag`: ensure we get a sane mediaPath
    for URLs (#7391).  In earlier 2.14.x versions, we'd get
    incorrect paths for resources downloaded from URLs when the
    media are extracted (including in PDF production).
  * Text.Pandoc.Parsing: improve `emailAddress` (#7398).
    Previously the parser would accept characters in domains
    that are illegal in domains, and this sometimes caused it
    to gobble bits of the following text.
  * txt2tags reader: modify the email address parser so
    it still includes form parameters, even after the change to
    `emailAddress` in Text.Pandoc.Parsing.
  * Text.Pandoc.Readers.Metadata: Fix regression with comment-only YAML
    metadata blocks (#7400).
  * reveal.js writer and template: better handling of options.  Previously
    it was impossible to specify false values for options that default to
    true (e.g. `center`); setting the option to false just caused the portion
    of the template setting the option to be omitted.  Now we prepopulate
    all the variables with their default values, including them all
    unconditionally and allowing them to be overridden.
  * Markdown writer: Fix regression in code blocks with attributes (#7397).
    Code blocks with a single class but nonempty attributes
    were having attributes drop as a result of #7242.
  * LaTeX writer:
    + Add strut at end of minipage if it contains line breaks.
      Without them, the last line is not as tall as it should be in
      some cases.
    + Always use a minipage for cells with line breaks, when
      width information is available (#7393).  Otherwise the way we treat them
      can lead to content that overflows a cell.
    + Use `\strut` instead of `~` before `\\` in empty line.
  * Use lts-18.0 stack resolver.
  * Require skylighting 0.10.5.2 (adding support for Swift).
  * Require commonmark 0.2.1.
  * Rephrase section on unsafe HTML in manual.
  * Create SECURITY.md

## pandoc 2.14.0.2 (2021-06-13)

  * Fix MediaBag regressions (#7345). iIn the 2.14 release `--extract-media`
    stopped working as before; there could be mismatches between the
    paths in the rendered document and the extracted media.
    This patch makes several changes that restore the earlier behavior
    (while keeping the same API).  The `mediaPath` in 2.14 was always
    constructed from the SHA1 hash of the media contents.  Now, we
    preserve the original path unless it's an absolute path or contains
    `..` segments (in that case we use a path based on the SHA1 hash of
    the contents).

    In Text.Pandoc.MediaBag, `mediaDirectory` and `mediaItems` now use the
    `mediaPath`, rather than the mediabag key, for the first component of the
    tuple.  This makes more sense, I think, and fits with the documentation of
    these functions; eventually, though, we should rework the API so that
    `mediaItems` returns both the keys and the MediaItems.

    In Text.Pandoc.Class.IO, rewriting of source paths in `extractMedia` has
    been fixed.

    In Text.Pandoc.Class.PandocMonad, `fillMediaBag` has been modified so that
    it doesn't modify image paths (that was part of the problem in #7345).

    We now do path normalization (e.g. `\` separators on Windows) in
    writing the media.

  * Text.Pandoc.PDF:

    + Text.Pandoc.PDF: Fix regression in 2.14 for generation of PDFs with
      SVGs (#7344).
    + Only print relevant part of environment on `--verbose`.  Since
      `--verbose` output might be put in an issue, we want to avoid
      spilling out secrets in environment variables.

  * Markdown reader: fix pipe table regression in 2.11.4 (#7343).
    Previously pipe tables with empty headers (that is, a header
    line with all empty cells) would be rendered as headerless
    tables.  This broke in 2.11.4.  The fix here is to produce an
    AST with an empty table head when a pipe table has all empty
    header cells.

  * LaTeX reader: don't allow optional `*` on symbol control sequences
    (#7340).  Generally we allow optional starred variants of LaTeX commands
    (since many allow them, and if we don't accept these explicitly,
    ignoring the star usually gives acceptable results).  But we
    don't want to do this for `\(*\)` and similar cases.

  * Docx reader: handle absolute URIs in Relationship Target (#7374).

  * Docx writer: fix handling of empty table headers (Albert Krewinkel,
    #7369).  A table header which does not contain any cells is now treated as
    an empty header.

  * LaTeX writer: Fix regression in table header position (#7347).
    In recent versions the table headers were no longer bottom-aligned
    (if more than one line).  This patch fixes that by using minipages
    for table headers in non-simple tables.

  * CommonMark writer:

    + Do not use simple class for fenced-divs (Jan Tojnar, amends #7242.)
    + Do not throw away attributes when `Ext_attributes` is enabled.
      `Ext_attributes` covers at least the following:
      `Ext_fenced_code_attributes`, `Ext_header_attributes`,
      `Ext_inline_code_attributes`, `Ext_link_attributes`.

  * Markdown writer:

    + Allow `pipe_tables` to be disabled for commonmark formats
      (`commonmark_x`, `gfm`) (#7375).
    + Re-use functions from Text.Pandoc.Markdown.Inline (Jan Tojnar).

  * DocBook writer: Remove non-existent admonitions (Jan Tojnar).
    `attention`, `error` and `hint` are reStructuredText specific.

  * HTML writer: Don't omit width attribute on div (#7342).

  * Text.Pandoc.MIME, `extensionFromMimeType`: add a few special cases.
    When we do a reverse lookup in the MIME table, we just get the
    last match, so when the same mime type is associated with several
    different extensions, we sometimes got weird results, e.g. `.vs`
    for `text/plain`.  These special cases help us get the most standard
    extensions for mime types like `text/plain`.

  * Lua utils: fix handling of table headers in `from_simple_table` (Albert
    Krewinkel, #7369).  Passing an empty list of header cells now results
    in an empty table header.

  * Text.Pandoc.Citeproc:

    + Avoid duplicate classes and attributes on references div.
    + Fix regression in citeproc processing (#7376).  If inline
      references are used (in the metadata `references` field), we
      should still only include in the bibliography items that are
      actually cited (unless `nocite` is used).

  * Require citeproc 0.4.0.1.  This fixes a bug which led to doubled
    "et al." in some (rare) circumstances.

  * MANUAL.txt:

    + Mention GladTeX for EPUB export (Sebastian Humenda).
      This updates the manual and the web site about the GladTeX usage.
    + More details and a useful link for YAML syntax.

  * CONTRIBUTING.md: update modules overview (Albert Krewinkel).

  * using-the-pandoc-api.md: switch from String to Text (Albert Krewinkel).


## pandoc 2.14.0.1 (2021-06-01)

  * Commonmark reader: Fix regression in 2.14 with YAML metdata block parsing,
    which could cause the document body to be omitted after metadata (#7339).

  * HTML reader: fix column width regression in 2.14 (#7334).
    Column widths specified with a style attribute were off by a factor of 100.

  * Markdown reader: in `rebasePaths`, check for both Windows and Posix
    absolute paths.  Previously Windows pandoc was treating
    `/foo/bar.jpg` as non-absolute.

  * Text.Pandoc.Logging: In rendering `LoadedResource`, use relative paths.

  * Docx writer: fix regression on captions (#7328).  The "Table Caption"
    style was no longer getting applied.  (It was overwritten by "Compact.")

  * Use commonmark-extensions 0.2.1.2

## pandoc 2.14 (2021-05-28)

  * Change reader types, allowing better tracking of source positions
    [API change].  Previously, when multiple file arguments were provided,
    pandoc simply concatenated them and passed the contents to the readers,
    which took a Text argument.  As a result, the readers had no way of knowing
    which file was the source of any particular bit of text.  This meant that
    we couldn't report accurate source positions on errors or include accurate
    source positions as attributes in the AST.  More seriously, it meant that
    we couldn't resolve resource paths relative to the files containing them
    (see e.g. #5501, #6632, #6384, #3752).

  * Add `rebase_relative_paths` extension (#3752).  When enabled, this
    extension rewrites relative image and link paths by prepending
    the (relative) directory of the containing file.  This
    behavior is useful when your input sources are split
    into multiple files, across several directories, with files
    referring to images stored in the same directory.  The
    extension can be enabled for all markdown and commonmark-based formats.

  * Add Text.Pandoc.Sources (exported module), with a `Sources` type and a
    `ToSources` class.  A `Sources` wraps a list of `(SourcePos, Text)` pairs
    [API change]. A parsec `Stream` instance is provided for `Sources`.  The
    module also exports versions of parsec's `satisfy` and other Char parsers
    that track source positions accurately from a `Sources` stream (or any
    instance of the new `UpdateSourcePos` class).

  * Text.Pandoc.Parsing

    + Export the modified Char parsers defined in Text.Pandoc.Sources
      instead of the ones parsec provides.  Modified parsers to use a
      `Sources` as stream [API change].
    + Improve include file functions [API change].  Remove old
      `insertIncludedFileF`.  Give `insertIncludedFile` a more general type,
      allowing it to be used where `insertIncludedFileF` was.
    + Add parameter to the `citeKey` parser from
      Text.Pandoc.Parsing, which controls whether the `@{..}`
      syntax is allowed [API change].

  * Text.Pandoc.Error: Modified the constructor `PandocParsecError` to take a
    `Sources` rather than a `Text` as first argument, so parse error locations
    can be accurately reported.

  * Fix source position reporting for YAML bibliographies (#7273).

  * Issue error message when  reader or writer format is malformed
    (#7231).  Previously we exited with an error status but (due to a bug)
    no message.

  * Smarter smart quotes (#7216, #2103).  Treat a leading `"` with no
    closing `"` as a left curly quote.  This supports the practice, in
    fiction, of continuing paragraphs quoting the same speaker without an
    end quote.  It also helps with quotes that break over lines in line blocks.

  * Markdown reader:

    + Use MetaInlines not MetaBlocks for multimarkdown metadata fields.
      This gives better results in converting to e.g.  pandoc markdown.
    + Implement curly-brace syntax for Markdown citation keys (#6026).
      The change provides a way to use citation keys that contain
      special characters not usable with the standard citation key syntax.
      Example: `@{foo_bar{x}'}` for the key `foo_bar{x}`.  It also allows
      separating citation keys from immediately following text, e.g. `@{foo}A`.

  * RST reader:

    + Seek include files in the directory of the file
      containing the include directive, as RST requires (#6632).
    + Use `insertIncludedFile` from Text.Pandoc.Parsing
      instead of reproducing much of its code.

  * Org reader: Resolve org includes relative to the directory containing the
    file containing the INCLUDE directive (#5501).

  * ODT reader: Treat tabs as spaces (#7185, niszet).

  * Docx reader:

    + Add handling of vml image objects (#7257, mbrackeantidot).
    + Support new table features (Emily Bourke, #6316):  column
      spans, row spans, multiple header rows, table description
      (parsed as a simple caption), captions, column widths.

  * LaTeX reader:

    + Improved siunitx support (#6658, #6620).
    + Better support for `\xspace` (#7299).
    + Improve parsing of `\def` macros.  We previously set "verbatim mode"
      even for parsing the initial `\def`; this caused problems
      for `\def` nested inside another `\def`.
    + Implement `\newif`.

  * ConTeXt writer: improve ordered lists (#5016, Denis Maier).
    Change ordered list from itemize to enumerate.  Add new
    itemgroup for ordered lists.  Remove manual insertion of
    width attributes.  Use tabular figures in ordered list
    enumerators.

  * HTML reader:

    + Don't fail on unmatched closing "script" tag (Albert Krenkel, #7282).
    + Keep h1 tags as normal headers (#2293, Albert Krewinkel).
      The tags `<title>` and `<h1 class="title">` often contain the same
      information, so the latter was dropped from the document. However, as
      this can lead to loss of information, the heading is now always
      retained.  Use `--shift-heading-level-by=-1` to turn the `<h1>`
      into the document title, or a filter to restore the previous behavior.
    + Handle relative lengths (e.g. `2*`) in HTML column widths (#4063).
      See <https://www.w3.org/TR/html4/types.html#h-6.6>.

  * DocBook/JATS readers:

    + Fix mathml regression caused by the switch in XML libraries (#7173).
    + Fix "phrase" in DocBook: take classes from "role" not "class" (#7195).

  * DocBook reader: ensure that first and last names are separated (#6541).

  * Jira reader (Albert Krewinkel, #7218):

    + Support "smart" links: `[alias|https://example.com|smart-card]` syntax.
    + Allow spaces and most unicode characters in attachment links.
    + No longer require a newline character after `{noformat}`.
    + Only allow URI path segment characters in bare links.
    + The `file:` schema is no longer allowed in bare links; these
      rarely make sense.

  * Plain writer: handle superscript unicode minus (#7276).

  * LaTeX writer:

    + Better handling of line breaks in simple tables (#7272).
      Now we also handle the case where they're embedded in other elements,
      e.g. spans.
    + For beamer output, support `exampleblock` and `alertblock` (#7278).
      A block will be rendered as an `exampleblock` if the heading
      has class `example` and an `alertblock` if it has class `alert`.
    + Separate successive quote chars with thin space (#6958,
      Albert Krewinkel).  Successive quote characters are separated with
      a thin space to improve readability and to prevent unwanted ligatures.
      Detection of these quotes sometimes had failed if the second quote
      was nested in a span element.
    + Separate successive quote chars with thin space (#6958, Albert
      Krewinkel).

  * EPUB Writer: Fix belongs-to-collection XML id choice (#7267, nuew).
    The epub writer previously used the same XML id for both the book
    identifier and the epub collection. This causes an error on epubcheck.

  * BibTeX/BibLaTeX writer: Handle `annote` field (#7266).

  * ZimWiki writer: allow links and emphasis in headers (#6605,
    Albert Krewinkel).

  * ConTeXt writer:

    + Support blank lines in line blocks (#6564, Albert Krewinkel,
      thanks to @denismaier).
    + Use span identifiers as reference anchors (#7246, Albert Krewinkel).

  * HTML writer:

    + Keep attributes from code nested below `pre` tag (#7221,
      Albert Krewinkel).  If a code block is defined with `<pre><code
      class="language-x">…</code></pre>`, where the `<pre>` element has no
      attributes, then the attributes from the `<code>` element are used
      instead. Any leading `language-` prefix is dropped in the code's
      `class` attribute are dropped to improve syntax highlighting.
    + Ensure headings only have valid attribs in HTML4 (#5944, Albert
      Krewinkel).
    + Parse `<header>` as a Div (Albert Krewinkel).

  * Org writer:

    + Inline latex envs need newlines (#7252, tecosaur).
      As specified in https://orgmode.org/manual/LaTeX-fragments.html, an
      inline \begin{}...\end{} LaTeX block must start on a new line.
    + Use LaTeX style maths deliminators (#7196, tecosaur).

  * JATS writer (Albert Krewinkel):

    + Use either styled-content or named-content for spans (#7211).
      If the element has a content-type attribute, or at least one class,
      then that value is used as `content-type` and the span is put inside
      a `<named-content>` element. Otherwise a `<styled-content>` element
      is used instead.
    + Reduce unnecessary use of `<p>` elements for wrapping (#7227).
      The `<p>` element is used for wrapping in cases were the contents
      would otherwise not be allowed in a certain context. Unnecessary
      wrapping is avoided, especially around quotes (`<disp-quote>` elements).
    + Convert spans to `<named-content>` elements (#7211).  Spans with
      attributes are converted to `<named-content>` elements instead of
      being wrapped with `<milestone-start/>` and `<milestone-end>`
      elements. Milestone elements are not allowed in documents using the
      articleauthoring tag set, so this change ensures the creation of valid
      documents.
    + Add footnote number as label in backmatter (#7210).  Footnotes in the
      backmatter are given the footnote's number as a label.  The
      articleauthoring output is unaffected from this change, as footnotes
      are placed inline there.
    + Escape disallows chars in identifiers.  XML identifiers must start
      with an underscore or letter, and can contain only a limited set
      of punctuation characters. Any IDs not adhering to these rules are
      rewritten by writing the offending characters as `Uxxxx`,
      where `xxxx` is the character's hex code.

  * Jira writer:  use `{color}` when span has a color attribute
    (Albert Krewinkel, tarleb/jira-wiki-markup#10).

  * Docx writer:

    + Autoset table width if no column has an explicit width (Albert
      Krewinkel).
    + Extract Table handling into separate module (Albert Krewinkel).
    + Support colspans and rowspans in tables (Albert Krewinkel, #6315).
    + Support multirow table headers (Albert Krewinkel).
    + Improve integration of settings from reference.docx (#1209).
      This change allows users to create a reference.docx that
      sets `w:proofState` for spelling or grammar to `dirty`,
      so that spell/grammar checking will be triggered on the
      generated docx.
    + Copy over more settings from reference.docx (#7240).  From settings.xml
      in the reference-doc, we now include: `zoom`, `embedSystemFonts`,
      `doNotTrackMoves`, `defaultTabStop`, `drawingGridHorizontalSpacing`,
      `drawingGridVerticalSpacing`, `displayHorizontalDrawingGridEvery`,
      `displayVerticalDrawingGridEvery`, `characterSpacingControl`,
      `savePreviewPicture`, `mathPr`, `themeFontLang`, `decimalSymbol`,
      `listSeparator`, `autoHyphenation`, `compat`.
    + Set zoom to 100% by default in settings.xml.
    + Align math options more with current Word defaults (e.g.  Cambria Math
      font).
    + Remove `rsid`s from default settings.xml.  Word will add these
      when revisions are made.

  * Ms writer: Handle tables with multiple paragraphs (#7288).
    Previously they overflowed the table cell width.  We now set line lengths
    per-cell and restore them after the table has been written.

  * Markdown writer:

    + Use cleaner braceless syntax for code blocks with a
      single class (#7242, Jan Tojnar).
    + Add quotes properly in markdown YAML metadata fields (#7245).
      This fixes a bug, which caused the writer to look at the *last*
      rather than the *first* character in determining whether quotes
      were needed.  So we got spurious quotes in some cases and
      didn't get necessary quotes in others.
    + Use `@{..}` syntax for citations when needed.
    + Use fewer unneeded escapes for `#` (see #6259).
    + Improve escaping of `@`.  We need to escape literal `@` before
      `{` because of the new citation syntax.

  * Commonmark writer: Use backslash escapes for `<` and `|`...
    instead of entities (#7208).

  * Powerpoint writer: allow `monofont` to be specified in metadata
    (#7187).

  * LaTeX template:

    + Use non-starred names for xcolor color names (#6109).
      This should make svgnames and x11names work properly.
    + Fix bad vertical spacing after bibliography (#7234, badumont).
    + List of figures before list of tables (#7235, Julien Dutant).
    + Move CSL macro definitions before header-includes so they can be
      overridden (#7286).
    + Improve treatment of CSL `entry-spacing` (#7296).
      Previously with the default template settings (`indent` variable
      not set), we would get interparagraph spaces separating bib
      entries even with `entry-spacing="0"`.  On the other hand,
      setting `entry-spacing="2"` gave ridiculously large spacing.
      This change makes the spacing caused by `entry-spacing` a multiple
      of `\parskip` by default, which gives aesthetically reasonable
      output.  Those who want a larger or smaller unit (e.g. because
      they use `indent` which sets `\parskip` to 0) may
      `\setlength{\cslentryspacingunit}{10pt}` in header-includes
      to override the defaults.
    + Move title, author, date up to top of preamble (#7295).
      This allows header-includes to use them, and puts them
      in a position where you can see them immediately.
    + Define commands for zero width non-joiner character
      (#6639, Albert Krewinkel).  The zero-width non-joiner character
      is used to avoid ligatures (e.g. in German).

  * ConTeXt template:

    + Define `enumerate` itemgroup (#5016, Denis Maier).
    + List of figures before list of tables (#7235, Julien Dutant).

  * reveal.js template:

    + Support `toc-title` (#7171, Florian Kohrt).
    + Use `hash: true` by default rather than `history: true` (#6968).

  * HTML-based slide shows: add support for `institute` (#7289, Thomas
    Hodgson).

  * Text.Pandoc.Extensions: Add constructor `Ext_rebase_relative_paths` to
    `Extensions` [API change].

  * Text.Pandoc.XML.Light: add Eq, Ord instances for Content,
    Element, Attr, CDataKind [API change].

  * Text.Pandoc.MediaBag:

    + Change type to use a `Text` key instead of `[FilePath]`.
      We normalize the path and use `/` separators for consistency.
    + Export `MediaItem` type [API change].
    + Change `MediaBag` type to a map from Text to MediaItem [API change].
    + `lookupMedia` now returns a `MediaItem` [API change].
    + Change `insertMedia` so it sets the `mediaPath` to a filename based on
      the SHA1 hash of the contents.  This will be used when contents
      are extracted.

  * Text.Pandoc.Class.PandocMonad:

    + Remove `fetchMediaResource` [API change].  Use `fetchItem` to get
      resources in `fillMediaBag`.
    + Add informational message in `downloadOrRead` indicating what path
      local resources have been loaded from.

  * Text.Pandoc.Logging:

    + Remove single quotes around paths in messages.
    + Add LoadedResource constructor to LogMessage [API change].
      This is for INFO-level messages telling where image data has been
      loaded from.  (This can vary because of the resource path.)

  * Text.Pandoc.Asciify: simplify code and export `toAsciiText` [API change].
    Instead of encoding a giant (and incomplete) map, we now
    just use unicode-transforms to normalize the text to
    a canonical decomposition, and manipulate the result.

  * App: allow tabs expansion even if file-scope is used (Albert Krewinkel,
    #6709).  Tabs in plain-text inputs  are now handled correctly, even if
    the `--file-scope` flag is used.

  * Add new internal module Text.Pandoc.Writers.GridTable (Albert Krewinkel).

  * Text.Pandoc.Highlighting: Change type of `languagesByExtension`, adding
    a parameter for a `SyntaxMap` [API change] (Jan Tojnar, #7241).
    Languages defined using `--syntax-definition` were not recognized by
    `languagesByExtension`.  This patch corrects that, allowing the writers
    to see all custom definitions.  The LaTeX writer still uses the default
    syntax map, but that's okay in that context, since
    `--syntax-definition` won't create new listings styles.

  * Text.Pandoc.Citeproc:

    + Ensure that CSL-related attributes are passed on to a Div with id
      'refs'.  Otherwise things like `entry-spacing` won't work when
      such Divs are used.
    + Use metadata's `lang` for the lang parameter of citeproc, overriding
      `localeLanguage`.
    + Recognize locators spelled with a capital letter (#7323).
    + Add a comma and a space in front of the suffix if it doesn't start
      with space or punctuation (#7324).
    + Don't detect math elements as locators (#7321).

  * Remove Text.Pandoc.BCP47 module [API change].  Use types and functions
    from UnicodeCollation.Lang instead.  This is a richer implementation
    of BCP 47.

  * Text.Pandoc.Shared:

    + Fix regression in grid tables for wide characters (#7214).
      In the translation from String to Text, a char-width-sensitive
      `splitAt'` was dropped.  This commit reinstates it and uses it to make
      `splitTextByInstances` char-width sensitive.
    + Add `getLang` (formerly in the now-removed BCP47) [API change].

  * Text.Pandoc.SelfContained: use `application/octet-stream`
    for unknown mime types instead of halting with an error (#7202).

  * Lua filters: respect Inlines/Blocks filter functions in `pandoc.walk_*`
    (Albert Krewinkel).

  * Add text as build-depend for trypandoc (#7193, Roman Beránek).

  * Bump upper-bounds for network-uri, time, attoparsec.

  * Use citeproc 0.4.

  * Use texmath 0.12.3.

  * Use jira-wiki-markup 1.3.5 (Albert Krewinkel).

  * Require latest skylighting (fixes a bug in XML syntax highlighting).

  * Use latest xml-conduit.

  * Use latest commonmark, commonmark-extensions, commonmark-pandoc.

  * Use haddock-library-1.10.0 (Albert Krewinkel).

  * Allow compilation with base 4.15 (Albert Krewinkel).

  * MANUAL:

    + Add information about `lang` and bibliography sorting.
    + Add info about YAML escape sequences, link to spec (#7152,
      Albert Krewinkel).
    + Note that `institute` variable works for HTML-based slides.
    + Update documentation on citation syntax.
    + Add citation example for locators and suffixes (Tristan Stenner)

  * Updated and fixed typos in documentation (Charanjit Singh,
    Anti-Distinctlyminty, Tatiana Porras, obcat).

  * Add instructions for installing pandoc-types before compiling filter.

  * INSTALL: add note that parallel installations should be avoided
    (#6865).

  * Remove `biblatex-nussbaum.md` test.  It is basically the same
    as `biblaetx-quotes.md`.

  * Command tests: fail if a file contains no tests---and fix a
    test that failed in that way!

  * Use smaller images in tests, reducing the size of the source tarball by 8 MB.


## pandoc 2.13 (2021-03-21)

  * Support `yaml_metadata_block` extension for `commonmark`, `gfm` (#6537).
    This supported is a bit more limited than with pandoc's
    `markdown`.  The YAML block must be the first thing in the input,
    and the leaf notes are parsed in isolation from the rest of
    the document.  So, for example, you can't use reference
    links if the references are defined later in the document.

  * Fix fallback to default partials when custom templates are used.
    If the directory containing a template does not contain the partial,
    it should be sought in the default templates, but this was not
    working properly (#7164).

  * Handle `nocite` better with `--biblatex` and `--natbib` (#4585).
    Previously the nocite metadata field was ignored with these formats.
    Now it populates a `nocite-ids` template variable and causes a
    `\nocite` command to be issued.

  * Text.Pandoc.Citeproc: apply `fixLinks` correctly (#7130).  This is code
    that incorporates a prefix like `https://doi.org/` into a following link
    when appropriate.

  * Text.Pandoc.Shared:

    + Remove `backslashEscapes`, `escapeStringUsing` [API change].  Replace
      these inefficient association list lookups with more efficient escaping
      functions in the writers that used them (for a 10-25% performance boost
      in org, haddock, rtf, texinfo writers).
    + Remove `ToString`, `ToText` typeclasses [API change].  These were needed
      for the transition from String to Text, but they are no longer used and
      may clash with other things.
    + Simplify `compactDL`.

  * Text.Pandoc.Parsing:

    + Change type of `readWithM` so that it is no longer polymorphic
      [API change].  The `ToText` class has been removed, and now that we've
      completed the transition to Text we no longer need this to operate
      on Strings.
    + Remove `F` type synonym [API change].  Muse and Org were defining their
      own `F` anyway.

  * Text.Pandoc.Readers.Metadata:

    + Export `yamlMetaBlock` [API change].
    + Make `yamlBsToMeta`, `yamlBsToRefs` polymorphic on the parser state
      [API change].

  * Markdown reader: Fix regression with `tex_math_backslash` (#7155).

  * MediaWiki reader: Allow block-level content in notes (ref) (#7145).

  * Jira reader (Albert Krewinkel):

    + Fixed parsing of autolinks (i.e., of bare URLs in the text).
      Previously an autolink would take up the rest of a line, as spaces
      were allowed characters in these items.
    + Emoji character sequences no longer cause parsing failures. This was
      due to missing backtracking when emoji parsing fails.
    + Mark divs created from panels with class "panel".

  * RST reader: fix logic for ending comments (#7134).  Previously comments
    sometimes got extended too far.

  * DocBook writer:  include Header attributes as XML attributes on
    section (Erik Rask).  Attributes with key names that are not allowed
    as XML attributes are dropped, as are attributes with invalid values
    and `xml:id` (DocBook 5) and `id` (DocBook 4).

  * Docx writer:

    + Make `nsid` in `abstractNum` deterministic.  Previously we assigned
      a random number, but we don't need random values, so now we just
      assign a value based on the list marker.
    + Use integral values for `w:tblW` (#7141).

  * Jira writer (Albert Krewinkel):

    + Block quotes are only rendered as `bq.` if they do not contain a
      linebreak.
    + Jira writer: improve div/panel handling.  Include div attributes in
      panels, always render divs with class `panel` as panels, and
      avoid nesting of panels.

  * HTML writer: Add warnings on duplicate attribute values.
    This prevents emitting invalid HTML.  Ultimately it would be good to
    prevent this in the types themselves, but this is better for now.

  * Org writer: Prevent unintended creation of ordered list items (#7132,
    Albert Krewinkel).  Adjust line wrapping if default wrapping would cause
    a line to be read as an ordered list item.

  * JATS templates: support 'equal-contrib' attrib for authors (Albert
    Krewinkel).  Authors who contributed equally to a paper may be marked
    with `equal-contrib`.

  * reveal.js template: replace JS comment with HTML (#7154, Florian Kohrt).

  * Text.Pandoc.Logging: Add `DuplicateAttribute` constructor to `LogMessage`.
    [API change]

  * Use `-j4` for linux release build.  This speeds up the build dramatically
    on arm.

  * cabal.project: remove ghcoptions.  Move flags to top level, so they can
    be set differently on the command line.

  * Require latest texmath, skylighting, citeproc, jira-wiki-markup.
    (The latest skylighting fixes a bad bug with Haskell syntax highlighting.)
    Narrow version bounds for texmath, skylighting, and citeproc, since
    the test output depend on them.

  * Use doclayout 0.3.0.2.  This significantly reduces the time and memory
    needed to compile pandoc.

  * Use `foldl'` instead of `foldl` everywhere.

  * Update bounds for random (#7156, Alexey Kuleshevich).

  * Remove uses of some partial functions.

  * Don't bake in a larger stack size for the executable.

  * Test improvements:

    + Use `getExecutablePath` from base, avoiding the dependency on
      `executable-path`.
    + Factor out `setupEnvironment` in Helpers, to avoid code duplication.
    + Fix finding of data files by setting teh `pandoc_datadir` environment
      variable when we shell out to pandoc. This avoids the need to use
      `--data-dir` for the tests, which caused problems finding `pandoc.lua`
      when compiling without the `embed_data_files` flag (#7163).

  * Benchmark improvements:

    + Build `+RTS -A8m -RTS` into default ghc-options for benchmark.
      This is necessary to get accurate benchmark results; otherwise we
      are largely measuring garbage collecting, some not related to the
      current benchmark.
    + Allow specifying BASELINE file in 'make bench' for comparison
      (otherwise the latest benchmark is chosen by default).
    + Force `readFile` in benchmarks early (Bodigrim).

  * CONTRIBUTING: suggest using a `cabal.project.local` file (#7153,
    Albert Krewinkel).

  * Add ghcid-test to Makefile.  This loads the test suite in ghcid.


## pandoc 2.12 (2021-03-08)

  * `--resource-path` now accumulates if specified multiple
    times (#6152).  Resource paths specified later on the command line are
    prepended to those specified earlier.  Thus,
    `--resource-path foo --resource-path bar:baz` is equivalent to
    `--resource-path bar:bas:foo`.  (The previous behavior was
    for the last `--resource-path` to replace all the rest.)
    `resource-path` in defaults files behaves the same way: it
    will be prepended to the resource path set by earlier
    command line options or defaults files.  This change
    facilitates the use of multiple defaults files: each can
    specify a directory containing resources it refers to
    without clobbering the resource paths set by the others.

  * Allow defaults files to refer to the home directory, the
    user data directory, and the directory containing the defaults file
    itself (#5871, #5982, #5977).  In fields that expect file paths
    (and only in these fields),

    + `${VARIABLE}` will expand to the value of the environment variable
      `VARIABLE` (and in particular `${HOME}` will expand to the path
      of the home directory).  A warning will be raised for undefined
      variables.
    + `${USERDATA}` will expand to the path of the user data
      directory in force when the defaults file is being processed.
    + `${.}` will expand to the directory containing the defaults file.
      (This allows default files to be placed in a directory containing
      resources they make use of.)

  * When downloading content from URL arguments, be sensitive to
    the character encoding (#5600).  We can properly handle UTF-8 and latin1
    (ISO-8859-1); for others we raise an error.  Fall back to latin1 if
    no charset is given in the mime type and UTF-8 decoding fails.

  * Allow abbreviations that don't end in a period to be
    specified using `--abbreviations` (#7124).

  * Add new unexported module Text.Pandoc.XML.Light, as well
    as Text.Pandoc.XML.Light.Types, Text.Pantoc.XML.Light.Proc,
    Text.Pandoc.XML.Light.Output.  (Closes #6001, #6565, #7091).

    This module exports definitions of `Element` and `Content`
    that are isomorphic to xml-light's, but with Text
    instead of String.  This allows us to keep most of the code in existing
    readers that use xml-light, but avoid lots of unnecessary allocation.

    We also add versions of the functions from xml-light's
    Text.XML.Light.Output and Text.XML.Light.Proc that operate on our
    modified XML types, and functions that convert xml-light types to our
    types (since some of our dependencies, like texmath, use xml-light).

    We export functions that use xml-conduit's parser to produce an
    `Element` or `[Content]`.  This allows existing pandoc code to use
    a better parser without much modification.

    The new parser is used in all places where xml-light's parser was
    previously used.  Benchmarks show a significant performance improvement
    in parsing XML-based formats (with docbook, opml, jats, and docx
    almost twice as fast, odt and fb2 more than twice as fast).

    In addition, the new parser gives us better error reporting than
    xml-light.  We report XML errors, when possible, using the new
    `PandocXMLError` constructor in `PandocError`.

    These changes revealed the need for some changes in the tests.  The
    docbook-reader.docbook test lacked definitions for the entities it used;
    these have been added. And the docx golden tests have been updated,
    because the new parser does not preserve the order of attributes.

  * DocBook reader:

    + Avoid expensive tree normalization step, as it is not necessary
      with the new XML parser.
    + Support `informalfigure` (#7079) (Nils Carlson).

  * Docx reader:

    + Use Map instead of list for Namespaces.  This gives a speedup of
      about 5-10%. With this and the XML parsing changes, the docx reader
      is now about twice as fast as in the previous release.

  * HTML reader:

    + Small performance tweaks.
    + Also, remove exported class `NamedTag(..)` [API change]. This was just
      intended to smooth over the transition from String to Text and is no
      longer needed.
    + As a result, the functions `isInlineTag` and `isBlockTag`
      are no longer polymorphic; they apply to a `Tag Text` [API change].
    + Do a lookahead to find the right parser to use.  This takes
      benchmarks from 34ms to 23ms, with less allocation.
    + Fix bad handling of empty `src` attribute in `iframe` (#7099).
      If `src` is empty, we simply skip the `iframe`.
      If `src` is invalid or cannot be fetched, we issue a warning
      nd skip instead of failing with an error.

  * JATS reader:

    + Avoid tree normalization, which is no longer necessary given the
      new XML parser.

  * LaTeX reader:

    + Don't export `tokenize`, `untokenize` [API change].  These are internal
      implementation details, which were only exported for testing.
      They don't belong in the public API.
    + Improved efficiency of the parser.  With these changes the reader
      is almost twice as fast as in the last release in our benchmarks.
    + Code cleanup, removing some unnecessary things.
    + Rewrite `withRaw` so it doesn't rely on fragile assumptions
      about token positions (which break when macros are expanded)
      (#7092).  This requires the addition of `sEnableWithRaw` and
      `sRawTokens` in `LaTeXState`, and a new combinator `disablingWithRaw`
      to disable collecting of raw tokens in certain contexts.
      Add `parseFromToks` to Text.Pandoc.Readers.LaTeX.Parsing.
      Fix parsing of single character tokens so it doesn't mess
      up the new raw token collecting.  These changes slightly increase
      allocations and have a small performance impact.
    + Handle some bibtex/biblatex-specific commands that used to be
      dealt with in pandoc-citeproc (#7049).
    + Optimize `satisfyTok`, avoiding unnecessary macro expansion steps.
      Benchmarks after this change show 2/3 of the run time and 2/3 of the
      allocation of the Feb. 10 benchmarks.
    + Removed `sExpanded` in state.  This isn't actually needed and checking
      it doesn't change anything.
    + Improve `braced'`.  Remove the parameter, have it parse the
      opening brace, and make it more efficient.
    + Factor out pieces of the LaTeX reader to make the module smaller.
      This reduces memory demands when compiling.  Created
      Text.Pandoc.Readers.{LaTeX,Math,Citation,Table,Macro,Inline}.
      Changed Text.Pandoc.Readers.LaTeX.SIunitx to export a command map
      instead of individual commands.
    + Handle table cells containing `&` in `\verb` (#7129).

  * Make Text.Pandoc.Readers.LaTeX.Types an unexported module [API change].

  * Markdown reader:

    + Improved handling of mmd link attributes in references (#7080).
      Previously they only worked for links that had titles.
    + Improved efficiency of the parser (benchmarks show a 15% speedup).

  * OPML reader:

    + Avoid tree normalization, which is no longer necessary with the
      new XML parser.

  * ODT reader:

    + Finer-grained errors on parse failure (#7091).
    + Give more information if the zip container can't be unpacked.

  * Org reader:

    + Support `task_lists` extension (Albert Krewinkel, #6336).
    + Fix bug in org-ref citation parsing (Albert Krewinkel, #7101).
      The org-ref syntax allows to list multiple citations separated by
      comma.  Previously commas were accepted as part of the citation id,
      so all citation lists were parsed as one single citation.

  * RST reader:

    + Use `getTimestamp` instead of `getCurrentTime` to fetch timestamp.
      Setting `SOURCE_DATE_EPOCH` will allow reproducible builds.
    + RST reader: fix handling of header in CSV tables (#7064).
      The interpretation of this line is not affected by the delim option.

  * Jira reader:

    + Modified the Doc parser to skip leading blank lines. This fixes
      parsing of documents which start with multiple blank lines (Albert
      Krewinkel, #7095).
    + Prevent URLs within link aliases to be treated as autolinks (Albert
      Krewinkel, #6944).

  * Text.Pandoc.Shared

    + Remove formerly exported functions that are no longer used in the
      code base: `splitByIndices`, `splitStringByIndicies`, `substitute`,
      and `underlineSpan` (which had been deprecated in April 2020)
      [API change].
    + Export `handleTaskListItem` (Albert Krewinkel) [API change].
    + Change `defaultUserDataDirs` to `defaultUserDataDir` [API
      change].  We determine what is the default user data directory
      by seeing whether the XDG directory and/or legacy
      directory exist.

  * BibTeX writer:

    + BibTeX writer: use doclayout and doctemplate.  This change allows
      bibtex/biblatex output to wrap as other formats do,
      depending on the settings of `--wrap` and `--columns` (#7068).

  * CSL JSON writer:

    + Output `[]` if no references in input, instead of raising a
      PandocAppError as before.

  * Docx writer:

    + Use `getTimestamp` instead of `getCurrentTime` for timestamp.
      Setting `SOURCE_DATE_EPOCH` will allow reproducible builds.

  * EPUB writer:

    + Use `getTimestamp` instead of `getCurrentTime` for timestamp.
      Setting `SOURCE_DATE_EPOCH` will allow reproducible builds (#7093).
      This does not suffice to fully enable reproducible in EPUB, since
      a unique id is still being generated for each build.
    + Support `belongs-to-collection` metadata (#7063) (Nick Berendsen).

  * JATS writer:

    + Escape special chars in reference elements (Albert Krewinkel).
      Prevents the generation of invalid markup if a citation element
      contains an ampersand or another character with a special meaning
      in XML.

  * Jira writer:

    + Use Span identifiers as anchors (Albert Krewinkel).
    + Use `{noformat}` instead of `{code}` for unknown languages (Albert
      Krewinkel). Code blocks which are not marked as a language supported
      by Jira are rendered as preformatted text via `{noformat}` blocks.

  * LaTeX writer:

    + Adjust hypertargets to beginnings of paragraphs (#7078).
      Use `\vadjust pre` so that the hypertarget takes you to the beginning
      of the paragraph rather than one line down.
      This makes a particular difference for links to citations using
      `--citeproc` and `link-citations: true`.
    + Change BCP47 lang tag from `jp` to `ja` (Mauro Bieg, #7047).
    + Use function instead of map for accent lookup (should be
      more efficient).
    + Split the module to make it easier to compile on low-memory
      systems:  added Text.Pandoc.Writers.LaTeX.{Util,Citation,Lang}.

  * Markdown writer:

    + Handle math right before digit.  We insert an HTML comment to
      avoid a `$` right before a digit, which pandoc will not recognize
      as a math delimiter.
    + Split the module to make it easier to compile on low-memory
      systems: added Text.Pandoc.Writers.Markdown.{Types,Inline}.

  * ODT writer:

    + Use `getTimestamp` instead of `getCurrentTime` for timestamp.
      Setting `SOURCE_DATE_EPOCH` will allow reproducible builds.
    + Update default ODT style (Lorenzo).  Previously, the "First paragraph"
      style inherited from "Standard" but not from "Text body." Now
      it is adjusted to inherit from "Text body", to avoid some ugly
      spacing issues. It may be necessary to update a custom `reference.odt`
      in light of this change.

  * Org writer:

    + Support `task_lists` extension (Albert Krewinkel, #6336).

  * Pptx writer:

    + Use `getTimestamp` instead of `getCurrentTime` for timestamp.
      Setting `SOURCE_DATE_EPOCH` will allow reproducible builds.

  * JATS templates: tag `author.name` as `string-name` (Albert Krewinkel).
    The partitioning the components of a name into surname, given names,
    etc. is not always possible or not available. Using `author.name`
    allows to give the full name as a fallback to be used when
    `author.surname` is not available.

  * Add default templates for bibtex and biblatex, so that
    the variables `header-include`, `include-before`, `include-after`
    (or alternatively the command line options
    `--include-in-header`, `--include-before-body`, `--include-after-body`)
    may be used.

  * LaTeX template:

    + Update to iftex package (#7073) (Andrew Dunning)
    + Wrap url colours in braces (#7121) (Loïc Grobol).

  * revealjs template: Add 'center' option for vertical slide centering.
    (maurerle, #7104).

  * Text.Pandoc.XML: Improve efficiency of `fromEntities`.

  * Text.Pandoc.MIME

    + Add exported function `getCharset` [API change].

  * Text.Pandoc.UTF8: change IO functions to return Text, not String
    [API change].  This affects `readFile`, `getContents`, `writeFileWith`,
    `writeFile`, `putStrWith`, `putStr`, `putStrLnWith`, `putStrLn`.
    `hPutStrWith`, `hPutStr`, `hPutStrLnWith`, `hPutStrLn`, `hGetContents`.
    This avoids the need to uselessly create a linked list of characters
    when emiting output.

  * Text.Pandoc.App

    + Add `parseOptionsFromArgs` [API change, new exported function].
    + Add fields for CSL options to `Opt` [API change]:
      `optCSL`, `optbibliography`, `optCitationAbbreviations`.

  * Text.Pandoc.Citeproc.BibTeX

    + `Text.Pandoc.Citeproc.writeBibTeXString` now returns
      `Doc Text` instead of `Text` (#7068).
    + Correctly handle `pages` (= `page` in CSL) (#7067).
    + Correctly handle BibLaTeX `langid` (= `language` in CSL, #7067).
    + In BibTeX output, protect foreign titles since there's no language
      field (#7067).
    + Clean up BibTeX parsing (#7049).  Previously there was a messy code
      path that gave strange results in some cases, not passing through raw
      tex but trying to extract a string content.  This was an artefact of
      trying to handle some special bibtex-specific commands in the BibTeX
      reader. Now we just handle these in the LaTeX reader and simplify
      parsing in the BibTeX reader. This does mean that more raw tex will
      be passed through (and currently this is not sensitive to the
      `raw_tex` extension; this should be fixed).

  * Text.Pandoc.Citeproc.MetaValue

    + Correctly parse "raw" date value in markdown references metadata.
      (See jgm/citeproc#53.)

  * Text.Pandoc.Citeproc

    + Use https URLs for links (Salim B, #7122).

  * Text.Pandoc.Class

    + Add `getTimestamp` [API change].  This attempts to read the
      `SOURCE_DATE_EPOCH` environment variable and parse a UTC time
      from it (treating it as a unix date stamp, see
      https://reproducible-builds.org/specs/source-date-epoch/). If the
      variable is not set or can't be parsed as a unix date stamp, then the
      function returns the current date.

  * Text.Pandoc.Error

    + Add `PandocUnsupportedCharsetError` constructor for
      `PandocError` [API change].
    + Export `renderError` [API change].
    + Refactor `handleError` to use `renderError`. This allows us render
      error messages without exiting.

  * Text.Pandoc.Extensions

    + `Ext_task_lists` is now supported by org (and turned
      on by default) (Albert Krewinkel, #6336).
    + Remove `Ext_fenced_code_attributes` from allowed commonmark attributes
      (#7097).  This attribute was listed as allowed, but it didn't actually
      do anything. Use `attributes` for code attributes and more.

  * Lua subsystem:

    + Always load built-in Lua scripts from default data-dir (Albert
      Krewinkel).  The Lua modules `pandoc` and `pandoc.List` are now always
      loaded from the system's default data directory. Loading from a
      different directory by overriding the default path, e.g. via
      `--data-dir`, is no longer supported to avoid unexpected behavior
      and to address security concerns.
    + Add module "pandoc.path" (Albert Krewinkel, #6001, #6565).
      The module allows to work with file paths in a convenient and
      platform-independent manner.
    + Use strict evaluation when retrieving AST value from the stack
      (Albert Krewinkel, #6674).

  * Text.Pandoc.PDF

    + Disable `smart` extension when building PDF via LaTeX.
      This is to prevent accidental creation of ligatures like
      `` ?` `` and `` !` `` (especially in languages with quotations like
      German), and similar ligature issues.  (See jgm/citeproc#54.)

  * Text.Pandoc.CSV:

    + Fix parsing of unquoted values (#7112).  Previously we didn't allow
      unescaped quotes in unquoted values, but they are allowed
      in CSV.

  * Test suite:

    + Use a more robust method for testing the executable.  Many
      of our tests require running the pandoc executable. This
      is problematic for a few different reasons. First,
      cabal-install will sometimes run the test suite after
      building the library but before building the executable,
      which means the executable isn't in place for the tests.
      One can work around that by first building, then building
      and running the tests, but that's fragile.  Second, we
      have to find the executable. So far, we've done that using
      a function `findPandoc` that attempts to locate it
      relative to the test executable (which can be located
      using findExecutablePath).  But the logic here is delicate
      and work with every combination of options.  To solve both
      problems, we add an `--emulate` option to the
      `test-pandoc` executable.  When `--emulate` occurs as the
      first argument passed to `test-pandoc`, the program simply
      emulates the regular pandoc executable, using the rest of
      the arguments (after `--emulate`). Thus, `test-pandoc
      --emulate -f markdown -t latex` is just like `pandoc -f
      markdown -t latex`.  Since all the work is done by library
      functions, implementing this emulation just takes a couple
      lines of code and should be entirely reliable.  With this
      change, we can test the pandoc executable by running the
      test program itself (locatable using `findExecutablePath`)
      with the `--emulate` option. This removes the need for the
      fragile `findPandoc` step, and it means we can run our
      integration tests even when we're just building the
      library, not the executable.  [Note: part of this change
      involved simplifying some complex handling to set
      environment variables for dynamic library paths.  I have
      tested a build with `--enable-dynamic-executable`, and it
      works, but further testing may be needed.]
    + Print accurate location if a test fails (Albert
      Krewinkel).  Ensures that tasty-hunit reports the location
      of the failing test instead of the location of the helper
      `test` function.

  * Documentation: Update URLs and use `https` where possible (#7122,
    Salim B).

  * Add `doc/libraries.md`, a description of libraries that support pandoc.

  * MANUAL.txt

    + MANUAL: block-level formatting is not allowed in line blocks (#7107).
    + Clarify `tex_math_dollars` extension.  Note that no blank lines
      are allowed between the delimiters in display math.
    + Add MANUAL section on reproducible builds.
    + Document no template fallback for absolute path (#7077, Nixon
      Enraght-Moony.)
    + Improve docs for cite-method.
    + Update README and man page.

  * Makefile: in `make bench`, create CSV files for comparison and compare
    against previous benchmark run.  Add timestamp to CSV filenames.

  * cabal.project: don't explicitly set -trypandoc.
    If we do, this can't be overridden on the cabal command line.

  * doc/lua-filters.md: improve documentation for
    `pandoc.mediabag.insert`, `pandoc.mediabag.fetch`,
    `directory`, `normalize` (Albert Krewinkel).

  * Allow base64-bytestring-1.2.* (Dmitrii Kovanikov)

  * Require jira-wiki-markup 1.3.3 (Albert Krewinkel)

  * Require citeproc 0.3.0.8, which correctly titlecases when titles
    contain non-ASCII characters.

  * Use skylighting 0.10.4.  This version of skylighting uses xml-conduit
    rather than hxt. This speeds up parsing of XML syntax definitions
    fourfold, and removes four packages from pandoc's dependency graph:
    hxt-charproperties, hxt-unicode, hxt-regex-xmlschema, hxt.

  * Add script `tools/parseTimings.pl` to help pin down which
    modules take the most time and memory to compile.

  * Avoid unnecessary use of NoImplicitPrelude pragma (#7089) (Albert
    Krewinkel)

  * Benchmarks

    + Use the lighter-weight tasty-bench instead of criterion.
    + Run writer benchmarks for binary formats too.
    + Alphabetize benchmarks.
    + Don't run benchmarks for bibliography formats
      (yet; we need a special input for them).
    + Show allocation data
    + Clean up benchmark code.
    + Allow specifying patterns using `-p blah'.

  * trypandoc: add 2 second timeout.

  * Use `-split-sections` in creating linux release binary.
    This reduces executable size significantly (by about 30%).

  * Remove `weigh-pandoc`.  It's not really useful any more, now that our
    regular benchmarks include data on allocation.

  * Improve linux package build process and add script to
    automate building an arm64 binary package.


## pandoc 2.11.4 (2021-01-22)

  * Add `biblatex`, `bibtex` as output formats (closes #7040).

  * Recognize more extensions as markdown by default (#7034):
    `mkdn`, `mkd`, `mdwn`, `mdown`, `Rmd`.

  * Implement defaults file inheritance (#6924, David Martschenko).
    Allow defaults files to inherit options from other defaults files by
    specifying them with the following syntax:
    `defaults: [list of defaults files or single defaults file]`.

  * Fix infinite HTTP requests when writing epubs from URL source (#7013).
    Due to a bug in code added to avoid overwriting the cover image
    if it had the form `fileX.YYY`, pandoc made an endless sequence
    of HTTP requests when writing epub with input from a URL.

  * Org reader:

    + Allow multiple pipe chars in todo sequences (Albert Krewinkel, #7014).
      Additional pipe chars, used to separate "action" state from "no further
      action" states, are ignored. E.g., for the following sequence, both
      `DONE` and `FINISHED` are states with no further action required:
      `#+TODO: UNFINISHED | DONE | FINISHED`.
    + Restructure output of captioned code blocks (Albert Krewinkel, #6977).
      The Div wrapper of code blocks with captions now has the class
      "captioned-content". The caption itself is added as a Plain block
      inside a Div of class "caption". This makes it easier to write filters
      which match on captioned code blocks. Existing filters will need to be
      updated.
    + Mark verbatim code with class `verbatim` (Dimitri Sabadie, #6998).

  * LaTeX reader:

    + Handle `filecontents` environment (#7003).
    + Put contents of unknown environments in a Div when `raw_tex` is not
      enabled (#6997). (When `raw_tex` is enabled, the whole environment is
      parsed as a raw block.) The class name is the name of the environment.
      Previously, we just included the contents without the surrounding Div,
      but having a record of the environment's boundaries and name can be
      useful.

  * Mediawiki reader:

    + Allow space around storng/emph delimiters (#6993).

  * New module Text.Pandoc.Writers.BibTeX, exporting
    writeBibTeX and writeBibLaTeX. [API change]

  * LaTeX writer:

    + Revert table line height increase in 2.11.3 (#6996).
      In 2.11.3 we started adding `\addlinespace`, which produced less dense
      tables.  This wasn't an intentional change; I misunderstood a comment in
      the discussion leading up to the change. This commit restores the earlier
      default table appearance.  Note that if you want a less dense table, you
      can use something like `\def\arraystretch{1.5}` in your header.

  * EPUB writer:

    + Adjust internal links to identifiers defined in raw HTML sections
      after splitting into chapters (#7000).
    + Recognize `Format "html4"`, `Format "html5"` as raw HTML.
    + Adjust internal links to images, links, and tables after splitting into
      chapters. Previously we only did this for Div and Span and Header
      elements (see #7000).

  * Ms writer:

    + Don't justify text inside table cells.

  * JATS writer:

    + Use `<element-citation>` if `element_citations`
      extension is enabled (Albert Krewinkel).
    + Fix citations (Albert Krewinkel, #7018).  By default
      we use formatted citations.
    + Ensure that `<disp-quote>` is always wrapped in `<p>` (#7041).

  * Markdown writer:

    + Cleaned up raw formats.  We now react appropriately
      to `gfm`, `commonmark`, and `commonmark_x` as raw formats.

  * RST writer:

    + Fix bug with dropped content from inside spans with a class in
      some cases (#7039).

  * Docx writer:

    + Handle table header using styles (#7008).  Instead of hard-coding
      the border and header cell vertical alignment, we now let this
      be determined by the Table style, making use of Word's
      "conditional formatting" for the table's first row.  For
      headerless tables, we use the tblLook element to tell Word
      not to apply conditional first-row formatting.

  * Commonmark writer:

    + Implement start number on ordered lists (#7009).  Previously they always
      started at 1, but according to the spec the start number is respected.

  * HTML writer:

    + Fix implicit_figure at end of footnotes (#7006).

  * ConTeXt template: Remove `\setupthinrules` from default template.
    The width parameter this used is not actually supported,
    and the command didn't do anything.

  * Text.Pandoc.Extensions:

    + Add `Ext_element_citations` constructor (Albert Krewinkel).

  * Text.Pandoc.Citeproc.BibTeX: New unexported function
    `writeBibtexString`.

  * Text.Pandoc.Citeproc:

    + Use finer grained imports (Albert Krewinkel).
    + Factor out and export `getStyle` [API change].
    + Export `getReferences` [API change, #7106].
    + Factor out getLang.

  * Text.Pandoc.Parsing: modify `gridTableWith'` for headerless tables.
    If the table lacks a header, the header row should be an empty
    list. Previously we got a list of empty cells, which caused
    an empty header to be emitted instead of no header.  In LaTeX/PDF
    output that meant we got a double top line with space between.

  * ImageSize:  use `viewBox` for SVG if no length, width attributes (#7045).
    This change allows pandoc to extract size information from more SVGs.

  * Add simple default.nix.

  * Use commonmark 0.1.1.3.

  * Use citeproc 0.3.0.5.

  * Update default CSL to use latest chicago-author-date.csl.

  * CONTRIBUTING.md: add note on GNU xargs.

  * MANUAL.txt:

    + Update description of `-L`/`--lua-filter`.
    + Document use of citations in note styles (#6828).

## pandoc 2.11.3.2 (2020-12-29)

  * HTML reader: use renderTags' from Text.Pandoc.Shared (Albert Krewinkel).
    A side effect of this change is that empty `<col>` elements are written
    as self-closing tags in raw HTML blocks.

  * Asciidoc writer: Add support for writing nested tables (#6972, timo-a).
    Asciidoc supports one level of nesting. If deeper tables are to be
    written, they are omitted and a warning is issued.

  * Docx writer: fix nested tables with captions (#6983).
    Previously we got unreadable content, because docx seems
    to want a `<w:p>` element (even an empty one) at the end of
    every table cell.

  * Powerpoint writer: allow arbitrary OOXML in raw inline elements
    (Albert Krewinkel).  The raw text is now included verbatim in the
    output. Previously is was parsed into XML elements, which prevented
    the inclusion of partial XML snippets.

  * LaTeX writer: support colspans and rowspans in tables (#6950,
    Albert Krewinkel).  Note that the multirow package is needed for
    rowspans.  It is included in the latex template under a variable,
    so that it won't be used unless needed for a table.

  * HTML writer: don't include p tags in CSL bibliography entries
    (#6966).  Fixes a regression in 2.11.3.

  * Add `meta-description` variable to HTML templates (#6982). This
    is populated by the writer by stringifying the `description`
    field of metadata (Jerry Sky).  The `description` meta tag will
    make the generated HTML documents more complete and SEO-friendly.

  * Citeproc: fix handling of empty URL variables (`DOI`, etc.).
    The `linkifyVariables` function was changing these to links
    which then got treated as non-empty by citeproc, leading
    to wrong results (e.g. ignoring nonempty URL when empty DOI is present).
    See jgm/citeproc#41.

  * Use citeproc 0.3.0.3.  Fixes an issue in author-only citations when
    both an author and translator are present, and an issue with
    citation group delimiters.

  * Require texmath 0.12.1.  This improves siunitx support in math,
    fixes bugs with `\*mod` family operators and arrays, and avoids
    italicizing symbols and operator names in docx output.

  * Ensure that the perl interpreter used for filters with `.pl`
    extension (wuffi).

  * MANUAL: note that textarea content is never parsed as Markdown
    (Albert Krewinkel).


## pandoc 2.11.3.1 (2020-12-18)

  * Added some missing files to extra-source-files and data
    files, so they are included in the sdist tarball.  Closes #6961.
    Cleaned up some extraneous data and test files, and added
    a CI check to ensure that the test and data files included
    in the sdist match what is in the git repository.

  * Use citeproc 0.3.0.1, which avoids removing nonbreaking
    space at the end of the `initialize-with` attribute. (Some
    journals require nonbreaking space after initials, and this
    makes that possible.)

## pandoc 2.11.3 (2020-12-17)

  * With `--bibliography` (or `bibliography` in metadata), a
    URL may now be provided, and pandoc will fetch the resource.
    In addition, if a file path is provided and it is not
    found relative to the working directory, the resource
    path will be searched (#6940).

  * Add `sourcepos` extension for `commonmark`, `gfm`, `commonmark_x`
    (#4565).  With the `sourcepos` extension set set, `data-pos`
    attributes are added to the AST by the commonmark reader. No other
    readers are affected.  The `data-pos` attributes are put on elements
    that accept attributes; for other elements, an enlosing Div or Span
    is added to hold the attributes.

  * Change extensions for `commonmark_x`: replace `auto_identifiers`
    with `gfm_auto_identifiers` (#6863).  `commonmark_x` never actually
    supported `auto_identifiers` (it didn't do anything), because the
    underlying library implements gfm-style identifiers only.  Attempts
    to add the `auto_identifiers` extension to `commonmark` will now
    fail with an error.

  * HTML reader:

    + Split module into several submodules (Albert Krewinkel).  Reducing
      module size should reduce memory use during compilation.
    + Support advanced table features (Albert Krewinkel):
      block level content in captions, row and colspans,
      body headers, row head columns, footers, attributes.
    + Disable round-trip testing for tables. Information for cell
      alignment in a column is not preserved during round-trips (Albert
      Krewinkel).
    + Allow finer grained options for tag omission (Albert Krewinkel).
    + Simplify list attribute handling (Albert Krewinkel).
    + Pay attention to `lang` attributes on body element (#6938).
      These (as well as `lang` attributes on the html element) should update
      lang in metadata.
    + Retain attribute prefixes and avoid duplicates (#6938).
      Previously we stripped attribute prefixes, reading `xml:lang` as
      `lang` for example. This resulted in two duplicate `lang`
      attributes when `xml:lang` and `lang` were both used.  This commit
      causes the prefixes to be retained, and also avoids invald
      duplicate attributes.

  * Commonmark reader:

    + Refactor `specFor`.
    + Set input name to `""` to avoid clutter in sourcepos output.

  * Org reader:

    + Parse `#+LANGUAGE` into `lang` metadata field (#6845, Albert
      Krewinkel).
    + Preserve targets of spurious links (#6916, Albert
      Krewinkel).  Links with (internal) targets that the reader doesn't
      know about are converted into emphasized text. Information on the
      link target is now preserved by wrapping the text in a Span of class
      `spurious-link`, with an attribute `target` set to the link's
      original target. This allows to recover and fix broken or unknown
      links with filters.

  * DocBook reader:

    + Table text width support (#6791, Nils Carlson).
      Table width in relation to text width is not natively supported
      by docbook but is by the docbook `fo` stylesheets through an XML
      processing instruction, `<?dbfo table-width="50%"?>`.

  * LaTeX reader:

    + Improve parsing of command options (#6869, #6873).
      In cases where we run into trouble parsing inlines til the
      closing `]`, e.g. quotes, we return a plain string with the
      option contents. Previously we mistakenly included the brackets
      in this string.
    + Preserve center environment (#6852, Igor Pashev).
      The contents of the `center` environment are put in a `Div`
      with class `center`.
    + Don't parse `\rule` with width 0 as horizontal rule. These are
      sometimes used as spacers in LaTeX.
    + Don't apply theorem default styling to a figure inside (#6925).
      If we put an image in italics, then when rendering to Markdown
      we no longer get an implicit figure.

  * Dokuwiki reader:

    + Handle unknown interwiki links better (#6932).
      DokuWiki lets the user define his own Interwiki links.  Previously
      pandoc reacted to these by emitting a google search link, which is
      not helpful. Instead, we now just emit the full URL including the
      wikilink prefix, e.g. `faquk>FAQ-mathml`.  This at least gives users
      the ability to modify the links using filters.

  * Markdown writer:

    + Properly handle boolean values in writing YAML metadata (#6388).
    + Ensure that a new csl-block begins on a new line (#6921).
      This just looks better and doesn't affect the semantics.

  * RST writer:

    + Better image handling (#6948).  An image alone in its paragraph
    (but not a figure) is now rendered as an independent image, with an
    `alt` attribute if a description is supplied.  An inline image that
    is not alone in its paragraph will be rendered, as before, using a
    substitution.  Such an image cannot have a "center", "left", or
    "right" alignment, so the classes `align-center`, `align-left`, or
    `align-right` are ignored.  However, `align-top`, `align-middle`,
    `align-bottom` will generate a corresponding `align` attribute.

  * Docx writer:

    + Keep raw openxml strings verbatim  (#6933, Albert Krewinkel).
    + Use Content instead of Element.  This allows us to inject
      raw OpenXML into the document without reparsing it into an
      Element, which is necessary if you want to inject an open
      tag or close tag.
    + Fix bullets/lists indentation, so that the first level is slightly
      indented to the right instead of right on the margin (cholonam).
    + Support bold and italic in "complex script" (#6911).
      Previously bold and italics didn't work properly in LTR
      text.  This commit causes the w:bCs and w:iCs attributes
      to be used, in addition to w:b and w:i, for bold and
      italics respectively.

  * ICML writer:

    + Fix image bounding box for custom widths/heighta (Mauro Bieg, #6936).

  * LaTeX writer:

    + Improve table spacing (#6842, #6860).
      Remove the `\strut` that was added at the end of minipage
      environments in cells.  Replace `\tabularnewline` with
      `\\ \addlinespace`.
    + Improve calculation of column spacing (#6883).
    + Extract table handling into separate module (Albert Krewinkel).
    + Fix bug with nested `csl-` display Spans (#6921).
    + Improve longtable output (#6883).  Don't create minipages for
      regular paragraphs.  Put width and alignment information in the
      longtable column descriptors.

  * OpenDocument writer:

    + Support for table width as a percentage of text width
      (#6792, Nils Carson).
    + Implement Div and Span ident support (#6755, Nils Carson).
      Spans and Divs containing an ident in the Attr will become bookmarks
      or sections with idents in OpenDocument format.
    + Add two extensions, `xrefs_name` and `xrefs_number` (#6774, Nils
      Carlson).  Links to headings, figures and tables inside the
      document are substituted with cross-references that will use the
      name or caption of the referenced item for `xrefs_name` or the
      number for `xrefs_number`.  For the `xrefs_number` to be useful
      heading numbers must be enabled in the generated document and
      table and figure captions must be enabled using for example the
      `native_numbering` extension.  In order for numbers and reference
      text to be updated the generated document must be refreshed.

  * JATS writer:

    + Support advanced table features (Albert Krewinkel).
    + Support author affiliations (#6687, Albert Krewinkel).

  * Docbook writer:

    + Use correct id attribute consistently (Jan Tojnar).
      DocBook5 should always use `xml:id` instead of `id`.
    + Handle admonition titles better (Jan Tojnar).
      Docbook reader produces a `Div` with `title` class for `<title>`
      element within an “admonition” element. Markdown writer then turns
      this into a fenced div with `title` class attribute. Since fenced
      divs are block elements, their content is recognized as a
      paragraph by the Markdown reader. This is an issue for Docbook
      writer because it would produce an invalid DocBook document from
      such AST – the `<title>` element can only contain “inline”
      elements.  Handle this special case separately by unwrapping
      the paragraph before creating the `<title>` element.
    + Add XML namespaces to top-level elements (#6923, Jan Tojnar).
      Previously, we only added `xmlns` attributes to chapter
      elements, even when running with `--top-level-division=section`.
      These namespaces are now added to part and section elements too,
      when they are the selected top-level divisions.
      We do not need to add namespaces to documents produced with
      `--standalone` flag, since those will already have xmlns attribute on
      the root element in the template.

  * HTML writer:

    + Fix handling of nested `csl-` display spans (#6921).
      Previously inner Spans used to represent CSL display attributes were
      not rendered as div tags as intended.

  * EPUB writer:

    + Include title page in landmarks (#6919).
      Note that the toc is also included if `--toc` is specified.
    + Add frontmatter type on body element for nav.xhtml (#6918).

  * EPUB templates: use preserveAspectRatio="xMidYMid" for cover image (#6895,
    Shin Sang-jae).  This change affects both the epub2 and the epub3
    templates.  It avoids distortion of the cover image by requiring that the
    aspect ratio be preserved.

  * LaTeX template:

    + Include `csquotes` package if `csquotes` variable set.
    + Put back `amssymb`.  We need it for checkboxes in todo lists,
      and maybe for other things.  In this location it seems compatible
      with the cases that prompted #6469 and PR #6762.
    + Disable language-specific shorthands in babel (#6817, #6887).
      Babel defines "shorthands" for some languages, and these can
      produce unexpected results. For example, in Spanish, `1.22`
      gets rendered as `122`, and `et~al.` as `etal`.
      One would think that babel's `shorthands=off` option (which
      we were using) would disable these, but it doesn't.  So we
      remove `shorthands=off` and add some code that redefines
      the shorthands macro.  Eventually this will be fixed in babel,
      I hope, and we can revert to something simpler.

  * JATS template: allow array of persistent institute ids in `pid`
    (Albert Krewinkel).

  * Text.Pandoc.Parsing:  minor code and efficiency improvements.

  * Text.Pandoc.Extension:

    + Add `Ext_sourcepos` constructor for `Extension` [API change].
    + Add `Ext_xrefs_name` and `Ext_xrefs_number` constructors for
      `Extension` (Nils Carson) [API change].

  * Text.Pandoc.Citeproc:

    + Fix truncation of `[Citation]` list in `Cite` inside footnotes (#6890).
      This affected author-in-text citations in footnotes.  It didn't cause
      problems for the printed output, but for filters that expected the
      citation id and other information.
    + Allow the use of both inline and external references (#6951),
      as with pandoc-citeproc.  References defined in the document's
      metadata take priority over references with the same id defined in
      an external bibliography.
    + Use `fetchItem` to get external bibliography (#6940).
    + Ensure that BCP47 lang codes can be used.  We ignore the variants
      and just use the base lang code and country code when passing off
      to citeproc.
    + Citeproc BibTeX parser: revert change in `getRawField`
      which was made (for reasons forgotten) when transferring
      this code from pandoc-citeproc.  The change led to `--` in
      URLs being interpreted as en-dashes, which is unwanted (#6874).

  * Text.Pandoc.ImageSize:

    + Default to DPI 72 if the format specifies DPI of 0 (#6880).
      This shouldn't happen, in general, but it can happen with
      JPEGs that don't conform to the spec.  Having a DPI of 0
      will blow up size calculations (division by 0).
    + ImageSize: use JuicyPixels to determine size for png, jpeg, and
      gif, instead of doing our own binary parsing (#6936). This
      gives more reliable results.

  * Text.Pandoc.CSS:

    + Remove `foldOrElse` (internal module) (Albert Krewinkel).

  * Use skylighting 0.10.2 (#6625).

  * Use citeproc 0.3. This fixes issues with references with
    duplicate ids (jgm/citeproc#36).

  * Use doctemplates 0.9.  This fixes issues with boolean
    metadata values in the Markdown writer (#6388)
    and in `meta-json` (#6650).  It also fixes
    issues with nested for loops in templates.

  * Add translations zh-Hans.yaml and zh-Hant.yaml (#6904, #6909,
    Kolen Cheung, taotieren).

  * Add tests: True to cabal.project.
    This fixes some CI failures for cabal.

  * Normalize test/tables/*.native (#6888, Kolen Cheung).

  * Move executable to `app` directory to avoid problems with cabal repl.

  * CONTRIBUTING: add section "How can I help?" (#6892, Albert Krewinkel).
    Also adds a paragraph aimed at highlighting the importance of feature
    maintenance.

  * MANUAL: Document that --number-sections works in `ms` (#6935).


## pandoc 2.11.2 (2020-11-19)

  * Default to using ATX (`##`-style) headings for Markdown output
    (#6662, Aner Lucero).  Previously we used Setext (underlined) headings
    by default for levels 1--2.

  * Add option `--markdown-headings=atx|setext`, and deprecate
    `--atx-headers` (#6662, Aner Lucero).

  * Support `markdown-headings` in defaults files.

  * Fix corner case in YAML metadata parsing (#6823).  Previously YAML
    metadata would sometimes not get recognized if a field ended with a
    newline followed by spaces.

  * `--self-contained`: increase coverage (#6854).
    Previously we only self-contained attributes for certain tag names
    (`img`, `embed`, `video`, `input`, `audio`, `source`, `track`,
    `section`).  Now we self-contain any occurrence of `src`,
    `data-src`, `poster`, or `data-background-image`, on any tag; and
    also `href` on `link` tags.

  * Markdown reader:

    + Fix detection of locators following in-text citations.
      Prevously, if we had `@foo [p. 33; @bar]`, the `p. 33` would be
      incorrectly parsed as a prefix of `@bar` rather than a suffix of
      `@foo`.
    + Improve period suppression algorithm for citations in notes
      in note citation styles (#6835).
    + Don't increment `stateNoteNumber` for example list references.
      This helps with #6836 (a bug in which example list references
      disturb calculation of citation note number and affect when
      `ibid` is triggered).

  * LaTeX reader:

    + Move `getNextNumber` from Readers.LaTeX to Readers.LaTeX.Parsing.
    + Fix negative numbers in siunitx commands.  A change in pandoc 2.11
      broke negative numbers, e.g.  `\SI{-33}{\celcius}` or `\num{-3}`.
      This fixes the regression.

  * DocBook reader: drop period in formalpara title
    and put it in a div with class `formalpara-title`, so that
    people can reformat with filters (#6562).

  * Man reader: improve handling of `.IP` (#6858).  We now better handle
    `.IP` when it is used with non-bullet, non-numbered lists, creating a
    definition list.  We also skip blank lines like groff itself.

  * Bibtex reader: fall back on `en-US` if locale for LANG not found.
    This reproduces earlier pandoc-citeproc behavior (jgm/citeproc#26).

  * JATS writer:

    + Wrap all tables (Albert Krewinkel).
      All `<table>` elements are put inside `<table-wrap>` elements, as the
      former are not valid as immediate child elements of `<body>`.
    + Move Table handling to separate module (Albert Krewinkel).
      Adds two new unexported modules:
      Text.Pandoc.Writers.JATS.Types, Text.Pandoc.Writers.JATS.Table.

  * Org writer:

    + Replace org #+KEYWORDS with #+keywords (TEC).
      As of ~2 years ago, lower case keywords became the standard (though
      they are handled case insensitive, as always).
    + Update org supported languages and identifiers according to the
      current list contained in
      <https://orgmode.org/worg/org-contrib/babel/languages/index.html>
      (TEC).

  * Only use `filterIpynbOutput` if input format is ipynb (#6841).
    Before this change content could go missing from divs with class
    `output`, even when non-ipynb was being converted.

  * When checking reader/writer name, check base name now that we permit
    extensions on formats other than markdown.

  * Text.Pandoc.PDF: Fix `changePathSeparators` for Windows (#6173).
    Previously a path beginning with a drive, like `C:\foo\bar`, was
    translated to `C:\/foo/bar`, which caused problems.
    With this fix, the backslashes are removed.

  * Text.Pandoc.Logging:

    + Add constructor `ATXHeadingInLHS` to `LogMessage` [API change].
    + Add constructor `EnvironmentVariableUndefined` to
      `LogMessage` [API change].

  * Fix error that is given when people specify `doc` output (#6834,
    gison93).

  * LaTeX template:  add a `\break` after parbox in `CSLRightInline`.
    This should fix spacing problems between entries with numeric styles.
    Also fix number of params on `CSLReferences`.

  * reveal.js template: Put quotes around `controlsLayout`,
    `controlsBackArrows`, and `display`, since these require strings.
    Add `showSlideNumber`, `hashOneBasedIndex`, `pause`.

  * Use citeproc 0.2.  This fixes a bug with title case around parentheses.

  * pandoc.cabal: remove 'static' flag.
    This isn't really necessary and can be misleading (e.g. on macOS,
    where a fully static build isn't possible). cabal's new option
    `--enable-executable-static` does the same. On stack you can add
    something like this to the options for your executable in package.yaml:

        ld-options: -static -pthread

  * Remove obsolete bibutils flag setting in `linux/make_artifacts.sh`.

  * Manual:

    + Correct `link-citation` -> `link-citations`.
    + Add a sentence about `pagetitle` for HTML (#6843, Alex Toldaiev).

  * INSTALL.md: Remove references to `pandoc-citeproc` (#6857).

  * CONTRIBUTING: describe hlint and how it's used (#6840, Albert
    Krewinkel).

## pandoc 2.11.1.1 (2020-11-07)

  * Citeproc: improve punctuation in in-text note citations (#6813).
    Previously in-text note citations inside a footnote would sometimes have
    the final period stripped, even if it was needed (e.g. on the end of
    'ibid').

  * Use citeproc 0.1.1.1.  This improves the decision about when
    to use `ibid` in cases where citations are used inside
    a footnote (#6813).

  * Support `nocase` spans for `csljson` output.

  * Require latest commonmark, commonmark-extensions.
    This fixes a bug with `autolink_bare_uris` and commonmark.

  * LaTeX reader: better handling of `\\` inside math in table cells (#6811).

  * DokuWiki writer:  translate language names for code elements
    and improve whitespace (#6807).

  * MediaWiki writer: use `syntaxhighlight` tag instead of deprecated
    `source` for highlighted code (#6810).  Also support `startFrom`
    attribute and `numberLines`.

  * Lint code in PRs and when committing to master (#6790,
    Albert Krewinkel).

  * doc/filters.md: describe technical details of filter invocations (#6815,
    Albert Krewinkel).


## pandoc 2.11.1 (2020-11-03)

  * DocBook Reader: fix duplicate bibliography bug (#6773, Nils Carlson).

  * HTML reader:

    + Parse contents of iframes (#6770).
    + Parse inline svg as image unless `raw_html` is set in the reader (in
      which case the svg is passed through as raw HTML) (#6770).

  * LaTeX reader:

    + Fix bug parsing macro arguments (#6796).  If `\cL` is
      defined as `\mathcal{L}`, and `\til` as `\tilde{#1}`, then
      `\til\cL` should expand to `\tilde{\mathcal{L}}`, but
      pandoc was expanding it to `\tilde\mathcal{L}`.  This is
      fixed by parsing the arguments in "verbatim mode" when the
      macro expands arguments at the point of use.
    + Properly support optional (cite) argument for
      `\blockquote` from `csquotes` (#6802).

  * LaTeX writer: Improved calculation of table column widths.
    We now have LaTeX do the calculation, using `\tabcolsep`.
    So we should now have accurate relative column widths no
    matter what the text width.  The default template has been modified to load
    the calc package if tables are used.

  * HTML writer: Fix duplicate "class" attribute for table
    rows (Andy Morris).

  * Text.Pandoc.Filter: allow shorter YAML representation of Citeproc
    (Albert Krewinkel).  The map-based YAML representation of filters expects
    `type` and `path` fields. The path field had to be present for all filter
    types, but is not used for citeproc filters. The field can now be omitted
    when type is "citeproc", as described in the MANUAL.

  * Text.Pandoc.Error: Add `PandocBibliographyError` constructor
    for `PandocError` [API change].  This ensures that bibliography parsing
    errors generate messages that include the bibliography file name --
    otherwise it can be quite mysterious where it is coming from.

  * Citeproc: properly handle `csl` field with `data:` URI (#6783).
    This is used with the JATS writer, so this fixes a regression
    in pandoc 2.11 with JATS output and citeproc.

  * Allow `citation-abbreviations` in defaults file.

  * JATS templates: ensure `jats_publishing` output is valid
    (Albert Krewinkel).

  * LaTeX template:  Fix `CSLRightInline`, so that it does not
    run over the right margin.

  * HTML template: default CSS tweaks (Mauro Bieg and John
    MacFarlane).

    - Fix margin before codeblock
    - Add `monobackgroundcolor` variable, making the background color
      and padding of code optional.
    - Ensure that backgrounds from highlighting styles take precedence over
      monobackgroundcolor
    - Remove list markers from TOC
    - Add margin-bottom where needed
    - Remove italics from blockquote styling
    - Change borders and spacing in tables to be more consistent with other
       output formats
    - Style h5, h6
    - Set font-size for print media to 12pt.
    - Reduce interline space.
    - Reduce interparagraph space.
    - Reduce line width.
    - Remove the special `line-height: 1` for table cells.
    - Remove the special line-height for pre.
    - Ensure that there is a bit more space before a heading
      than after.
    - Slightly reduced space after title header.
    - Add CSS example to MANUAL

  * man template:  Change comment that triggers `tbl` from
    `.\"t` to `'\" t`, as specified in groff_man(7) (#6803).

  * Use latest commonmark, commonmark-extensions.
    This fixes a bug with nested blocks in footnotes with the
    `footnote` extension to `commonmark`.  See jgm/commonmark-hs#63.

  * Citeproc: use comma for in-text citations inside footnotes.
    When an author-in-text citation like `@foo` occurs in a footnote,
    we now render it with:  `AUTHOR NAME + COMMA + SPACE + REST`.
    Previously we rendered: `AUTHOR NAME + SPACE + "(" + REST + ")"`.
    This gives better results.  Note that normal citations are still
    rendered in parentheses.

  * Use latest citeproc:

    + citeproc no longer capitalizes notes, so we do it
      in pandoc when appropriate.
    + Closes #6783.

  * Clarify manual on `--track-changes` (#6801).

  * Add `doc/jats.md` to document pandoc's handling of JATS
    (#6794, Albert Krewinkel).

  * Fix code example in lua-filters.md (#6795).

## pandoc 2.11.0.4 (2020-10-21)

  * Commonmark writer: fix regression with fenced divs (#6768).
    Starting with 2.10.1, fenced divs no longer render with
    HTML div tags in commonmark output.  This is a regression
    due to our transition from cmark-gfm.  This commit fixes it.

  * Use released version of citeproc.  (This fixes a mis-step
    in the 2.11.0.3 release, which is now deprecated.)

  * Use latest sylighting, with support for `groovy`.

  * Document that --html-q-tags requires the smart extension on the reader
    (#6766).

## pandoc 2.11.0.3 (2020-10-20)

  * Use latest citeproc (closes #6765). This fixes a problem with
    author-in-text citations for references including both an author
    and an editor. Previously, both were included in the text, but only
    the author should be.

  * With `--citeproc`, ensure that the final period is removed when
    citations that occur in notes in note-based styles get put in
    parentheses.  See jgm/citeproc#20.

  * Normalize rewritten image paths with `--extract-media` (#6761).
    This change will avoid mixed paths like this one when
    `--extract-media` is used with a Word file:
    `![](C:\Git\TIJ4\Markdown/media/image30.wmf)`.  Instead we'll get
    `![](C:\Git\TIJ4\Markdown`media`image30.wmf)`.

  * Modify `--version` output.  Use space more efficiently and report the
    citeproc and ipynb versions, along with skylighting, texmath, and
    pandoc-types.  Drop the word "default" before "user data directory."

  * DocBook reader: bibliomisc and anchor support (#6754, Nils Carlson).
    Also ensure that bibliodiv without a title no longer results in
    an empty Header.

  * ConTeXt template: adds `\setupinterlinespace` to fonts larger
    than normal (#6763, Denis Maier).

  * LaTeX template: Do not load amssymb if not needed (#6469, Angelo Peronio).
    See <https://tex.stackexchange.com/a/549938>.

  * Relax upper bound on hslua, allow hslua-1.3.* (Albert Krewinkel).

  * MANUAL:

    + Improve explanation of "indent" variable (#6767, Cyrus Yip).
    + Remove org from list of input formats supporting raw_tex (#6753,
      Nick Fleisher).


## pandoc 2.11.0.2 (2020-10-15)

  * Fix handling of `xdata` in bibtex/biblatex bibliographies (#6752).

  * Fix some small typos in the API documentation (#6751, Michael Hoffmann).

  * Require citeproc 0.1.0.2.  This fixes a regression from pandoc-citeproc
    involving spacing between components of a reference in certain styles
    (e.g. `cell.csl`).

  * Fix typos in comments, doc strings, error messages, and tests
    (Albert Krewinkel, #6738).

## pandoc 2.11.0.1 (2020-10-13)

  * LaTeX reader: support more acronym commands (#6746):
    `\acl`, `\aclp`, and capitalized versions of already
    supported commands.

  * Commonmark reader: add `pipe_table` extension *after* defaults (#6739).
    Otherwise we get bad results for non-table, non-paragraph
    lines containing pipe characters.

  * Markdown writer: Fix autolinks rendering for gfm (#6740).
    Previously, autolinks rendered as raw HTML, due to the
    `class="uri"` added by pandoc's markdown reader.

  * LaTeX writer:

    + Escape option values in lstlistings environment (#6742).
    + Fix handling of `lang` `pt-BR` (#2953).  For polyglossia we now
      use `\setmainlanguage[variant=brazilian]{portuguese}` and for babel
      `\usepackage[shorthands=off,main=brazilian]{babel}`.

  * Depend on latest citeproc (0.1.0.1).

    + This fixes the citation number issue with ieee.csl and other
      styles that do not explicitly sort bibliographies (#6741). (Pandoc
      was numbering them by their order in the bibliography file,
      rather than the order cited, as required by the CSL spec.)
    + Fixes groupin/collapsing with citation items with prefixes.

  * default.latex: fix `CSLReference` macro definition.

  * Fix MANUAL.txt CSL JSON conversion examples.

  * Fix spelling errors in changelog, MANUAL.txt, `doc/org.md` (#6738).


## pandoc 2.11 (2020-10-11)

  * Add `--citeproc` (`-C`) option to trigger built-in citation processing.
    It is no longer necessary to use the external `pandoc-citeproc`
    filter.  `--citeproc` behaves like a filter and can be positioned
    relative to other filters as they appear on the command line.

    The new built-in citation processing uses the citeproc library,
    a reimplementation of CSL citation processing that fixes many
    of the shortcomings of pandoc-citeproc.  In general, citation
    processing should work much the same as it did with pandoc-citeproc,
    but with greater fidelity to CSL styles and better performance.
    (The tests from the pandoc-citeproc package have been carried
    over to pandoc.) The following differences should be noted:

    - At this point, only some of the writers (HTML, ms, LaTeX) properly
      interpret CSL display styles.  You should get decent output in
      all formats (at least as good as with pandoc-citeproc), but
      indentation and block-alignment may not be right.

    - pandoc-citeproc searches the `~/.csl` directory for `.csl`
      styles.  Pandoc instead searches the `csl` subdirectory of
      the pandoc user data directory (e.g., `~/.pandoc/csl` or
      `~/.local/share/pandoc/csl`).  Users who already keep
      CSL styles in `~/.csl` may find it convenient to add a
      symlink.

    - Some of the bibliography formats supported by pandoc-citeproc (via
      hs-bibutils) are no longer supported:  Copac, EndNote,
      ISI, MEDLINE, MODS, and RIS.  If you use one of these formats,
      you may use the `bibutils` utility to convert to BibLaTeX.
      We now support only BibTeX, BibLaTeX, CSL JSON,
      and pandoc's YAML/Markdown analogue of CSL JSON.

    - pandoc-citeproc would always retrieve the independent parent
      of a dependent style by doing an HTTP request.  pandoc will
      now first seek the independent parent locally (in the resource
      path or in the `csl` subdirectory of the pandoc user data
      directory) before resorting to HTTP.  In addition, you may
      omit the `.csl` extension, e.g. `--csl zoology`.

    - Using the `--bibliography` option (or including
      `bibliography` in YAML metadata) no longer triggers
      citation processing implicitly: one must always use the
      `--citeproc` option if citation processing is wanted.

  * Add `csljson` as and input and output format. This allows pandoc
    to convert between `csljson` and other bibliography formats
    (e.g. `-f csljson -t markdown -s` or `-f bibtex -t csljson`),
    and to generate formatted versions of CSL JSON bibliographies
    (e.g., `pandoc -f csljson --citeproc pl.json -o pl.pdf`).

  * Added `bibtex`, `biblatex` as input formats.  This allows pandoc
    to convert between BibLaTeX and BibTeX and other bibliography formats,
    and to generated formatted versions of BibTeX/BibLaTeX bibliographies
    (e.g., `pandoc -f biblatex --citeproc pl.bib -o pl.pdf`).

  * Raise informative errors when YAML metadata parsing fails (#6730).
    Previously the command would succeed, returning empty metadata,
    with no errors or warnings.

  * Sort languages in `--list-highlight-languages` output (#6718,
    Albert Krewinkel).  Languages were previously sorted by their
    long name, which leads to unexpected results).

  * Add CSS to default HTML template (#6601, Mauro Bieg).  This
    greatly improves the default typography in pandoc's HTML
    output.  The CSS is sensitive to a number of variables
    (e.g. `mainfont`, `fontsize`, `linestretch`): see the manual for
    details. To restore the earlier, more spartan output, you can
    disable this with `-M document-css=false`.

  * Support `--toc-depth` option for ODT writer (#6696, niszet).

  * Fix issues with Windows UNC paths with some options (#5127).

  * Remove `fenced_code_blocks` and `backtick_code_blocks` from allowed
    `commonmark` and `gfm` extensions.  These shouldn't really be counted
    as extensions, because they can't be disabled in commonmark.
    Adjust markdown writer to check for the commonmark variant in addition
    to extensions.

  * Add these extensions to `gfm` and `commonmark`:
    `fenced_code_blocks`, `backtick_code_blocks`, `fenced_code_attributes`.
    These can't really be disabled in the reader, but they need to be enabled
    in the writer or we just get indented code.

  * Make sure proper set of extensions is recognized for `commonmark_x`.

  * Allow `gfm_auto_identifiers`, `ascii_identifiers` extensions for `docx`.

  * Markdown reader:

    + Add `Maybe FilePath` parameter to `yamlToMeta` [API change].
    + Export `yamlToRefs` [API change], a version of `yamlToMeta`
      specialized to references.
    + Set `citationNoteNum` accurately in citations.
    + Revise abbreviation support.  Don't insert a nonbreaking space after a
      potential abbreviation if it comes right before a note or citation.
      This causes problems for citeproc's moving of note citations.

  * LaTeX reader:

    + Support missing siunitx commands (#6658).
    + Support `squared`, `cubed`, `tothe` in siunitx (#6657).
    + Improved uncertainty handling in slunitx.
    + Factored out siunitx stuff into separate unexposed module.
    + Fix improper empty cell filtering (#6689, Christian Despres).
    + Fix parsing of "show name" in `\newtheorem` (#6734).
      Previously we were just treating it as a string and
      ignoring  accents and formatting.
    + Prevent wrong nesting of `\multirow` and `\multicolumn` table
      cells (#6603, Laurent P. René de Cotret).
    + Table cell parser not consuming spaces correctly (#6596,
      Laurent P. René de Cotret).
    + Change `SIRange` to `SIrange` (#6617, Emerson Harkin).
    + Allow blank lines inside `\author` (#6324).

  * DocBook reader:

    + Don't squelch space at end of emphasis and other inline elements;
      instead, move it outside the element (#6719).
    + Implement table cell alignment (#6698, Nils Carlson).
    + Implement column span support for tables (#6492, Nils Carlson).
    + Update list of block level tags (#6610).


  * JATS reader:

    + Don't squelch space at end of emphasis and other inline elements;
      instead, move it outside the element (#6719).

  * RST reader:

    + Apply `.. class::` directly to following Header rather than creating
      a surrounding Div (#6699).

  * Docx reader:

    + Allow empty dates in comments and tracked changes (#6726, Diego
      Balseiro).

  * Markdown writer:

    + Be less aggressive about using quotes for YAML values,
      allowing e.g. a quotation mark or bracket as long as it's not at the
      beginning of the line.
    + Use double quotes for YAML metadata (#6727).
    + Sort YAML metadata keys in Markdown output case-insensitive.


  * Asciidoc writer:

    + Support asciidoctor's block figures (#6538, argent0).

  * LaTeX writer:

    + Fix spacing issue with list in definition list.
      When a list occurs at the beginning of a definition list definition,
      it can start on the same line as the label, which looks bad.
      Fix that by starting such lists with an `\item[]`.

  * HTML writer:

    + Support intermediate table headers (#5314, Albert Krewinkel).
    + Support attributes on all table elements (Albert Krewinkel).
    + Render table footers if present (#6314, Albert Krewinkel).
    + Fix addition of `doc-biblioentry` role.
    + Support colspans and rowspans in HTML tables (#6312, Albert Krewinkel).

  * ICML writer:

    + Support internal document links (#5541, Leonard Rosenthol).
    + Changed default link state to invisible (#6676, Leonard Rosenthol).

  * Docx writer:

    + Better handle list items whose contents are lists (#5948, Michael
      Hoffmann).  If the first element of a bulleted or ordered list is
      another list, then that first item would previously disappear if the
      target format is docx.
    + Separate adjacent tables (#4315).  Word combines adjacent tables,
      so to prevent this we insert an empty paragraph between two
      adjacent tables.

  * Org writer:

    + Don't force blank line after headings (#6554).

  * OpenDocument writer:

    + Implement table cell alignment (#6700 Nils Carson, Mauro Bieg).
    + New table cell support with row and column spans (#6682, Nils Carson).
    + Syntax highlighting for inline code (#6711, niszet).

  * Add Text.Pandoc.Citeproc module, exporting `processCitations`
    [API change].  This depends on several other, unexported
    modules under Text.Pandoc.Citeproc.

  * Add module Text.Pandoc.Writers.CslJson, exporting `writeCslJson`.
    [API change]

  * Add module Text.Pandoc.Readers.CslJson, exporting `readCslJson`.
    [API change]

  * Add module Text.Pandoc.Readers.BibTeX, exporting `readBibTeX` and
    `readBibLaTeX`. [API change]

  * Text.Pandoc.Filter: Add `CiteprocFilter` constructor to Filter.
    [API change] This runs the processCitations transformation.
    We need to treat it like a filter so it can be placed
    in the sequence of filter runs (after some, before others).
    In FromYAML, this is parsed from `citeproc` or `{type: citeproc}`,
    so this special filter may be specified either way in a defaults file
    (or by `citeproc: true`, though this gives no control of positioning
    relative to other filters).

  * Add new exported module Text.Pandoc.Writers.AnnotatedTable [API change]
    (#6655, Christian Despres).  This module (which should generally
    be imported qualified to avoid name conflicts) provides a
    `Table` type that mirrors the structure of a pandoc `Table`,
    but with added inferred information so that the writers do not have to
    lay out tables themselves. The `toTable` and `fromTable` functions convert
    between an annotated `Table` and a regular pandoc `Table`. In addition to
    producing a `Table` with coherent and well-formed annotations, the
    `toTable` function also normalizes its input table like the table
    builder does.  Tests ensure that `toTable` normalizes tables exactly
    like the table builder, and that its annotations are coherent.

  * Text.Pandoc.Logging:

    + Remove unused `CouldNotParseYamlMetadata` constructor for `LogMessage`
      [API change].
    + Add `CiteprocWarning` constructor to `LogMessage` [API change].

  * Text.Pandoc.Readers.Metadata: export `yamlBsToRefs` [API change].
    These allow specifying an id filter so we parse only references
    that are used in the document.

  * Text.Pandoc.Parsing:

    + Export ParseError [API change].
    + Add `stateInNote` and `stateNoteNumber` to `ParserState`
      [API change].  These are used to populate note numbers for citation
      processing.

  * Fix apparent typos in sample.lua (#6729, William Lupton).
    Also make the writer less aggressive in escaping quotes.

  * Text.Pandoc.Options:

    + `defaultMathJaxURL`: use `tex-chtml-full` instead of `tex-mml-chtml`
      (#6599, Kolen Cheung).  This drops the MathML support (which we
      don't need for HTML math rendering) and includes the full JavaScript,
      which makes it possible to use `--self-contained` (though there may
      still be issues if the required math fonts aren't available).  This
      change should also reduce latency in pages with lots of formulas.
    + Add `/tex-chtml-full.js` to `defaultMathJaxURL` (#6593) Previously we
      added this in processing command line options, but not in processing
      defaults files, which was inconsistent.

  * epub.css: Fix cover page selectors and add note explaining their use
    (#6649, a-vrma).

  * Add data files needed for Text.Pandoc.Citeproc:  these include
    `default.csl` in the data directory and a `citeproc` directory that
    is only used at compile-time for biblatex localizations.  Note that we've
    added `file-embed` as a mandatory rather than a conditional
    dependency, because of the biblatex localization files.

  * Lua filters:

    + Add SimpleTable for backwards compatibility (#6575, Albert Krewinkel).
      A new type `SimpleTable` is made available to Lua filters. It is
      similar to the `Table` type in pandoc versions before 2.10;
      conversion functions from and to the new Table type are provided.
      Old filters using tables now require minimal changes and can use, e.g.,

          if PANDOC_VERSION > {2,10,1} then
            pandoc.Table = pandoc.SimpleTable
          end

      and

          function Table (tbl)
            tbl = pandoc.utils.to_simple_table(tbl)
            …
            return pandoc.utils.from_simple_table(tbl)
          end

      to work with the current pandoc version.

    + Make `attr` argument optional in `Table` constructor (Albert Krewinkel).
      This changes the Lua API. It is highly unlikely for this change to affect
      existing filters, since the documentation for the new Table constructor
      (and type) was incomplete and partly wrong before.  The Lua API is now
      more consistent, as all constructors for elements with attributes now
      take attributes as the last parameter.

  * MANUAL.txt:

    * Add a dedicated Citations section which consolidates the information
      the manual used to contain about citation processing, and incorporates
      some information formerly found in the pandoc-citeproc man page.
    + Add note about lualatex using `selnolig`.
    + Remove duplicate `seriespage` (#6568, Blake Eryx).
    + Remove lists of support extensions for markdown variants (#6604).
      Instead, offer the advice to use `--list-extensions=FORMAT`.
    + Fix position of attributes in header (Albert Krewinkel).
    + Delete obsolete section on compact and loose lists (#6684).


  * doc/lua-filters.md:

    + Add info on how to debug Lua filters (#6732, Ian Max Andolina).
    + Document Underline type and constructor (Albert Krewinkel).
    + Document `body` field (Albert Krewinkel).
    + Add missing header attribute
    + Add missing Link.title field (Albert Krewinkel).
    + Make the setting-the-date example conditional (the-solipsist).
      This makes the example a bit more realistic.
    + Remove outdated link table example.

  * doc/org.md:

    + Add section on tables (Albert Krewinkel).
    + Add section on handling of unknown directives (Albert Krewinkel).

  * CONTRIBUTING.md: fix typo (#6584, Dmitry Volodin).

  * Use golden test framework for command tests.  This means that
    `--accept` can be used to update expected output.

  * Use the `smart` extension when generating pandoc's man page (#6613).

  * Release-candidate: don't build windows i386.
    So far we haven't been able to figure out how to get
    stack to use a 32-bit ghc.

  * Use `null` instead of deprecated `Builder.isNull`.

  * Makefile:

    + Fix macospkg target to fetch target from S3 artifacts.
    + Fix pandoc-templates target to include all partials.

  * Remove duplicated dependency in pandoc.cabal (#6591, Felix Yan).

  * Sort build depends in pandoc.cabal alphabetically (#6691,
    Albert Krewinkel).

  * Add .travis.yml for macos release candidate build (#6622).
    We need to build the release candidate on Travis rather
    than GitHub actions, because GH has macos 10.15, and
    binaries compiled on that OS will not work with 10.13.
    This build is only triggered on `rc/*` branches.

  * Remove instructions for building pandoc-citeproc from CI and
    release binary build instructions.  We will no longer distribute
    pandoc-citeproc.

  * Fix math rendering in trypandoc (this broke after
    commit d8ad766d17603784b86fc5c2e1b22864125d04d1).

  * Use latest versions of skylighting, commonmark (#6589),
    comonmark-extensions, commonmark-pandoc, texmath.

  * Relax version bounds for hslua, hslua-module-text, bytestring.

  * Use released pandoc-types 1.22.  This changes the JSON
    encoding slightly for the new table types introduced
    in 1.21, so they're more consistent with the rest.
    Developers of libraries for pandoc filters will want
    to take note.

  * Fix hlint suggestions, update hlint.yaml (#6680, Christian Despres).

  * Code cleanup (#6678, Joseph C. Sible).

  * Add haddocks to functions in Text.Pandoc.Writers.Shared (Albert
    Krewinkel).

  * Remove duplicate `tshow` definition.

  * Linux release candidate build: use ghc-musl container.  This simplifies
    our build process (over using a customized alpine container).


## pandoc 2.10.1 (2020-07-23)

  * Add `commonmark_x` output format. This is `commonmark` with a number
    of useful pandoc extensions enabled.

  * Many more extensions now work with `commonmark` and `gfm`.

  * Add generic `attributes` extension.  This allows attributes to
    be added to any block or inline element in a uniform way.  Since
    the Pandoc AST doesn't include attributes on each element type,
    the attributes will sometimes be added by creating a surrounding
    Div or Span container.  Currently this extension is only
    compatible with the `commonmark` and `gfm` readers.

    To add an attribute to a block-level element, e.g. a paragraph,
    put it before the block:
    ```
    {#mypara}
    This is a paragraph.
    ```
    Multiple attributes may be used and will be combined:
    ```
    {#mypara}
    {.blue .warning key="val"}
    This is a paragraph.
    ```
    To add an attribute to an inline-level element, put it
    immediately after the element:
    ```
    *emphasized text*{.special}
    ```

  * Support `--number-sections` for docx output (#1413).

  * LaTeX reader:

    + Support `\SIRange` reader (#6418, Emerson Harkin).
    + Support table col-span and row-span (#6311, Laurent P. René de Cotret).
      Supports `\multirow` and `\multicolumn`.
    + Support amsthm:  `\newtheorem`, `\theoremstyle`, and theorem and
      proof environments, including labels and references.  The only thing
      that is unsupported is the second optional argument, which causes
      numbering to be reset after the specified series is incremented.
    + Moved some code to T.P.LaTeX.Parsing.  We need to reduce the size
      of the LaTeX reader to ease compilation on resource-limited systems.

  * RST reader:

    + Fix csv tables with multiline cells (#6549).
    + Fix spurious newlines in some attributes from directives.
    + Avoid extra newline in included code blocks.

  * Commonmark reader:

    + Switch from cmark-gfm to commonmark-hs for commonmark and gfm
      parsing.  This avoids depending on a C library
      and allows us to support more pandoc extensions for
      `commonmark` and `gfm`.

  * DocBook reader:

    + Parse releaseinfo as metadata (#6542).

  * Docx reader:

    + Only use `bCs/iCs` on runs with `rtl` or `cs` property (#6514, Nikolay
      Yakimov).
    + Code cleanup/refactoring (Nikolay Yakimov).

  * Org reader (Albert Krewinkel):

    + Respect export setting which disables entities
      MathML-like entities, e.g., `\alpha`, can be disabled with the
      `#+OPTION: e:nil` export setting (Albert Krewinkel).
    + Respect export setting disabling footnotes.  Footnotes can be removed
      from the final document with the `#+OPTION: f:nil` export setting.
    + Respect tables-excluding export setting.  Tables can be removed from
      the final document with the `#+OPTION: |:nil` export setting.

  * Markdown writer:

    + Move `asciify` out of `escapeString`.  Otherwise `unsmartify`
      doesn't catch quotes that have already been turned to entities.
    + Add `writeCommonmark` (new exported function, API change).
    + Use unicode super/subscript characters when possible if the
      `superscript` or `subscript` extension or `raw_html` aren't available.
    + Render caption as following paragraph when `table_caption` extension
      is not enabled.
    + Use numerical labels for reference links that are longer
      than 999 characters or contain square brackets, for conformity
      with commonmark (#6560).

  * Commonmark writer:

    + Instead of using cmark-gfm, use `writeCommonmark` from the
      Markdown writer.  This function calls the markdown writer
      with appropriate extensions and a few small modifications
      (e.g. not requiring backslashes before spaces inside
      super/subscripts).  With this change `comonmark` and
      `gfm` output can be used with a wider selection of
      extensions.

  * Jira writer: keep image caption as alt attribute (#6529, Albert
    Krewinkel).

  * HTML writer:

    + Improve alt-text/caption handling for HTML5 (#6491, Albert Krewinkel).
      Screen readers read an image's `alt` attribute and the figure caption,
      both of which come from the same source in pandoc. The figure caption is
      hidden from screen readers with the `aria-hidden` attribute. This
      improves accessibility.  For HTML4, where `aria-hidden` is not allowed,
      pandoc still uses an empty `alt` attribute to avoid duplicate contents.

  * Ms writer:

    + Fix code highlighting with blank lines.  Previously blank lines
      were simply omitted from highlighted code.
    + Escape starting periods in ms writer code blocks (#6505, Michael
      Hoffmann).  If a line of ms code block output starts with a period (.),
      it should be prepended by `\&` so that it is not interpreted as a roff
      command.

  * Text.Pandoc.Extensions:

    + Add `raw_markdown` extension (which only affects `ipynb`
      input).
    + Trim down `githubMarkdownExtensions`.
      Previously it included all of the following, which make
      sense for the legacy `markdown_github` but not for `gfm`,
      since they are part of base commonmark and thus
      can't be turned off in `gfm`:

      - `Ext_all_symbols_escapable`
      - `Ext_backtick_code_blocks`
      - `Ext_fenced_code_blocks`
      - `Ext_space_in_atx_header`
      - `Ext_intraword_underscores`
      - `Ext_lists_without_preceding_blankline`
      - `Ext_shortcut_reference_links`

      These have been removed from `githubMarkdownExtensions`, though
      they're still turned on for legacy `markdown_github`.
    + Add `Ext_attributes` constructor for `Extension` [API change].

  * LaTeX template: use selnolig to selectively suppress ligatures with
    lualatex (#6534).

  * Benchmark bytestring readers (Nikolay Yakimov).

  * Documentation:

    + Update using-the-pandoc-api.md (favonia).
    + Fix Typos in lua-filters.md (tajmone).
    + Rewrite Raw HTML/TeX section in MANUAL.txt to avoid duplicate
      headings for the extensions.
    + Fix typo in MANUAL.txt (Benjamin Wuethrich).
    + Remove duplicate 'titlepage' in MANUAL.txt (Blake Eryx).
    + CONTRIBUTING.md: Advertise the official nightlies in GitHub actions.
      Replaces #6500, thanks to @ickc.



## pandoc 2.10 (2020-06-29)

  * Use pandoc-types 1.21.  This adds two things:

    + A native Underline constructor for Inline (#6277, Vaibhav Sagar).
    + More expressive types for tables (#1024, Christian Despres).
      Tables can now take attributes; and rowspans and colspans,
      column headers, multiple row headers, table head and foot
      can all be represented.  (Note, however, that reader and
      writer support for these features is still lacking, so
      most users won't see any differences in table conversion
      yet.  These changes just lay the foundation for further
      improvements.)

  * Support new Underline element in readers and writers (#6277,
    Vaibhav Sagar).

  * Support new Table type (Christian Despres).
    The Builder.simpleTable now only adds a row to the TableHead
    when the given header row is not null. This uncovered an
    inconsistency in the readers: some would unconditionally
    emit a header filled with empty cells, even if the header
    was not present. Now every reader has the conditional
    behaviour.  Only the XWiki writer depended on the header row
    being always present; it now pads its head as necessary.

  * Add an option to disable certificate validation (#6156, Cédric Couralet,
    Cécile Chemin, Juliette Fourcot).  This commit adds the option
    `--no-check-certificate`, which disables certificate checking when
    resources are fetched by HTTP.

  * Unify defaults and markdown metadata parsers (#6328, Nikolay
    Yakimov). Clean up code in Text.Pandoc.Readers.Metadata and
    properly handle errors in `yamlToMeta`.  This fixes parsing
    of Boolean fields in metadata withinin defaults files and reduces
    code duplication.

  * Docbook reader:

    + Implement `<procedure>` (#6442, Mathieu Boespflug).
    + Implement `<phrase>` (#6438, Mathieu Boespflug).
    + Treat envar and systemitem like code (#6435, Mathieu Boespflug).
    + Implement `<replaceable>` (#6437, Mathieu Boespflug)
    + Map `<simplesect>` to unnumbered section (#6436, Mathieu Boespflug).

  * JATS reader:

    + Handle "label" element in section title (#6288).
    + Parse abstract element into metadata field of same name
      (#6480, Albert Krewinkel).

  * Jira reader (Albert Krewinkel):

    + Resolve multiple parsing problems, including issues with empty
      table cells, faulty recognition of closing emphasis characters, and
      parsing of image attributes (#6212, #6219, #6220).
    + Two consecutive markup chars are now parsed verbatim (#6343);
      styled text must not be empty.
    + Newlines are no longer allowed within styled text (#6325).
    + Links to anchors are now parsed as links (#6407).
    + Retain image attributes (#6234).  Jira images attributes as in
      `!image.jpg|align=right!` are retained as key-value pairs. Thumbnail
      images, such as `!example.gif|thumbnail!`, are marked by a
      `thumbnail` class in their attributes.
    + Use Underline for inserted text (#6237). Previously, the span was
      marked with the non-standard class `inserted`.
    + Improve icon conversion for `(/)`, `(x)`, `(!)`, `(?)`
      `(+)`, `(-)`, `(off)`, `(*)`. (#6236, #6264).
    + Support citations, attachment links, and user links (#6231, #6238,
      #6239).
    + Resolve parsing issues of blockquote, color (#6233, #6235).

  * HTML reader:

    + Parse attributes into table attributes.
    + Support `<bdo>` (#5794, Tristan de Cacqueray).
    + Add `summary` to list of block-level HTML tags (#6385).
      This improves support for summary/details inside Markdown.
      NOTE:  you need to include a blank line before the closing
      `</details>`, if you want the last part of the content to
      be parsed as a paragraph.
    + Fix parsing unclosed th elements in a table (#6247).

  * Commonmark reader: Implement `implicit_figures` extension (#6350).

  * Markdown Reader:

    + Fix inline code in lists (#6284, Nikolay Yakimov).
      Previously inline code containing list markers was sometimes
      parsed incorrectly.
    + Don't require blank line after grid table (#6481).
      This allows grid tables to be enclosed in fenced divs with no
      intervening blank lines.

  * LaTeX reader:

    + Don't parse beyond `\end{document}` (#6380).
      This required some internal changes to `\subfile` handling.
    + Better handling of `\lettrine`.  SmallCaps instead of Span
      for the part after the initial capital.  Ensure that both
      arguments are parsed, so that in Markdown both
      are treated as raw LateX. (Closes #6258.)

  * Org reader (Albert Krewinkel):

    + Recognize images with uppercase extensions (#6472).
    + Keep unknown keyword lines as raw org.  The lines of unknown
      keywords, like `#+SOMEWORD: value` are no longer read as metadata,
      but kept as raw `org` blocks. This ensures that more information
      is retained when round-tripping org-mode files; additionally,
      this change makes it possible to support non-standard org
      extensions via filters.
    + Unify keyword handling.  Handling of export settings and other
      keywords (like `#+LINK`) has been combined and unified.
    + Support `LATEX_HEADER_EXTRA` and `HTML_HEAD_EXTRA`
      settings.  These export settings are treated like their non-extra
      counterparts, i.e., the values are added to the `header-includes`
      metadata list.
    + Allow multiple `#+SUBTITLE` export settings.  The values of all
      lines are read as inlines and collected in the `subtitle`
      metadata field.
    + Read `#+INSTITUTE` values as text with markup.  The value is
      stored in the `institute` metadata field and used in the
      default beamer presentation template.
    + The behavior of the `#+AUTHOR` and `#+KEYWORD` export
      settings has changed: Org now allows multiple such lines
      and adds a space between the contents of each line. Pandoc
      now always parses these settings as meta inlines; setting
      values are no longer treated as comma-separated lists.
      Note that a Lua filter can be used to restore the previous
      behavior.
    + Read description lines as inlines (#6485). `#+DESCRIPTION` lines
      are now treated as text with markup. If multiple such
      lines are given, then all lines are read and separated by soft
      linebreaks.
    + Honor tex export option (#4070).  The `tex` export option can be set
      with `#+OPTION: tex:nil` and allows three settings:
      `t` (the default) causes LaTeX fragments to be parsed as TeX or added
      as raw TeX.  `nil` removes all LaTeX fragments from the document.
      `verbatim` treats LaTeX as text.


  * RST reader:

    + Pass arbitrary attributes through in code blocks (#6465).
      Exceptions: name (which becomes the id), class (which becomes the
      classes), and number-lines (which is treated specially to fit
      with pandoc highlighting).
    + Handle `date::` directive (#6276).

  * Textile reader: support `pre.` for code blocks (#6454).

  * Ipynb reader:

    + Implement `raw_markdown` extension (#5408).  Specifying
      `-f ipynb+raw_markdown` will cause Markdown cells
      to be represented as raw Markdown blocks, instead of being
      parsed.  This is not what you want when going from `ipynb`
      to other formats, but it may be useful when going from `ipynb`
      to Markdown or to `ipynb`, to avoid semantically insignificant
      changes in the contents of the Markdown cells that might
      otherwise be introduced.
    + Handle application/pdf output as image (#6430).
    + Properly handle image/svg+xml as an image (#6430).

  * Docx reader:

    + Distinguish between docx parsing and docx container unpacking errors.

  * MediaWiki reader:

    + Fix `gfm_auto_identifiers` so that `-` is not replaced by `_` (#6335).

  * Vimwiki reader:

    + Add nested syntax highlighting (#6256, Vlad Hanciuta).
      Nested syntaxes are specified like this:
      ```
      {{{sql
      SELECT * FROM table
      }}}
      ```
      The preformatted code block parser has been extended to check if the
      first attribute of the block is not a `key=value` pair, and in that
      case it will be considered as a class.


  * Jira writer (Albert Krewinkel):

    + Always escape braces (#6478).  Braces are now always escaped, even
      within words or when surrounded by whitespace. Jira and
      Confluence treat braces specially.
    + Convert Underline to inserted text (`+inserted+`).
    + Add image attributes (#6234).  Image attributes are added
      to the output as image parameters. If the image has a
      class "thumbnail", then a thumbnail image is generated;
      all other attributes are discarded in this case.

  * LaTeX writer:

    + Ensure that `-M csquotes` works even in fragment mode (#6265).
    + Escape `^` specially for listings (#6460).
    + Create hypertarget for links with identifier (#6360).
    + Distinguish between single and double quotes when using enquote
      package (#6457, dbecher-ito).
    + Add support for customizable alignment of columns in beamer (#6331,
      andrebauer).
    + Add support for customizable alignment of columns in beamer
      (#4805, #4150, andrebauer).

  * HTML writer:

    + Use CSS in favor of `<br>` for display math (#6372)
      Some CSS to ensure that display math is
      displayed centered and on a new line is now included
      in the default HTML-based templates; this may be
      overridden if the user wants a different behavior.

  * Org writer:

    + Clean-up Div handling (Albert Krewinkel).

  * Docx writer:

    + Enable column and row bands for tables (#6371).
      This change will not have any effect with the default style.
      However, it enables users to use a style (via a reference.docx)
      that turns on row and/or column bands.

  * OpenDocument (and ODT) writer:

    + Add custom-style "Abstract" in metadata abstract.
      This ensures that the abstract is rendered with style Abstract.
    + Enable custom-style attribute on a Div.
      This allows you to apply a custom style to contained paragraphs.

  * DocBook writer:

    + Add id of figure to enclosed image.
    + Add personname element to docbook author (#6244).

  * FB2 writer:

    + Properly handle cover-image containing spaces (#6391).

  * Markdown writer:

    + Ensure consistent padding for pipe tables (#6240).
    + Avoid unnecessary escapes before intraword `_` when
      `intraword_underscores` extension is enabled (#6296).

  * RST writer:

    + Properly handle images with same alt text (#6194).
      Previously we created duplicate references for these in rendering RST.

  * AsciiDoc writer:

    + Add blank line after Div (#6308).

  * Haddock Writer:

    + Support Haddock tables (Joe Hermaszewski).
      See this PR on Haddock for details on the table format:
      <https://github.com/haskell/haddock/pull/718>.

  * PowerPoint writer (Jesse Rosenthal):

    + Write math input verbatim in speaker notes (#6301).
      OMML in speaker notes would lead to corrupt PowerPoint output. We now
      output the OMML verbatim as LaTeX in the speaker notes.

  * LaTeX template: Make polyglossia package options list-aware
    (#6444, Frederik Elwert).

  * Reveal.js template:

    + Update template for reveal.js 4.0.0 (#6390, Salim B).
    + Update template with newly available options (#6347, Jake Zimmerman).
    + Use CDN version of revealjs v4 by default (#6408).

  * opendocument template: Add abstract and subtitle to opendocument
    template (#6369).

  * reference.odt: clean up styles.  Add Abstract.
    Change Author, Date to centered paragraphs with no character
    styling.

  * epub.css: wrap overlong lines in highlighted code blocks (#6242).
    This fixes a problem in iBooks v2.4 with our earlier
    horizontally scrolling code blocks.  The problem seems to
    be a bug in iBooks, not pandoc, but since iBooks is a major
    target we're changing pandoc's default behavior so that
    pandoc-produced epubs work on that platform.

  * Text.Pandoc.PDF:

    + Use `--enable-local-file-access` in invoking `wkhtmltopdf` (#6474).
      `wkhtmltopdf` changed in recent versions to require this for
      access to local files.  This fixes PDF via HTML5 with `--css`.
    + Send verbose output to stderr, not stdout (#6483).

  * Text.Pandoc.MIME: Fix MIME type for TrueType fonts in EPUBs
    (#6464, Michael Reed).

  * Text.Pandoc.Shared:

    + `makeSections`: omit number attribute when unnumbered class
      is present (#6339).  Previously the attribute was included but given
      an empty value, and this caused the table of contents creation
      functions in Text.Pandoc.Writers.Shared to think these items had
      numbers, which meant that they were included in the TOC even if the
      `unlisted` class was used.
    + Deprecate `underlineSpan` in Shared in favor of
      `Text.Pandoc.Builder.underline` (Vaibhav Sagar).
    + `renderTags'`: use self-closing tag for col element (#6295).

  * Text.Pandoc.UUID: Fix `getRandomUUID`, which previously would
    return the same value twice in a row. Make `getRandomUUID`
    polymorphic in PandocMonad.  Remove `getUUID` (#6228, Joseph C. Sible).

  * Text.Pandoc.Class: Generalize `PandocIO` functions to `MonadIO`.

  * Fixed Katex standalone script (#6399, Lucas Escot).
    Global macros are now persistent when using the HTML Writer with the
    `--katex` option.

  * Lua subsystem (Albert Krewinkel):

    + Use new type PandocLua for all pandoc Lua operations (API change).
      The new type `PandocLua` is an instance of the `PandocMonad` typeclass
      and can thus be used in a way similar to `PandocIO`.
    + Use PandocError for exceptions in Lua subsystem (API change).
      The PandocError type is used throughout the Lua subsystem. All Lua
      functions throw an exception of this type if an error occurs. The
      `LuaException` type is removed and no longer exported from
      `Text.Pandoc.Lua`. In its place, a new constructor `PandocLuaError`
      is added to PandocError.

  * Lua filters: improve error messages for failing filters (#6332,
    Albert Krewinkel).  Print the Lua error properly instead of
    displaying their `show` string.

  * Use latest skylighting.  This fixes a bug with lua multiline
    comments (and may improve handling of other syntaxes as well).
    IT also adds `aria-hidden="true"` to the empty a elements, which
    helps people who use screen readers.

  * Use latest texmath.

  * Require latest doctemplates 0.8.2.
    This adds support for template pipes `first`, `rest`,
    `last`, `allbutlast`.

  * Revert  0e48a02 and dependency on base-noprelude, which hasn't
    been updated for ghc 8.10 (see #6187).

  * Dependency adjustments:

    + Allow haddock-library 1.9.x.
    + Allow hslua 1.1 (#6243, Felix Yan).
    + Allow base64-bytestring 1.1.
    + Use latest jira-wiki-markup.
    + Allow http-client 0.7.
    + Allow tasty 1.3.x.
    + Allow aeson 1.5 (#6400, Felix Yan).
    + Remove unused dependency `vector` (#6462, Laurent P. René de Cotret).
    + Bump QuickCheck upper bound.

  * Significant code cleanup and simplification (Joseph C. Sible, #6223,
    #6209, #6225, #6229, #6226, #6340).

  * Remove unnecessary hlint ignores (#6341, Joseph C. Sible).

  * Remove obsolete RelaxedPolyRec extension (#6487, Nikolay Yakimov).

  * trypandoc improvements (Mike Tzou):

    + Add standalone option to the command text (#6210).
    + Update third party libraries.

  * MANUAL.txt:

    + Clarify template partial naming (#6476, Mauro Bieg).
    + Describe `jira` as "Jira/Confluence wiki markup" (#6351, Albert
      Krewinkel).  In the past, Jira's wiki markup was also used by -- and
      could be imported into -- Atlassian Confluence.
    + Add link to print-css.rocks (#6272, Mauro Bieg).
    + Clarify pipe table column width adjustment (#6254).
    + Fix ATX header syntax.
    + Fix misleading note about image size conversions (#6353).
    + Update links to reveal.js documentation (#6386, Salim B).
    + Separate adjacent verbatim code blocks (#6307, tom-audm).

  * org.md:

    + Document behavior of `smart` extension (#4387, Albert Krewinkel).
    + Describe all supported export options in detail.

  * lua-filters.md:

    + Fix description of BulletList Lua type (Levi Gruspe).
    + Use pandoc.system module in TikZ example (Albert
      Krewinkel).  Showcase temporary directory handling with
      `with_temporary_directory` and `with_working_directory`.

  * INSTALL.md: fix FreeBSD port link (#6422, Mo).
    The FreeBSD port was renamed from pandoc to hs-pandoc in 2010.
    The old pandoc port is still at version 1.5.1.1

  * Propagate `(DY)LD_LIBRARY_PATH` in tests (#6376, Lila).

  * Bump `cabal-version` to 2.2 (#6377).

  * Make it possible to compile using Stack on NixOS (#6439, Mathieu
    Boespflug).

  * CI action to check for commit message length (Nikolay Yakimov, #6398).


## pandoc 2.9.2.1 (2020-03-23)

  * Markdown reader: Fix table alignment when heading begins with t (#6153).
    Due to a typo (`t` instead of `\t`) we were center aligning column
    headings that begin with a lowercase `t`!

  * Text.Pandoc.Readers.Roff:

    + Fix parsing of `\.` in man/ms readers (#6175).
      Previously due to a typo it was being parsed as `` ` ``.
    + Fix parsing of `\'` in man/ms readers (#6175).  It was being parsed
      as a backtick.

  * Jira reader (Albert Krewinkel):

    + Fix parsing of tables without preceding blankline (#6198).
      A bug was fixed which caused faulty parsing if a table was not
      preceded by a newline and the first table cell had no space
      after the initial `|` characters.
    + Fix parsing of strikeout, emphasis (#6196).  A bug was fixed which
      caused non-emphasized text containing digits and/or non-special
      symbols (like dots) to sometimes be parsed incorrectly.
    + Support colored inline text, indented lists (#6183, #6184).

  * Ms writer:

    + Fix definition lists so indent even when paragraph indent is
      set to 0 (as is the default).  Also ensure indent for display math
      that falls back to TeX.
    + Use `.QS/.QE` instead of `.RS/.RE` for block quotes.

  * EPUB writer: fix regression on detection of front/back/bodymatter
    (#6170).  This bug caused sections with epub:type `dedication` to be
    misplaced in bodymatter instead of frontmatter as specified
    in the manual.  The same problem would affect other epub:types.
    The pattern matching needed to be changed with the use of
    `makeSection`.

  * AsciiDoc writer:  remove redundant `otherwise` guard in
    `inlineToAsciiDoc` (#6146, Ryan Scott).

  * Text.Pandoc.Class:

    + Fix missing import when data files are not embedded (Albert Krewinkel).
    + Subdivide Text.Pandoc.Class into small unexported modules
      and ensure that all functions have Haddock documentation (#6106,
      Albert Krewinkel).
    + Finer grained imports of Text.Pandoc.Class submodules (#6203, Albert
      Krewinkel).

  * Text.Pandoc.XML: Add to list of HTML5 attributes:
    `allow`, `autocapitalize`, `decoding`, `enterkeyhint`,
    `imagesizes`, `imagesrcset`, `loading`.

  * Use implicit Prelude (#6187, Albert Krewinkel).
    The previous behavior was introduced as a fix for #4464. It seems that
    this change alone did not fix the issue, and `stack ghci` and `cabal
    repl` only work with GHC 8.4.1 or newer, as no custom Prelude is loaded
    for these versions. Given this, it seems cleaner to revert to the
    implicit Prelude.

  * Always use custom prelude (#6187, Albert Krewinkel).
    Previously, the custom prelude was used only with older GHC versions, as
    a workaround for problems with ghci. The ghci problems are resolved by
    replacing package `base` with `base-noprelude`, allowing for consistent
    use of the custom prelude across all GHC versions.

  * Remove outdated checks for no longer supported base versions
    (Albert Krewinkel).

  * PDF via wkhtmltopdf: put user-specified options last (#6171).
    Certain options (e.g. `cover`) need to come after flags on
    the command line.

  * Text.Pandoc.App: set resource path at the beginning so it can affect
    things like include-in-header (#5982).

  * Change macOS release candidate CI process so that notarized
    packages can be produced (#6169).

  * Make MANUAL more explicit about nonbreaking space handling by
    `all_symbols_escapable` (#6154, Fabien Schurter).

  * trypandoc (Mike Tzou):

    + Add checkbox for standalone option (#6189).
    + Use strict mode for JavaScript code (#6188).
    + Fetch resources over https (#6188).
    + Remove unnecessary attributes on style, script elements (#6188).

  * Use details tag to make GitHub releases changelog collapsible.

  * Update filter code in doc/filters.md so it works with latest pandoc
    (#6185).

  * linux/Dockerfile: upgrade to alpine 3.11 (#6180, Albert Krewinkel).
    This is used to build the static linux binaries.


## pandoc 2.9.2 (2020-02-15)

  * Add `csv` as an input format (#6100).  The CSV table is converted into a
    pandoc simple table.  A new module Text.Pandoc.Readers.CSV
    exports `readCSV` [API change].

  * Introduce new format variants for JATS writer (#6014, Albert Krewinkel):

    - `jats_archiving` for the "Archiving and Interchange Tag Set",
    - `jats_publishing` for the "Journal Publishing Tag Set", and
    - `jats_articleauthoring` for the "Article Authoring Tag Set."

    The `jats` output format is now an alias for `jats_archiving`.
    The module Text.Pandoc.Writers.JATS now exports
    `writeJatsArchiving`, `writeJatsPublishing`, and
    `writeJatsArticleAuthoring`, as well as the legacy
    `writeJATS` [API change].

  * `--defaults`: Support `bibliography` and `csl` fields.
    Move `addMeta` from Text.Pandoc.App.CommandLineOptions to
    Text.Pandoc.App.Opt (internal change).

  * Add timing info for filters in `--verbose` mode (#6112).
    When verbose mode is specified (verbosity == INFO), print a
    notice when running a filter and when a filter completes (including
    timing).

  * LaTeX reader:

    + Allow `&` in LaTeX citation keys (#6110).
    + Improve caption and label parsing.
    + Don't emit empty Span elements for labels.
    + Put tables with labels in a surrounding Div.
    + Resolve `\ref` to table numbers (#6137).
    + Skip comments in more places where this is needed (#6114).
    + Allow beamer overlays for all commands in all raw tex (#6043).
      This affects parsing of raw tex in LaTeX and in Markdown and
      other formats.
    + Improve parsing of raw environments (#6034).  If parsing fails
      in a raw environment (e.g. due to special characters like unescaped
      `_`), try again as a verbatim environment, which is less sensitive to
      special characters.  This allows us to capture special environments
      that change catcodes as raw tex when `-f latex+raw_tex` is used.

  * RST reader:

    + Add highlight directive (#6140, Lucas Escot).

  * MediaWiki writer:

    + Prevent triple `[[[` which confuses MediaWiki (#6119).

  * HTML reader:

    + Don't parse `data-id` as `id` attribute.  And similarly don't
      parse any `data-X` as `X` when `X` is a valid HTML attribute.

  * Org reader:

    + Simplify parsing of sub- and superscripts (#6127, Albert Krewinkel).
      Speeds up parsing of single-word, markup-less sub- and superscripts.

  * LaTeX writer:

    + Group biblatex citations even with prefix and suffix (#5849, Ethan
      Riley).  Previously biblatex citations were only grouped if there
      was no prefix.  This patch allows them to be grouped in subgroups split
      by prefixes and suffixes, which allows better citation sorting.
    + Fix regression in handling of columns in beamer slides (#6033).
      Columns in title slides were causing problems with
      slide division.
    + Fix duplicate frame classes in LaTeX/Beamer output (#6107).

  * HTML writer:

    + Fix duplicate attributes on headings (#6062), regression from 2.7.x.
    + Fix `--number-offset` with HTML TOC.  Eventually it would be worth
      adding a parameter to `makeSections` so this could be done at that
      level; then it would also affect other writers that construct
      TOC manually.
    + reveal.js: restore old behavior for 2D nesting (#6032).
      The fix to #6030 actually changed behavior, so that the
      2D nesting occurred at slide level N-1 and N, instead of
      at the top-level section.  This commit restores the v2.7.3 behavior.
      If there are more than 2 levels, the top level is horizontal
      and the rest are collapsed to vertical.
    + reveal.js: ensure that pauses work even in title slides (#5819).

  * Markdown writer:

    + Fix regression: spurious dots in markdown_mmd metadata output (#6133).

  * Docx writer:

    + Fix regression with Compact style on tight lists (#6072).
      Starting in 2.8, the docx writer no longer distinguishes between tight
      and loose lists, since the Compact style is omitted.  This is a
      side-effect of the fix to #5670, as explained in the changelog.  This
      patch fixes the problem by extending the exception currently offered to
      Plain blocks inside tables to Plain blocks inside list items.

  * Jira writer:

    + Fix output of table headers (Albert Krewinkel, #6035).

  * Add Text.Pandoc.Image with unexported svgToPng.

  * Text.Pandoc.XML: Export `html5Attributes`, `html4Attributes`,
    `rdfaAttributes` (formerly unexported in Text.Pandoc.Writers.HTML).
    [API change]

  * Text.Pandoc.Shared: Export a new function `findM` (#6125,
    Joseph C. Sible).

  * Text.Pandoc.Logging: Add `RunningFilter`, `FilterCompleted`
    constructors to LogMessage [API change].

  * Text.Pandoc.CSV: fix bug in CSV parser; previously an extra blank record
    would sometimes be inserted at the end.

  * LaTeX template: add space option to xeCJK with PassOptionsToPackage
    (#6002).  Otherwise we can get a clash with documentclasses that
    already load the package.

  * Lua filters:

    + Allow filtering of element lists (#6038, Albert Krewinkel).  Lists of
      Inline and Block elements can now be filtered via `Inlines` and
      `Blocks` functions, respectively. This is helpful if a filter
      conversion depends on the order of elements rather than a single
      element.  For example, the following filter can be used to remove all
      spaces before a citation:

          function isSpaceBeforeCite (spc, cite)
            return spc and spc.t == 'Space'
             and cite and cite.t == 'Cite'
          end

          function Inlines (inlines)
            for i = #inlines-1,1,-1 do
              if isSpaceBeforeCite(inlines[i], inlines[i+1]) then
                inlines:remove(i)
              end
            end
            return inlines
          end

    + Add methods `insert`, `remove`, and `sort` to pandoc.List
      (Albert Krewinkel).  Example of use:

          local numbers = pandoc.List {2, 3, 1}
          numbers:sort()     -- numbers is now {1, 2, 3}
    + Make `pandoc.List` a callable constructor (Albert Krewinkel).
      It is now possible to construct a new List via
      `pandoc.List()` instead of `pandoc.List:new()`.
    + Add tests for pandoc.List module (Albert Krewinkel).

  * Text.Pandoc.App.CommandLineOptions: Change `setVariable` to use `Text`
    instead of `String`.  This avoids some unnecessary unpacking.

  * Use versioned directory for windows release zipfile.
    Also remove old `make-windows-installer.bat`, superseded by GitHub
    actions workflow, and modify `pandoc.wxs` for new paths.

  * Extensive code cleanup (#6141, #6128, #6129, #6130, #6123,
    #6105, 6102, #6117, #6124, #6115, #6116, #6111, Joseph C. Sible).

  * Fix hlint warnings (Albert Krewinkel).

  * Use latest doclayout, doctemplates (#6031).  The new version of
    doclayout fixes a memory leak that affected `--include-in-header` with
    large files (and possibly other cases involving extremely long lines).

  * Use latest texmath.

  * Use latest skylighting and fix test suite (#6086).

  * sample.lua: Fix typo in descriptive comments (#6136, Caleb Maclennan).
    Fix typo in error message (#6135).

  * Add Docker and GH Actions instructions/links to INSTALL.md.

  * Update filter documentation (#6065). Improve cabal v2 instructions.
    Remove example using pandoc API directly (we have other
    docs for that and it was outdated).

  * Lua filter docs:

    + Cross-link constructors and types (Albert Krewinkel).
      Thanks to @bpj for the idea.
    + Sort pandoc.List methods alphabetically (Albert Krewinkel).
    + Unify, fix anchors and internal links (#6061, Albert Krewinkel).
      Links and anchors now follow consistent conventions, like
      lowercase-only anchor names.  This breaks some links to specific
      sections in the document, but will make it much easier to link
      documentation in the future.
    + Clarify filter function execution order (#6059, Albert Krewinkel).

  * In docs, update URLs and use `https:` wherever possible (#6090,
    Salim B).

## pandoc 2.9.1.1 (2020-01-05)

  * Markdown reader:

    + Fix parsing bug affected indented code after raw HTML (#6009, #5360).

  * LaTeX writer:

    + Fix regression in beamer slide structure with certain slide levels
      (#6030).
    + Allow framebreaks for beamer's TOC (Heiko Schlittermann, #6012)
    + Properly handle unnumbered headings level 4+ (#6018).
      Previously the `\paragraph` command was used instead of
      `\paragraph*` for unnumbered level 4 headings.

  * HTML writer:

    + Fix revealjs slide structure regression with certain slide levels
      (#6030).
    + Add newlines to make slide show output more readable.

  * Org writer:

    + Remove extra spaces from table cells (Albert Krewinkel, #6024).

  * JATS template: Update JATS dtd (Arfon Smith, #6020).  Use the archiving
    and interchange DTD rather than the more restrictive journal publishing
    DTD (which doesn't permit ext-link as a valid child).

  * Text.Pandoc.PDF: Fix `runTeXProgram` so that the input source is always
    overwritten (#6027).  Previously it wasn't overridden if the file already
    existed, which led to bad results on subsequent runs when
    `pdf-engine-opt=-output-directory=` was used to specify an explicit temp
    dir.

  * Text.Pandoc.BCP47: Change `getLang` to handle block-level contents
    (#6008).  Some readers (e.g. RST) will populate the `lang` metadata field
    with block-level content.  `getLang` has been modified to handle this.
    Previously in these cases the LaTeX writer would not properly set the
    "main language" of the document.

  * Fix `test/tables.org` (Albert Krewinkel).

  * Use HTTPS in copyright message (Felix Yan, #6010)


## pandoc 2.9.1 (2019-12-23)

  * Add Jira reader (Albert Krewinkel, #5556).

  * Jira writer: use jira-wiki-markup renderer (Albert Krewinkel,
    #5926). The following improvements are included in this change:

    + non-jira raw blocks are fully discarded instead of showing
      as blank lines;
    + table cells can contain multiple blocks;
    + unnecessary blank lines are removed from the output;
    + markup chars within words are properly surrounded by
      braces;
    + preserving soft linebreaks via `--wrap=preserve` is
      supported.

    Note that backslashes are rendered as HTML entities, as there
    appears no alternative to produce a plain backslash if it is
    followed by markup. This may cause problems when used with
    confluence, where rendering seems to fail in this case.

  * Fix regression with `--number-sections`. Starting with 2.8,
    `--number-sections` also had the effect of `--section-divs`,
    even if `--section-divs` was not specified.

  * Improved table of contents generation in markdown, RTF,
    commonmark, better handling cases where section headings are
    enclosed in divs.

  * Ensure that later default file values for `variable` replace
    earlier ones (5988).

  * HTML reader: Add `nav` to list of block-level tags.

  * Org reader (Albert Krewinkel):

    + Wrap named table in Div, using name as id (#5984).
      Tables which are given a name via `#+NAME:` or `#+LABEL:`
      are wrapped in an additional Div, with the name set as the
      Div's ID.
    + Report parsing errors properly.
    + Fix parsing problem for colons in headline (#5993).

  * Text.Pandoc.PDF: Ensure UTF8 when printing source in
    `--verbose` mode, avoiding an error on platforms that
    default to something other than UTF-8 (#5997).

  * Text.Pandoc.Templates: Strip directory before trying to find
    partial in data files (#5987).

  * Text.Pandoc.Shared: Improve `makeSections` so we don't get
    doubled "number" attributes in EPUB output (or anywhere
    else) (#5986).

  * Added tests for `--toc` and `--section-divs`.

  * Text.Pandoc.MIME: Added glsl MIME type for WebGL maps (#6000,
    Jared Lander).

  * MANUAL: A bit clearer explanation for `--base-header-level`.
    We now say exactly how to translate between the deprecated
    `--base-header-level` and `--shift-heading-level-by`.

  * lua-filters.md:

    + Remove spurious dot in title (#5996, Mauro Bieg).
    + Replace metadata example with image centering (#6004,
      Albert Krewinkel). Thanks to @efx for proposing this filter.


## pandoc 2.9 (2019-12-11)

  * Text.Pandoc.Templates [API change]

    + Add Monad wrappers `WithDefaultPartials` and `WithPartials`.
      Wrapping these around an instance of `PandocMonad` gives
      us different instances of `TemplateMonad`, with different
      search behavior in retrieving partials.
      To compile a template and limit partial search to pandoc's
      data files, use `runWithDefaultPartials (compileTemplate ...)`.
      To compile a template and allow partials to be found locally
      (either on the file system or via HTTP, in the event that
      the main template has an absolute URL), ue
      `runWithPartials (compileTemplate ...)`.
    + Export `getTemplate`, which seeks a template locally,
      or via HTTP if the template has an absolute URL, falling
      back to the data files if not found.
    + Export `compileDefaultTemplate` -- does `getDefaultTemplate`
      and compiles the result, raising an error on failure.

  * Text.Pandoc.Class [API change]

    + Remove `TemplateMonad` instances for `PandocIO` and `PandocPure`.
      These were too limiting and caused a bug whereby a local
      partial could be used even when the default template was requested.
      We now rely on instances provided in the Templates module.

  * Text.Pandoc.App.OutputSettings: Simplify template retrieval code.

  * ConTeXt template: Adjust to title formatting (#5949, Denis Maier).
    Add `\setupinterlinespace` to `title`, `subtitle`, `date` and `author`
    elements:  otherwise longer titles that run over multiple lines will look
    squashed as `\tfd` etc. won't adapt the line spacing to the font size.

  * reveal.js template: Add title-slide-attributes variable (#5981,
    Frederik Elwert).

  * More informative JSON parse error (#5973).

  * Use external emojis package (forked from pandoc).  Removed emoji data
    in Text.Pandoc.Emoji.

  * Fix regression in `makeSections` (#5965).
    Previously `hierarchicalize` (the ancestor of `makeSections`) would put
    header attributes on the containing Div.  In 2.8 this behavior changed,
    which broke some tools depending on pandoc.  Here we roll back this change,
    so that attributes again migrate from the header to the containing Div when
    `makeSections` is run.  Note that attributes are retained on the header as
    well (unlike before) -- with the exception of the `id` attribute, which of
    course cannot be duplicated.

  * Fix `--toc-depth` regression in 2.8 (#5967).

  * Use doctemplates 0.8.  Rename template 'filters' as 'pipes'
    to avoid confusion with the other notion of filter used by pandoc.

  * Fix README.md so that relative links from manual become absolute.
    Previously they'd be broken links when viewed on GitHub or Hackage.
    So we add the base URL for the pandoc manual.

  * Document display math syntax in manual.

## pandoc 2.8.1 (2019-12-05)

  * Add `ascii_identifiers` as a supported extension for `markdown`.
    This fixes a regression in 2.8.

  * Fix regression with behavior of `--variable` (#5962).
    Previously `-Vfoo=1 -Vfoo=2` would produce a list value for foo;
    with 2.8 it produced just `2`.  This commit restores the earlier
    behavior.

  * Roll back part of of `--shift-heading-level-by` change (#5957).
    With positive heading shifts, starting in 2.8 this option
    caused metadata titles to be removed and changed to regular
    headings.  This behavior is incompatible with the old
    behavior of `--base-header-level` and breaks old workflows,
    so we have rolled back the change. Note that there is now an
    asymmetry in positive and negative heading level shifts:
    With positive shifts, the metadata title stays the same and
    does not get changed to a heading in the body, while with
    negative shifts, a heading can be converted into the
    metadata title.

  * Text.Pandoc.Shared: Fix `makeSections` so it doesn't turn
    column Divs into sections.

  * HTML writer: add task-list class to ul if all elements are
    task list items.  This will allow styling unordered task
    lists in a way that omits the bullet.

  * HTML-based templates: Add CSS to suppress bullet on unordered task lists.

  * ConTeXt template: Fix `\startcslreferences` and use ConTeXt syntax
    conventions (#5945, Denis Maier).  The old version had a too large
    a skip at the beginning of the reference list.

  * LaTeX template: keep the `\author{}` command even if author is not
    specified (#5961, Yihui Xie).  Avoids a LaTeX warning.

  * Generate Emoji module with TH.

    + Add Text.Pandoc.Emoji.TH.
    + Replace long literal list in Text.Pandoc.Emoji with one-liner
      generating it from `emoji.json` using TH.
    + Add Makefile target to download `emoji.json`.
    + Remove `tools/emoji.hs`.

  * Increase GC allocation space for compilation in cabal.project.

  * Clean up manual on PDF generation backend options (#5940).

  * Update release checklist to include code signing step and update
    Windows release-candidate builds (#5950).


## pandoc 2.8.0.1 (2019-11-26)

  * List `pdf` in `--list-output-formats`.
  * EPUB writer: Fix regression with `--css` (#5937).  In 2.8 `--css`
    would not have an effect on EPUB output.
  * RST writer: Use grid tables for one-column tables, since
    simple tables clash with heading syntax in this case (#5936).
  * Add unexported module Text.Pandoc.Readers.Metadata (see #5914).
  * Use doctemplates 0.7.2, which adds the `nowrap` filter to
    templates.
  * Update default man template using `nowrap` for .TH heading (#5929).
  * HTML templates: Add support for `toc-title` variable (#5930,
    Alexandre Franke).
  * Remove `grffile` (LaTeX package) requirement in MANUAL.txt
    (#5927, Ian Max Andolina).
  * Use skylighting 0.8.3.

## pandoc 2.8 (2019-11-22)

  * Improvements in templates system (from doctemplates):

    + Pandoc templates now support a number of new features that
      have been added in doctemplates: notably, `elseif`, `it`,
      partials, filters, and syntax to control nesting and reflowing of
      text.  These changes make pandoc more suitable out of the
      box for generating plain-text documents from data in YAML
      metadata.  It can create enumerated lists and even tabular
      structures.
    + We now used templates parameterized on doclayout Doc types.
      The main impact of this change is better reflowing of
      content interpolated into templates.  Previously,
      interpolated variables were rendered independently and
      interpolated as strings, which could lead to overly long
      lines. Now the templates interpolated as Doc values which
      may include breaking spaces, and reflowing occurs after
      template interpolation rather than before.
    + Remove code from the LaTeX, Docbook, and JATS writers that
      looked in the template for strings to determine whether it
      is a book or an article, or whether csquotes is used. This
      was always kludgy and unreliable.
    + Change template code to use new API for doctemplates.

  * Add `--defaults`/`-d` option.  This adds the ability to specify
    a collection of default values for options in a YAML file. For
    example, one might define a set of defaults for letters,
    and then do `pandoc -d letter myletter.md -o myletter.pdf`.
    See the documentation of this feature in MANUAL.txt.

  * Raise error on unsupported extensions (#4338).

  * The `--list-extensions[=FORMAT]` option now lists only
    extensions that affect the given FORMAT.

  * Add `-L` option as shortcut for `--lua-filter`.

  * Add `--shift-heading-level-by` option and deprecate
    `--base-heading-level` (#5615). The new option does
   everything the old one does, but also allows negative shifts.
   It also promotes the document metadata (if not null) to a
   level-1 heading with a +1 shift, and demotes an initial
   level-1 heading to document metadata with a -1 shift. This
   supports converting documents that use an initial level-1
   heading for the document title.

  * Allow `--metadata-file` to be used repeatedly to include
    multiple metadata files (Owen McGrath, #5702). Values in
    files specified first will be overridden by those in later
    files.

  * `--ascii` now uses numerical hex character references (#5718).

  * Allow PDF output to stdout (#5751).  PDF output now behaves like other
    binary formats: it will not be output to the terminal, but can be
    sent to stdout using either `-o -` or a pipe.  The intermediate format
    will be determined based on the setting of `--pdf-engine`.

  * Make some writers sensitive to 'unlisted' class on headings (#1762).
    If this is present on a heading with the 'unnumbered' class,
    the heading won't appear in the TOC.  This class has no
    effect if 'unnumbered' is not also specified.  This affects HTML-based
    writers (including slide shows and EPUB), LateX (including beamer), RTF,
    and PowerPoint.  Other writers do not yet support `unlisted`.

  * Fix `gfm_auto_identifiers` behavior with emojis (#5813).  Note that
    we also now use emoji names for emojis when `ascii_identifiers`
    is enabled.

  * When `--ipynb-output` is used with the default "best" format, strip
    ANSI escape codes for non-ipynb output (#5633).  These cause problems
    in many formats, including LaTeX.

  * Don't look for template files remotely for remote input (#5579).
    Previously pandoc would look for the template at a remote URL when a
    URL was used for the input file, instead of taking it from the
    data directory.

  * Allow combining `-Vheader-includes` and `--include-in-header` (#5904).
    Previously `header-includes` set as a variable would be
    clobbered by material included using `--include-in-header`.

  * Change merge behavior for metadata.  Previously, if a document
    contained two YAML metadata blocks that set the same field, the
    conflict would be resolved in favor of the first. Now it is resolved
    in favor of the second (due to a change in pandoc-types).
    This makes the behavior more uniform with other things in pandoc
    (such as reference links and `--metadata-file`).

  * Don't add a newline to fragment output if there's already one.

  * Change exit codes and document in MANUAL.txt:

    + `PandocAppError` was 1, is now 4
    + `PandocOptionError` was 2, is now 6
    + `PandocMakePDFError` was 65, is now 66

  * Switch to new pandoc-types and use Text instead of String [API change].
    (Christian Despres, #5884).

  * HTML reader:

    + Better handling of `<q>` with cite attribute (#5798, Ole Martin Ruud).
      If a `<q>` tag has a `cite` attribute, we interpret it as a Quoted
      element with an inner Span.
    + Add support for HTML `<samp>` element (#5792, Amogh Rathore).
      The `<samp>` element is parsed as Code with class `sample`.
    + Add support for HTML `<var>` element (#5799, Amogh Rathore).
      The `<var>` element is parsed as Code with class `variable`.
    + Add support for `<mark>` elements (Florian B, #5797).  Parse
      `<mark>` elements from HTML as Spans with class `mark`.
    + Add support for `<kbd>` elements, parsing them as Span with class
      `kbd` (Daniele D'Orazio, #5796).
    + Add support for `<dfn>`, parsing this as a Span with class `dfn`
      (#5882, Florian Beeres).

  * Markdown reader:

    + Headers: don't parse content over newline boundary (#5714).
    + Handle inline code more eagerly within lists (Brian Leung, #5627).
    + Removed some needless lookaheads.
    + Don't parse footnote body unless extension enabled.
    + Fix small super/subscript issue (#5878).  Superscripts and subscripts
      cannot contain spaces, but newlines were previously allowed
      (unintentionally).  This led to bad interactions in some cases
      with footnotes.  With this change newlines are also not allowed inside
      super/subscripts.
    + Use `take1WhileP` for `str`, table row.  This yields a small but
      measurable performance improvement.


  * LaTeX reader:

    + Fix parsing of optional arguments that contain braced text (#5740).
    + Don't try to parse includes if `raw_tex` is set (#5673).
      When the `raw_tex` extension is set, we just carry through
      `\usepackage`, `\input`, etc. verbatim as raw LaTeX.
    + Properly handle optional arguments for macros (#5682).
    + Fix `\\` in `\parbox` inside a table cell (#5711).
    + Improve `withRaw` so it can handle cases where the token string is
      modified by a parser (e.g. accent when it only takes part of a Word
      token) (#5686).  This fixes a bug that caused the ends of
      certain documents to be dropped.
    + Handle `\passthrough` macro used by latex writer (#5659).
    + Support tex `\tt` command (#5654).
    + Search for image with list of extensions like latex does, if an
      extension is not provided (#4933).
    + Handle `\looseness` command values better (#4439).
    + Add `mbox` and `hbox` handling (Vasily Alferov, #5586).
      When `+raw_tex` is enabled, these are passed through literally.
      Otherwise, they are handled in a way that emulates LaTeX's behavior.
    + Properly handle `\providecommand` and `\provideenvironment` (#5635).
      They are now ignored if the corresponding command or environment
      is already defined.
    + Support epigraph command in LaTeX Reader (oquechy, #3523).
    + Ensure that expanded macros in raw LaTeX  end with a space
      if the original did (#4442).
    + Treat `ly` environment from lilypond as verbatim (Urs Liska, #5671).
    + Add `tikzcd` to list of special environments (Eigil Rischel).
      This allows it to be processed by filters, in the same way that
      one can do for `tikzpicture`.

  * Roff reader:

    + Better support for `while`.
    + More improvements in parsing conditionals.
    + Fix problem parsing comments before macro.
    + Improve handling of groups.
    + Better parsing of groups (#5410).  We now allow groups
      where the closing `\\}` isn't at the beginning of a line.

  * RST reader:

    + Keep `name` property in `imgAttr` (Brian Leung, #5619).
    + Fixed parsing of indented blocks (#5753).  We were requiring
      consistent indentation, but this isn't required by RST.
    + Use title, not admonition-title, for admonition title.
      This puts RST reader into alignment with docbook reader.
    + Don't strip final underscore from absolute URI (#5763).
    + Avoid spurious warning when resolving links to internal anchors
      ending with `_` (#5763).

  * Org reader:

    + Accept `ATTR_LATEX` in block attributes (Albert Krewinkel, #5648).
      Attributes for LaTeX output are accepted as valid block attributes;
      however, their values are ignored.
    + Modify handling of example blocks (Brian Leung, #5717).
    + Allow the `-i` switch to ignore leading spaces (Brian Leung).
    + Handle awkwardly-aligned code blocks within lists (Brian Leung).
      Code blocks in Org lists must have their `#+BEGIN_` aligned in a
      reasonable way, but their other components can be
      positioned otherwise.
    + Fix parsing of empty comment lines (#5856, Albert Krewinkel).
      Comment lines in Org-mode can be completely empty.

  * Muse reader (Alexander Krotov):

    + Add RTL support (#5551).
    + Do not allow closing asterisks to be followed by `*`.
    + Do not split series of asterisks into symbols and emphasis (#5821).
    + Do not terminate emphasis on `*` not followed by space.

  * DokuWiki reader:

    + Parse markup inside monospace ('') (#5916, Alexander Krotov).

  * Docx reader:

    + Move style-parsing-specific code to a new unexported module,
      Text.Pandoc.Readers.Docx.Parse.Styles (Nikolay Yakimov).
    + Move StyleMap to docx writer (Nikolay Yakimov).
    + Only use LTR when it is overriding BiDi setting (#5723, Jesse
      Rosenthal).  The left-to-right direction setting in docx is used
      in the spec only for overriding an explicit right-to-left setting.
      We only process it when it happens in a paragraph set with BiDi.
      This is especially important for docs exported from Google Docs,
      which explicitly (and unnecessarily) set `rtl=0` for every paragraph.
    + Fix list number resumption for sublists (#4324).
      The first list item of a sublist should not resume numbering
      from the number of the last sublist item of the same level,
      if that sublist was a sublist of a different list item.

  * Docbook reader:

    + Richer parse for admonitions (Michael Peyton Jones, #1234).
      Instead of parsing admonitions as blockquotes, we now parse
      them as Divs with an appropriate class. We also handle titles
      for admonitions as a nested Div with the "title" class.
    + Fix nesting of chapters and sections (#5864, Florian Klink,
      Félix Baylac-Jacqué).
    + Fix bug with entities in mathphrase element (#5885).

  * MediaWiki reader:

    + Skip optional `{{table}}` template (#5757).

  * LaTeX reader:

    + Fix dollar-math parsing to ensure that space is left between a
      control sequence and a following letter (#5836).
    + In `untokenize`, ensure space between control sequence
      and following letter (#5836).
    + Don't omit macro definitions defined in the preamble.
      These were formerly omitted (though they still affected macro
      resolution if `latex_macros` was set).  Now they are included in
      the document body.
    + Parse macro definitions as raw LaTeX when `latex_macros` is
      disabled.  (When `latex_macros` is enabled, we omit them, since
      pandoc is applying the macros itself.)
    + Fix a hang/memory leak in certain circumstances (#5845).
    + Text.Pandoc.Readers.LaTeX.Parsing: add `[Tok]` parameter to
      `rawLaTeXParser`.  This allows us to repeat retokenizing
      unnecessarily in e.g. `rawLaTeXBlock`.
    + Add KOMA-Script metadata commands (#5910, Andrew Dunning).
      Add all titling commands to existing definition for `\dedication`.
    + Parse `\micro` siunitx unit command (#5921, Jose Luis Duran).

  * Markdown writer:

    + Ensure proper nesting when we have long ordered list markers (#5705).
    + Make `plain` output plainer (#5741).  Previously we used the following
      Project Gutenberg conventions for plain output: extra space before and
      after level 1 and 2 headings, all-caps for strong emphasis,
      underscores surrounding regular emphasis. Now these
      conventions are used only when the `gutenberg` extension is
      enabled. By default, Strong and Emph are rendered without
      special formatting, and headings are rendered without
      special formatting, and with only one blank line following.
      To restore the former behavior, use `-t plain+gutenberg`.
    + Prefer using raw_attribute when enabled (#4311).
      The `raw_attribute` will be used to mark raw bits, even HTML
      and LaTeX, and even when `raw_html` and `raw_tex` are
      enabled, as they are by default. To get the old behavior,
      disable `raw_attribute` in the writer.
    + Prefer `pipe_tables` to raw HTML even when we must
      lose width information (#2608, #4497).
    + Improve escaping in list items (#5918).

  * AsciiDoc writer:

    + Don't include `+` in code blocks for regular asciidoc.
      This is asciidoctor-specific.
    + Handle admonitions (#5690).

  * LaTeX writer:

    + Add thin space when needed in LaTeX quote ligatures (#5684).
    + Use `\hspace{0pt}` for 0-width space U+200B (#5756).
    + Use `cslreferences` environment for csl bibliographies.
      This allows bibliographies to receive special formatting.
      The template now contains definition of this environment (enabled
      only when CSL is used).  It also defines a `\cslhangindent` length.
      This is set to 2em by default when the bibliography style specifies
      a hanging indent.  To override the length, you can use e.g.
      `\setlength{\cslhangindent}{7em}` in header-includes.
      See jgm/pandoc-citeproc#410.
    + Strip off `{}` around locator for biblatex/natbib output (#5722).
    + Fix line breaks at start of paragraph (#3324).
      Previously we just omitted these. Now we render them
      using `\hfill\break` instead of `\\`.  This is a revision
      of a PR by @sabine (#5591) who should be credited with the idea.
    + We no longer look in the template or header-includes to see if a
      book or article documentclass is used, or to see whether the
      `csquotes` package is used. To use `csquotes` for LaTeX, set
      `csquotes` in your variables or metadata. To specify a book
      style, use the `documentclass` variable or
      `--top-level-division`.
    + Fix horizontal rule (#5801).  We change to use 0.5pt rather than
      `\linethickness`, which apparently only ever worked "by accident"
      and no longer works with recent updates to texlive.

  * ConTeXt writer:

    + Add option to include source files in ConTeXt PDFs (Tristan
      Stenner, #5578). The metadata field or variable
      (`includesource`) can be set to attach the source documents
      to the resulting PDF.
    + Customizable type of PDF/A for the ConTeXt writer (Karl
      Pettersson, #5608).  The `pdfa` variable may now be set in metadata.
      Also updated color profile settings in accordance with ConTeXt wiki,
      and made ICC profile and output intent for PDF/A customizable
      using `pdfaiccprofile` and `pdfaintent`.
    + Unit tests: adjust code property to avoid an irrelevant
      failure involving inline code with two consecutive newlines.
    + Set `csl-hanging-indent` variable if needed.
    + Use special environment for CSL references.
    + Use braces, not start/stop, for inline language tags.
      This prevents unwanted gobbling of spaces.

  * HTML writer:

    + Use numeric character references with `--ascii` (#5718).
      Previously we used named character references with html5 output.
      But these aren't valid XML, and we aim to produce html5 that is
      also valid XHTML (polyglot markup).  (This is also needed for epub3.)
    + Ensure that line numbers in code blocks get id-prefix (#5650).
    + Ensure TeX formulas are rendered correctly (Philip Pesca,
      #5658). The web service passed in to `--webtex` may render
      formulas using inline or display style by default.
      Prefixing formulas with the appropriate command ensures
      they are rendered correctly.
    + Render inline formulas correctly with `--webtex` (Philip
      Pesca, #5655). We add `\textstyle` to the beginning of the
      formula to ensure it will be rendered in inline style.
    + Pass through `aria-` attributes to HTML5 (#5642).
    + Render a Quoted element with an inner Span with
      `cite` attribute using a `<q>` tag (#5798, Ole Martin Ruud).
    + Render a Span with class `mark` using the `<mark>` element
      (Florian Beeres, #5797).
    + Render Span with class `dfn` using `<dfn>` element
      (Florian Beeres, #5882).
    + Render Span with class `kbd` using `<kbd>` element (Daniele
      D'Orazio, #5796).
    + Render Code with class `variable` using `<var>` element
      (Amogh Rathore, #5799).
    + Render Code with class `sample` using `<samp>` element
      (Amogh Rathore, #5799).

  * EPUB writer:

    + Improve splitting into chapters (#5761), using `makeSection`.
    + Avoid issuing warning multiple times when title not set (see #5760).
    + Use svg tag wrapper for cover image (#5638).  In addition, the
      code generating the image has been moved to the template, to make it
      more customizable. NOTE: Those who use custom EPUB
      templates will need to adjust their templates, adding the
      code to generate the cover image. (Previously this was just
      inserted into 'body'.)
    + Improve toChapters, making it work better if there are Divs
      around sections.
    + Add support for EPUB2 covers (blmage, #3992).
    + Do not override existing "fileN" medias when writing to EPUB format
      (blmage, #4206).
    + Ensure that `lang` variable is set on all chapters (so that it
      will add an `xml:lang` attribute on the `html` element).

  * RST writer:

    + Removed remnants of `admonition-title`.
    + Fix handling of `:align:` on figures and images (#4420).
      When the image has the `align-right` (etc.) class, we now use
      an `:align:` attribute.
    + Improve spacing for tables with no width information (#5899).
      If a simple table would be too wide, we use a grid table.
    + Fix backslash escaping after strings (Albert Krewinkel, #5906).
      The check whether a complex inline element following a string must
      be escaped, now depends on the last character of the string instead
      of the first.
    + Ensure there's a blank line before tables (#5898).

  * Dokuwiki writer:

    + Handle mixed lists without HTML fallback (#5107).

  * XWiki writer:

    + Fix multiline table (Zihang Chen, #5683).

  * Muse writer:

    + Add RTL support (Alexander Krotov, #5551).

  * Man writer:

    + Suppress non-absolute link URLs (#5770).  Absolute URLs are
      still printed in parentheses following the link text, but relative
      URLs are suppressed (just as internal links starting with '#'
      always have been).
    + Improved definition list term output.  Now we boldface code but
      not other things. This matches the most common style in man pages
      (particularly option lists).

  * Ms writer:

    + Use `.LP` instead of `.PP` for line block (#5588).
    + Use boldface for definition terms in DefinitionLists.

  * JATS writer:

    + Do not emit empty `<fn-group>` (Mauro Bieg, #5595).
    + Update template to v1.1dtd (#5632, Arfon Smith).
    + Update `data/jats.csl` to avoid commas between editor
      name-part elements. (#5629)
    + Add `abstract` to template (Mauro Bieg).

  * TEI writer:

    + Don't strip hash from internal links (#5922).

  * Jira writer:

    + Remove escapeStringForJira for code blocks (Jan-Otto Kröpke).
    + Remove extraneous newline after single-line block quotes
      (#5858, Albert Krewinkel).
    + Improve escaping of special characters, using backslash escapes
      instead of HTML entities (#5858, Albert Krewinkel).

  * OpenDocument writer:

    + Avoid duplicate attributes (#4634). We use the innermost
      attribute in nested cases.
    + If `native_numbering` extension is set, use native OpenDocument
      enumeration for figures and tables (Nils Carlson).
    + Place caption before table (#5681, Dmitry Pogodin).

  * ODT writer:

    + Add a test for MathML formulas in ODT documents (blmage).
    + Improve the parsing of frames in ODT documents (blmage).

  * Docx writer:

    + Make handling of styles more robust in localized versions
      of Word (Nikolay Yakimov, #5523, #5052, #5074).  We now use
      style names, not ids, for assigning semantic meaning, since
      the ids can change depending on the locale. Style name
      comparisons are case-insensitive, since those are
      case-insensitive in Word. Since docx style names can have
      spaces in them, and pandoc-markdown classes can't, anywhere
      when style name is used as a class name, spaces are
      replaced with ASCII dashes `-`. Code styles, i.e. "Source
      Code" and "Verbatim Char" now honor style inheritance. Docx
      Reader now honours "Compact" style (used in
      Pandoc-generated docx). The side-effect is that "Compact"
      style no longer shows up in docx+styles output. Styles
      inherited from "Compact" will still show up.
    + Re-use Readers.Docx.Parse for StyleMap (#5766, Nikolay Yakimov).
    + Internal improvements and code simplification (Nikolay Yakimov).
    + Preserve built-in styles in DOCX with custom style (Ben Steinberg,
      #5670).  This change prevents custom styles on divs and spans
      from overriding styles on certain elements inside them, like
      headings, blockquotes, and links. On those elements, the
      "native" style is required for the element to display correctly.
      This change also allows nesting of custom styles; in order to do so,
      it removes the default "Compact" style applied to Plain blocks,
      except when inside a table.
    + Add `proofState` to list of elements carried over from
      settings.xml in the reference.docx (Krystof Beuermann, #5703).
    + Change order of `ilvl` and `numId` in `document.xml` (Agustín
      Martín Barbero, #5645). Also, make list para properties go first.
      This reordering of properties shouldn't be necessary but
      it seems Word Online does not understand the docx correctly otherwise.

  * PowerPoint writer:

    + Code formatting is now context dependent (Jeroen de Haas, #5573).
      This commit alters the way in which the Powerpoint writer treats
      inline code and code blocks. Inline code is now formatted at
      the same size as the surrounding text. Code blocks are now given
      a margin and font size according to their level.
      Furthermore this commit allows changing the font with which code is
      formatted via the `monofont` option.
    + Start numbering at appropriate numbers (Jesse Rosenthal, #5709).
      Starting numbers for ordered lists were previously ignored. Now
      we specify the number if it is something other than 1.

  * Text.Pandoc.Parsing:

    + Add `manyChar`, `many1Char`, `manyTillChar`, `many1TillChar`,
      `many1Till`, `manyUntil`, `mantyUntilChar`: these are like their
      unsuffixed counterparts but pack some or all of their output
      (Christian Despres, #5884).
    + Add `stateAllowLineBreaks` to `ParserState` [API change].
    + Fix inline parsing in grid table cells (#5708).
    + Change type of `setLastStrPos` so it takes a `Maybe SourcePos`
      rather than a `SourcePos` [API change].
    + Make `parseFromString'` and `gridTableWith` and
      `gridTableWith'` polymorphic in the parser state,
      constraining it with `HasLastStrPosition` [API change].
    + `parseFromString'`: reset `stateLastStrPos` to `Nothing` before parse.
    + Rename takeWhileP -> take1WhileP and clean it up.
      (It doesn't match the empty sequence.)

  * Text.Pandoc.PDF:

    + For PDFs via HTML, ensure temp file is deleted even if the pdf
      program is not found (#5720).
    + Better detection of a Cygwin environment (#5451).
    + Don't assume tex log file is UTF8-encoded (#5872).
      Fall back to latin1 if it can't be read as UTF-8.

  * Text.Pandoc.Extensions:

    + Export new function `getAllExtensions`, which returns the
      extensions that affect a given format (whether enabled by default
      or not) [API change].
    + Change type of `parseFormatSpec` from
      `Either ParseError (String, Extensions -> Extensions)`
      to `Either ParseError (String, [Extension], [Extension])`
      [API change].
    + Add `Ext_gutenberg` constructor to `Extension` [API change].
    + Add `Ext_native_numbering` constructor to `Extension` [API change]
      (Nils Carlson).

  * Text.Pandoc.Readers, Text.Pandoc.Writers:

    + Change type of `getReader` and `getWriter` so they return
      a value in the PandocMonad instance rather than an Either
      [API change].  Exceptions for unknown formats and unsupported
      extensions are now raised by these functions.

  * Text.Pandoc.App

    + Change `optMetadataFile` type from `Maybe FilePath` to `[FilePath]`
      (Owen McGrath, #5702) [API change].

  * Text.Pandoc.Logging:

    + Add `CouldNotDeduceFormat` constructor to `LogMessage` [API change].
      Issue this warning when we're falling back to markdown or
      html because we don't recognize the extension of the input or
      output files.
    + Clarify warning for missing title (#5760).
    + Add `UnusualConversion` constructor to `LogMessage` [API change]
      (Mauro Bieg, #5736).  Emit warning on `-f latex -o out.pdf`.

  * Lua filters:

    + Improve function documentation (Albert Krewkinkel).
    + Traverse nested blocks and inlines in correct order (Albert
      Krewinkel, #5667). Traversal methods are updated to use the new
      Walk module so that sequences with nested Inline (or Block) elements
      are traversed in the order in which they appear in the linearized
      document.
    + New unexported module `Text.Pandoc.Lua.Walk` (Albert Krewinkel).
      Lua filters must be able to traverse sequences of AST
      elements and to replace elements by splicing sequences back
      in their place. Special `Walkable` instances can be used for
      this; those are provided in a new module
      `Text.Pandoc.Lua.Walk`.
    + `Attr` values can now be given as normal Lua tables (Albert
      Krewinkel, #5744).  This can be used as a convenient alternative
      to constructing `Attr` values with `pandoc.Attr`. Identifiers
      are taken from the `id` field, classes must be given as
      space separated words in the `class` field. All remaining fields
      are included as attributes. With this change, the following lines
      now create equal elements:
      ```
      pandoc.Span('test', {id = 'test', class = 'a b', check = 1})
      pandoc.Span('test', pandoc.Attr('test', {'a','b'}, {check = 1}))
      ```
      This also works when using the *attr* setter:
      ```
      local span = pandoc.Span 'text'
      span.attr = {id = 'test', class = 'a b', check = 1}
      ```
      Furthermore, the *attributes* field of AST elements can now be a
      plain key-value table even when using the `attributes` accessor:
      ```
      local span = pandoc.Span 'test'
      span.attributes = {check = 1}   -- works as expected now
      ```
    + Export `make_sections`, remove `hierarchicalize`. Lua filters that
      use `hierarchicalize` will need to be rewritten to use
      `make_sections`.
    + Add a `clone()` method to all AST elements (Albert Krewinkel, #5568).
    + Fix Lua function names in pandoc.system (niszet). Change
      `get_current_directory` to `get_working_directory` and
      `with_temp_directory` to `with_temporary_directory`, to
      conform to the manual.

  * Text.Pandoc.Error:

    + Add constructors `PandocUnknownReaderError`,
      `PandocUnknownWriterError`, `PandocUnsupportedExtensionError`.
      [API change].
    + Better message for `PandocShouldNeverHappenError`.
    + Better message for `PandocTemplateError`.

  * Text.Pandoc.Emoji:

    + Update emoji list (#5666). Done using new `tools/emojis.hs`,
     which uses the list from the gem GitHub uses. Future updates
     can be done with this tool.

  * Text.Pandoc.PDF:

    + Pass value of `--dpi` to `rsvg-convert` when converting SVG to PDF
      in the process of creating a PDF (#5721).

  * Text.Pandoc.Shared:

    + Replace `Element` and `makeHierarchical` with `makeSections`.
      Now that we have Divs, we can use them to represent the
      structure of sections, and we don't need a special Element type.
      `makeSections` reorganizes a block list, adding Divs with class
      `section` around sections, and adding numbering if needed.
      This change also fixes some longstanding issues recognizing section
      structure when the document contains Divs (#3057, see also #997).
    + Remove `Element` type [API change]
    + Remove `makeHierarchicalize` [API change]
    + Add `makeSections` [API change]
    + Export `deLink` [API change]
    + Make `filterIpynbOutput` strip ANSI escapes from code in output
      for non-ipynb formats, when the default "best" option is used with
      `--ipynb-output` (#5633).
    + Fix `camelCaseToHyphenated` so it handles `ABCDef` better.
    + Improve `isTight` (#5857).  If a list has an empty item,
      this should not count against its being a tight list.
    + Export `htmlSpanLikeElements` [API change] (Daniele D'Orazio, #5796).
      This is a mapping of HTML span-like elements that are internally
      represented as a Span with a single class.
    + Change the implementation of `htmlSpanLikeElements` to retain
      classes and attributes (#5882, Florian Beeres).

  * Text.Pandoc.Slides: recognize content in Divs when determining
    slide level.

  * Text.Pandoc.SelfContained:

    + Omit content-type on type attribute for `<style>` (#5725).
      It doesn't seem to be valid for HTML5, and as a result Chrome
      ignores the style element.

  * Text.Pandoc.Pretty has been removed [API change].
    We now use the new external doclayout module instead.

  * Text.Pandoc.Writers.Shared:

    + Remove `metaToJSON`, `metaToJSON'` [API change].
    + Modify `addVariablesToContext`, `defField`, `setField`, `getField`,
     `resetField` to work with Context rather than JSON values. [API change]
    + Export new function `endsWithPlain` [API change].
    + Change `gridTables` so it does better at keeping the widths of
      columns (#4320) and does better at figuring out column widths
      when no widths are given (#5899).

  * Text.Pandoc.Options

    + Change type of `writerTemplate` to `Maybe Template` instead
      of `Maybe String`.
    + Change To/FromJSON instances for `HTMLMathMethod`, `CiteMethod`,
      `ObfuscationMethod`, `TrackChanges`, `WrapOption`,
      `TopLevelDivision`, `ReferenceLocation`, `HTMLSlideVariant` (#5790).
      In each case we use lowercase (or hyphenated lowercase) for
      constructors to line up more closely with command-line option
      values.  This is a breaking change for those who manually decode or
      encode JSON for these data types (e.g. for `ReaderOptions` or
      `WriterOptions`).

  * Text.Pandoc.Filters:

    + Add `FromYAML` instance for `Filter`.
    + `applyFilters`: Add and apply filters in order (not reversed)
      This changes `applyFilters` from Text.Pandoc.Filter so
      that it does a left fold rather than a right fold, applying
      the filters in the order listed.

  * Text.Pandoc.XML:

    + Change `toEntities` to emit numerical hex character references
      (#5718).

  * Text.Pandoc.Highlighting:

    + Add additional listings languages (Wandmalfarbe).

  * Text.Pandoc.MediaBag:

    + Some of the types using Strings were switched to use FilePath instead
      (Christian Despres, #5884).

  * Text.Pandoc.Templates:

    + Don't import/export `varListToJSON` [API change].  It is removed in
      doctemplates >= 0.3.
    + Change type of `renderTemplate'` [API change].  Return value is
      now Text rather than being polymorphic.  This makes room for
      upcoming removal of the `TemplateTarget` class from doctemplates.

  * Text.Pandoc.App.Opt [API changes]:

    + More convenient To/FromJSON instances.  Make the field names
      like `strip-empty-paragraphs` rather than `optStripEmptyParagraphs`.
      Anyone who is using JSON serialization of Opt will need to adjust
      things accordingly.
    + Change `optHighlightStyle` to a `Maybe String` instead of
      `Maybe Style`.  Do the parsing/loading of themes later, after
      option parsing.
    + Remove `optBaseHeaderLevel` from `Opt`.  We now just use
      `optShiftHeadingLevelBy`, to avoid redundancy.
    + Change `optShiftHeadingLevel` to `optShiftHeadingLevelBy` to
      match the option.
    + Custom FromJSON instance for `LineEnding`, so either `CRLF`
      or `crlf` will work.
    + Change `optVariables` from `[(String, String)]` to `Context Text`.
    + Change `optMetadata` to `Meta`, to allow structured values.
      The current behavior of the `--metadata` option stays the same.
    + Rename `optReader`, `optWriter` as `optFrom`, `optTo`.
    + Add `FromYAML` instances to `Opt` and to all subsidiary types.
    + Rename `optMetadataFile` to `optMetadataFiles`.
    + Rename `optPDFEngineArgs` to `optPDFEngineOpts`.
    + Rename `optWrapText` to `optWrap`.
    + Add `IpynbOutput` enumerated type: use this instead of
      a string for `optIpynbOutput`.
    + Change optInputFiles to a `Maybe [FilePath]` (#5888) [API change].
      `Nothing` means: nothing specified.
      `Just []` means: an empty list specified (e.g. in defaults).
    + List fields in Opt so they aren't reversed (#5881) [API change].
      Previously `optIncludeInHeader`, etc. were in reverse order.
    + The `sourcefile` variable is now always a list. It used to be
      sometimes a string, sometimes a list (when there was more than one).

  * Template changes:

    + default.latex: added code for `cslreferences` environment,
      to be used for pandoc-citeproc references. A `csl-hanging-indent`
      variable (set automatically if there is a `hanging-ident`
      class on the references Div) controls whether contents of this
      environment receive a hanging indent.
    + default.latex: Add `space` as default option for xeCJK, so that
      spaces between words are preserved (#5855, jeongminkim-islab).
      This is necessary for Korean.
    + default.latex: Remove include of `grffile` (#5848).
      This package used to be needed for proper handling of image filenames
      containing periods (in addition to the period before the extension).
      It no longer works with the latest LaTeX kernel and graphicx,
      so we have removed it. Future versions of graphicx will handle
      these filenames without the need for `grffile`.
    + default.context: add a saner default for page numbers.
      Previously they appeared centered at the top of the page;
      now we put them centered at the bottom, unless the `pagenumbering`
      variable is set.
    + default.context: define a start-stop-pair `cslreferences` to
      allow for hanging indents in the bibliography (#5875, Denis Maier).
    + default.ms: update defaults.  Use Palatino font, use slightly
      wider interparagraph space, don't indent paragraphs,
      and put page numbers on the bottom.  This brings ms output
      closer to default LaTeX output.
    + default.revealjs: add navigationMode (Mauro Bieg, #5657).
    + default.muse: handle multiple authors better.
    + docbook4, docbook5 templates: add indentation to body.
    + HTML-based templates: use `styles.html` partial to avoid
      code duplication.
    + HTML-based templates: change indentation of styles in template.

  * reference.docx (#5820):

    + Change Block Text (block quote) style so that the same font
      is used as in the body text, and the block text is indented
      left and right.
    + All headings now have a uniform color.
    + Level-1 headings no longer set `w:themeShade="B5"`.
    + Level-2 headings are now 14 point rather than 16 point.
    + Level-3 headings are now 12 point rather than 14 point.
    + Level-4 headings are italic rather than bold.

  * epub.css: Add CSS for hanging-indent div to support pandoc-citeproc's
   new hanging indents.

  * pandoc.cabal:

    + Repeat ghc-options in all stanzas.
    + Remove conditionals for ghc < 8 (Albert Krewinkel, #5693).
    + Compile with `-Wcpp-undef` and `-fhide-source-paths` when possible
      (Albert Krewinkel).
    + Add cabal.project to extra-source-files (Albert Krewinkel).
    + Add dependency on skylighting-core (#5729). Even though it shouldn't
      be needed, some tools require it.
    + Require latest pandoc-types, texmath, skylighting, haddock-library.
    + Ensure TemplateHaskell is added to other-extensions when it is
      used (Vanessa McHale, #5728).
    + Remove `derive_json_via_th` flag; always use TH.  This cuts down
      on code duplication and reduces the chance for errors.  See #4083.

  * Makefile:

    + Add ghci target.
    + Add ghcid target.
    + Remove references to obsolete flag (#5694).

  * Benchmarks: fix failure on ipynb.

  * Use MathJax 3 (zorkow).

  * KaTeX math: respect `classoption=fleqn` variable,
    bump KaTeX version to 0.11.1 (#5815, Mauro Bieg).

  * Fix redundant constraint compiler warnings (Pete Ryland, #5625).

  * Use throwError instead of fail when appropriate.

  * Use Prelude.fail to avoid ambiguity with fail from GHC.Base.

  * Add `diff-zip.sh` to tools (John MacFarlane, Agustín Martín Barbero).
    This is intended to make it easier to test differences in zip
    containers, like epub, docx, or odt.

  * Add `.gitattributes` (#5747).  This ensures that the golden files
    in `test/fb2/reader/` don't have newlines converted. This should fix
    a test failure on GitHub CI with Windows.

  * Reorder options in `--help`.

  * Revise code for HsYAML-0.2.0.0 (@vijayphoenix, #5704).

  * Remove blank line in code example in Haddocks (Leif Metcalf, #5679).

  * Fix trypandoc with `getReader`/`getWriter` changes.

  * Allow building pandoc with GHC 8.8.

  * linux tarball: add architecture `-amd64` to filename.
    Now it will be: `pandoc-VERSION-linux-ARCH.tar.gz`.

  * MANUAL.txt:

    + Add section for exit codes.
    + Document some pptx limitations in slide show section:
      No incremental display (#5689).  No pause with `. . .` (#5701).
    + reveal.js flags (Mauro Bieg, #5653).
    + Document addition of `data-` prefix to unknown attributes in HTML5.
    + Link to YAML spec (Mauro Bieg, #5687).
    + Fix capitalization of "Linux" (#5859, Marcus Stollsteimer).
    + Use a table for exit codes.
    + Put all template variable docs into one section.
    + Use ATX headers consistently.
    + Add fuller documentation of templates (#5779), including
      new template syntax, partials, etc.
    + Add documentation for the variable `hyperrefoptions` (Wandmalfarbe).
    + Clarify when macro definitions are passed as raw latex.
      In Markdown input, they are always passed through.  In LaTeX, only if
      `latex_macros` is disabled.
    + Clarify that `--dpi` provides a default and doesn't override
      dpi values specified in the images themselves (#5721).
    + Document how to use custom writers with `--standalone` (#5866).
    + Clarify `--preserve-tabs` default.

  * INSTALL.md:

    + Fix instructions for libicu.
    + Add Void Linux instructions (Volodymyr Kozachnyskyi).

  * CONTRIBUTING.md:

    + Add information on tests (Agustín Martín Barbero, #5652).
    + Add information about command test naming to CONTRIBUTING (Florian B).

  * Fix typos in changelog and comments (#5896, Brian Wignall).

  * doc/lua-filters.md:

    + Fix mistakes in mediabag module docs (#5851, Albert Krewinkel).
    + Improve metadata replacement example in lua-filters doc (#5851).
    + Mention which Lua version is shipped with pandoc (Albert Krewinkel,
      #5892).

## pandoc 2.7.3 (2019-06-11)

  * Add `jira` (Atlassian's Jira wiki markup) as output format (#2497,
    Albert Krewinkel).

  * Add `tex_math_dollars` to `multimarkdownExtensions` (#5512).
    This form is now supported in multimarkdown,
    in addition to `tex_math_double_backslash`.

  * Fix `--self-contained` so it works when output format has extensions.
    Previously if you used `--self-contained` with `html-smart` or
    `html+smart`, it wouldn't work.

  * Add template variable `curdir` with working directory
    from which pandoc is run (#5464).

  * Markdown reader: don't create implicit reference for empty header
    (#5549).

  * Muse reader: allow images inside link descriptions (Alexander Krotov).

  * HTML reader: epub related fixes.

    + With epub extensions, check for `epub:type` in addition to `type`.
    + Fix problem with noteref parsing which caused block-level
      content to be eaten with the noteref.
    + Rename `pAnyTag` to `pAny`.
    + Refactor note resolution.
    + Trim definition list terms (Alexander Krotov).

  * LaTeX reader:

    + Add braces when resolving `\DeclareMathOperator`
      (#5441).  These seem to be needed for xelatex but not pdflatex.
    + Allow newlines in `\mintinline`.
    + Pass through unknown listings language as class (#5540).
      Previously if the language was not in the list of languages supported
      by listings, it would not be added as a class, so highlighting
      would not be triggered.
    + `rawLaTeXInline`: Include trailing `{}`s in raw latex commands (#5439).
      This change affects the markdown reader and other readers that allow raw
      LaTeX.  Previously, trailing `{}` would be included for unknown
      commands, but not for known commands.  However, they are sometimes used
      to avoid a trailing space after the command.  The chances that a `{}`
      after a LaTeX command is not part of the command are very small.

  * MediaWiki reader: handle multiple attributes in table row (#5471,
    chinapedia).

  * Docx reader: Add support for `w:rtl` (#5545).  Elements with this
    property are put into Span inlines with `dir="rtl"`.

  * DocBook reader: Issue `IgnoredElement` warnings.

  * Org reader (Albert Krewinkel):

    + Fix planning elements in headers level 3 and higher
      (#5494).  Planning info is now always placed before
      the subtree contents.  Previously, the planning info was placed after
      the content if the header's subtree was converted to a list, which
      happens with headers of level 3 and higher per default.
    + Omit, but warn about unknown export options.  Unknown export
      options are properly ignored and omitted from the output.
    + Prefer plain symbols over math symbols (#5483).
      Symbols like `\alpha` are output plain and unemphasized, not as math.
    + Recognize emphasis after TODO/DONE keyword (#5484).

  * FB2 reader:

    + Skip unknown elements rather than throwing errors (#5560).
      Sometimes custom elements are used (e.g. `id` element
      inside `author`); previously the reader would halt with an error.
      Now it skips the element and issues an `IgnoredElement` warning.
    + Parse notes (#5493, Alexander Krotov).
    + Internal improvements (Alexander Krotov).

  * OpenDocument writer:  Roll back automatic figure/table numbering
    (#5474).  This was added in pandoc 2.7.2, but it makes it impossible
    to use pandoc-crossref. So this has been rolled back for now,
    until we find a good solution to make this behavior optional
    (or a creative way to let pandoc-crossref and this feature
    to coexist).

  * New module Text.Pandoc.Writers.Jira, exporting `writeJira` [API
    change] (Albert Krewinkel).

  * EPUB writer:

    + Don't include 'landmarks' if there aren't any.
      Previously we could get an empty ol element, which caused
      validation errors with epubcheck.
    + Ensure unique ids for styleesheets in content.opf (#5463).
    + Make stylesheet link compatible with kindlegen (#5466,
      Eric Schrijver).  Pandoc omitted `type="text/css"` from both
      `<style>` and `<rel="stylesheet">` elements in all templates, which
      is valid according to the spec. However, Amazon’s kindlegen software
      relies on this attribute on `<link>` elements when detecting
      stylesheets to include.

  * HTML writer:

    + Output video and audio elements depending on file
      extension of the image path (Mauro Bieg).
    + Emit empty alt tag in figures (#5518, Mauro Bieg).
      The same text is already in the <figcaption> and
      screen-readers would read it twice, see #4737.
    + Don't add variation selector if it's already there.
      This fixes round-trip failures.
    + Prevent gratuitous emojification on iOS (#5469).
      iOS chooses to render a number of Unicode entities, including '↩', as
      big colorful emoji.  This can be defeated by appending Unicode
      VARIATION SELECTOR-15'/'VARIATION SELECTOR-16'.  So we now append this
      character when escaping strings, for both '↩' and '↔'.  If other
      characters prove problematic, they can simply be added to
      `needsVariationSelector`.  + Add `class="heading"` to level 7+ Headers
      rendered as `<p>` elements (#5457).

  * RST writer: treat Span with no attributes as transparent (#5446).
    Previously an Emph inside a Span was being treated as
    nested markup and ignored.  With this patch, the Span
    is just ignored.

  * LaTeX writer:

    + Include inline code attributes with `--listings` (#5420).
    + Don't produce columns environment unless beamer (#5485).
    + Fix footnote in image caption.  Regression: the fix for #4683 broke
      this case.
    + Don't highlight code in headings (#5574).  This causes
      compilation errors.
    + Use `\mbox` to get proper behavior inside `\sout` (#5529).

  * EPUB writer: Fix document section assignments (#5546).
    For example, introduction should go in bodymatter, not frontmatter, and
    epigraph, conclusion, and afterward should go in bodymatter, not
    backmatter.  For the full list of assignments, see the manual.

  * Markdown writer:

    + Add backslashes to avoid unwanted interpretation of
      definition list terms as other kinds of block (#554).
    + Ensure the code fence is long enough (#5519).
      Previously too few backticks were used when the code block
      contained an indented line of backticks.  (Ditto tildes.)
    + Handle labels with integer names (Jesse Rosenthal, #5495).
      Previously if labels had integer names, it could produce a conflict
      with auto-labeled reference links. Now we test for a conflict and
      find the next available integer.  This involves adding a new state
      variable `stPrevRefs` to keep track of refs used in other document
      parts when using `--reference-location=block|section`

  * Textile writer: fix closing tag for math output (Albert Krewinkel).
    Opening and closing tag for math output match now.

  * Org writer: always indent src blocks content by 2 spaces (#5440, Albert
    Krewinkel).  Emacs always uses two spaces when indenting the content of
    src blocks, e.g., when exiting a `C-c '` edit-buffer. Pandoc used to
    indent contents by the space-equivalent of one tab, but now always uses
    two spaces, too.

  * Asciidoc writer:

    + Use `` `+...+` `` form for inline code.
      The old `` `a__b__c` `` yields emphasis inside code in asciidoc.
      To get a pure literal code span, use `` `+a__b__c+` ``.
    + Use proper smart quotes with asciidoctor (#5487).
      Asciidoctor has a different format for smart quotes.
    + Use doubled ## when necessary for spans (#5566).
    + Ensure correct nesting of strong/emph (#5565): strong
      must be the outer element.

  * JATS writer:

    + Wrap elements with p when needed (#5570).  The JATS spec restricts
      what elements can go inside `fn` and `list-item`.  So we wrap other
      elements inside `<p specific-use="wrapper">` when needed.
    + Properly handle footnotes (#5511) according to "best practice."
      (Group them at the end in `<fn-group>` and use `<xref>` elements
      to link them.)
    + Fix citations with PMID so they validate (#5481).  This includes
      an update to data/jats.csl.
    + Ensure validity of `<pub-date>` by parsing the date and
      extracting year, month, and day, as expected.  Also add an
      iso-8601-date attribute automatically.
    + Don't use `<break>` element for LineBreak. It is only
      allowed in a few special contexts, and not in `<p>` elements.
    + Don't make `<string-name>` a child of `<string>`, which is illegal.

  * FB2 writer:

    + Do not wrap note references into `<sup>` and brackets
      (Alexander Krotov).  Existing FB2 readers, such as FBReader, already
      display links with type="note" as a superscript.
    + Use genre metadata field (#5478).

  * Muse writer: do not escape empty line after `<br>` (Alexander Krotov).

  * Add unicode code point in "Missing character" warning (#5538).
    If the character isn't in the console font, the message is pretty useless,
    so we show the code point for anything non-ASCII.

  * Lua: add Version type to simplify comparisons (Albert Krewinkel).
    Version specifiers like `PANDOC_VERSION` and `PANDOC_API_VERSION` are
    turned into `Version` objects. The objects simplify version-appropriate
    comparisons while maintaining backward-compatibility.
    A function `pandoc.types.Version` is added as part of the newly
    introduced module `pandoc.types`, allowing users to create version
    objects in scripts.

  * pandoc lua module (Albert Krewinkel):

    - Fix deletion of nonexistent attributes (#5569).
    - Better tests for Attr and AttributeList.

  * pandoc.mediabag lua module (Albert Krewinkel):

    + Add function `delete` for deleting a single item.
    + Add function `empty` for removing all entries.
    + Add function `items` for iterating over mediabag.

  * Text.Pandoc.Class: Fix handling of `file:` URL scheme in
    `downloadOrRead` (#5517, Mauro Bieg).  Previously `file:/`
    URLs were handled wrongly and pandoc attempted to make HTTP
    requests, which failed.

  * Text.Pandoc.MIME: add `mediaCategory` [API change] (Mauro Bieg).

  * Text.Pandoc.Shared:

    + Add `onlySimpleTableCells` [API change]
      (Mauro Bieg) and use this to consolidate simple-table detection
      (#5524).  This fixes an inconsistency in the HTML reader, which did not
      treat tables with `<p>` inside cells as simple.
    + `metaToJSON`: treat digits starting with 0 as a string, not a number
      (#5479).  This fixes a regression in YAML metadata in pandoc 2.7.2.

  * Text.Pandoc.Logging: Add `IgnoredElement` constructor for
    `LogMessage`.  `SkippedConetnt` doesn't work for some XML-based
    readers which don't have access to parsec source positions.

  * Text.Pandoc.Asciify: Add Turkish undotted-i (#5433, Mauro Bieg).

  * Improve output of Lua tests (#5499, Albert Krewinkel).
    This makes use of tasty-lua, a package to write tests in Lua
    and integrate the results into Tasty output. Test output becomes
    more informative: individual tests and test groups become visible
    in test output. Failures are reported with helpful error messages.

  * Lua: add `pandoc.system` module (#5468, Albert Krewinkel).
    The `system` Lua module provides utility functions to interact with the
    operating- and file system. E.g.
    `print(pandoc.system.get_current_directory())`
    or
    ```lua
        pandoc.system.with_temporary_directory('tikz', function (dir)
          -- write and compile a TikZ file with pdflatex
        end)
    ```

  * LaTeX template: Add pdflang to hypersetup if lang is set (#5443).

  * beamer template: Fix using Beamer with geometry (#5505, Daniel Maslowski).
    Beamer already loads geometry, so we need to use the `\geometry`
    command to set geometry options.

  * EPUB2/3 templates: Move inline styles to default epub.css (#5466).
    NOTE: Those who use a custom CSS stylesheet with EPUB should
    add these lines:

        code{ white-space: pre-wrap; }
        span.smallcaps{ font-variant: small-caps; }
        span.underline{ text-decoration: underline; }
        q { quotes: "“" "”" "‘" "’"; }
        div.column{ display: inline-block; vertical-align: top; width: 50%; }

  * reveal.js template:

    + Updates for revealjs 3.8.0 (#5435, ebiim).
    + Remove reference to head.min.js (#5448, Winnie Hellmann).
      NOTE: users will need to update reveal.js to at least 3.8.0
      for their presentations generated with this version of pandoc
      to work correctly.

  * Text.Pandoc.PDF:

    + Replace `</>` with literal `/` (#5451).
      We use forward-slash for a directory separator in tmpDir,
      even on Windows (because that's what tex likes).  So we
      should not put a backslash between the tmpDir and the
      filename on Windows.  This is harmless enough in normal
      Windows setups, but it breaks on Cygwin.
      Thanks to @cc2x for noticing and diagnosing the problem.
    + Allow use of `-output-directory` in `--pdf-engine-opt` (#5462).
      This is currently possible with `mklatex` and `-outdir`, but
      was not yet possible with xelatex and `-output-directory`.
    + For PDF via ms/pdfroff, make TOC appear at beginning and in
      PDF bookmarks (#5553).  Previously the TOC appeared at the end
      of the document, and was not bookmarked.  To keep it at the end,
      add `--pdf-engine-opt=--no-toc-relocation` to your command line.

  * Fix broken links in documents (#5473, Shim Myeongseob).
    Also, use absolute links to pandoc.org when possible, so that
    the links can be followed by people viewing these documents
    on GitHub.

  * Improved sample lua tikz filter in lua-filters docs (#5445,
    Matthew Doty).  There are three changes:

    + It only processes elements which begin with `\begin{tikzpicture}`
    + It uses pdf2svg instead of imagemagick to preserve fidelity
    + The images produced have transparent backgrounds

  * MANUAL.txt:

    + Add note about `title-meta`, `author-meta`, `date-meta` (#5486).
    + Fix typo (#5489, Christian Krause).

  * add test/tables.xwiki to git and pandoc.cabal (#5498, Mauro Bieg).

  * Disable missing-home-modules warning in `stack.yaml`.
    Otherwise `stack ghci` fails.


## pandoc 2.7.2 (2019-04-05)

  * Add XWiki writer (#1800, Derek Chen-Becker).
    Add `Text.Pandoc.Writers.XWiki`, exporting `writeXWiki` [API change].

  * Dokuwiki Reader: parse single curly brace (#5416, Mauro Bieg).

  * Vimwiki reader: improve handling of internal links (#5414).
    We no longer append `.html` to link targets, and we add a title
    `wikilink`.  This mirrors behavior of other wiki readers.  Generally
    the `.html` extension is not wanted.  It may be important for output
    to HTML in certain circumstances, but it can always be added using a
    filter that matches on links with title `wikilink`.

    If your workflow requires the current behavior, here is a lua filter
    that will add the `.html` extension:

    ```lua
    function Link(el)
      if el.title == 'wikilink' then
        el.target = el.target .. ".html"
      end
      return el
    end
    ```
  * ipynb reader:

    + Use format `ipynb` for raw cell where no format given.
    + Avoid introducing spurious `.0` on integers in metadata.

  * Markdown reader: fenced div takes priority over setext header.

  * HTML reader: read `data-foo` attribute into `foo` (#5392).
    The HTML writer adds the `data-` prefix for HTML5 for nonstandard
    attributes.  But the attributes are represented in the AST without
    the `data-` prefix, so we should strip this when reading HTML.

  * LaTeX reader: Improve autolink detection (#5340).

  * PowerPoint writer (Jesse Rosenthal):

    + Expand builtin reference doc to model all layouts.
      The previous built-in reference doc had only title and content
      layouts. Add in a section-header slide and a two-content slide, so
      users can more easily modify it to build their own templates.
    + Always open up in slide view.  When editing a
      template/reference-doc, the user might be in Master view, but when
      producing a slide show, it is assumed that slide view will be
      desired.
    + Remove `handoutsMasterList` from template presentation.xml
    + Fix numerous errors in templating (#5402). Previously, some templates
      produced by Office 365 (MacOS) would not render with `--reference-doc`
      correctly. We now apply correct shapes for content, and build
      shape trees correctly.
    + Make default placeholder type for template lookup.
    + Apply speaker notes to metadata slide if applicable.
    + Test for speaker notes after breaking header.
    + Correctly handle notes after section-title header.
      Previously, if notes came after a section-title header (ie, a level-1
      header in a slide-level=2 presentation), they would go on the next
      slide. This keeps them on the slide with the header.
    + Internal improvements.

  * ipynb writer:

    + Use format `ipynb` for raw cell where no format given.
      According to nbformat docs, this is supposed to render in every
      format.  We don't do that, but we at least preserve it as a raw
      block in markdown, so you can round-trip.
    + Consolidate adjacent raw blocks.  Sometimes pandoc creates two
      HTML blocks, e.g. one for the open tag and one for a close tag.
      If these aren't consolidated, only one will show up in output cell.
    + Fixed carry-over of nbformat from metadata.
    + Preserve `nbformat_minor` if it's given.  This helps with round-tripping.

  * LaTeX writer:

    + Avoid inadvertently creating ?` or !` ligatures (#5407).
      These are upside down ? and !, resp.
    + Fix footnotes in table caption and cells (#5367).  This fixes a
      bug wherein footnotes appeared in the wrong order, and with
      duplicate numbers, when in table captions and cells.
      We now use regular `\footnote` commands, even in the table
      caption and the minipages containing cells. Apparently
      longtable knows how to handle this.

  * HTML writer: Don't add data- prefix to RDFa attributes (#5403).

  * JATS writer: Ensure that plain strings go inside `<pub-id>` tag (#5397).

  * Markdown writer:

    + Better rendering of numbers (#5398).  If the number is integral,
      we render it as an integral not a float.
    + Proper rendering of empty map in YAML metadata (#5398).  Should
      be `{}`, not empty string.
    + Properly escape attributes in Markdown writer (#5369).
    + Be sure implicit figures work in list contexts (#5368).
      Previously they would sometimes not work: e.g., when they
      occurred in final paragraphs in lists that were originally
      parsed as Plain and converted later using PlainToPara.

  * Docx writer: Use `w:br` without attributes for line breaks (#5377).
    We previously added the attribute `type="textWrapping"`, but
    this causes problems on Word Online.

  * LaTeX template (Andrew Dunning):

    + Ensure correct heading/table order (#5365).  Improve workaround
      (#1658) for tables following headings.  The new solution works whether
      or not the `indent` variable is enabled.
    + Remove `subparagraph` variable. The default is now to use run-in
      style for level 4 and 5 headings (`\paragraph` and `\subparagraph`).
      To get the previous default behavior (where these were formatted as
      blocks, like `\subsubsection`), set the `block-headings` variable.
    + Add pandoc to PDF metadata (#5388).
    + Group graphics-related code (#5389).
    + Move `\setstretch` after front matter (#5179).  Ensures that
      `\maketitle`, `\tableofcontents`, and so forth are not affected by
      changes to line spacing.

  * Update data/jats.csl to avoid commas between name-part elements
    (#5397).

  * Add support for golang (`go`) with `--listings` (#5427).

  * Text.Pandoc.Shared - improve `metaToJSON` behavior with numbers.
    We now do a better job marshalling numbers from MetaString
    or MetaInlines into JSON Number.

  * Text.Pandoc.Writers.Shared: `metaValueToJSON`: use Number Values for
    integers.  Pandoc's MetaValue doesn't have a distinguished number type,
    so numbers are put in MetaStrings.  If the MetaString consists
    entirely of digits, we convert it to a Number.  We should probably
    consider adding a MetaNumber constructor to MetaValue, for better
    round-tripping with JSON etc.  This change aids round-tripping in
    ipynb metadata fields, like `toc_depth`.

  * Text.Pandoc.Class: `fetchItem`: don't treat UNC paths as
    protocol-relative URLs (#5127).  These are paths beginning `//?/UNC/...`.

  * Text.Pandoc.ImageSize: Improve `pdfSize` so it handles
    a wider range of PDFs (#4322, with help from Richard Davis).

  * Text.Pandoc.Pretty: avoid stack overflow by using strict sum (#5401).

  * Fix harmless error in file-scope code (#5422).

  * MANUAL.txt:

    + Improve 'header' and 'heading' usage (#5423, Andrew
      Dunning).  The term 'header' was being used where 'heading'
      is more appropriate.
    + Add paragraph on options affecting markdown in ipynb.

  * stack.yaml - remove -Wmissing-home-modules
    This seems to cause problems with stack ghci.  Remove RTS options.

  * Add ghc-options to cabal.project.

  * appveyor.yml - use ghc 8.6.4.  Fixes segfault issues on Windows (#5037).

  * linux build process: Remove clone of pandoc-citeproc (#5366).
    It wasn't being used; cabal.project specifies the version to use.

## pandoc 2.7.1 (2019-03-14)

  * Add tectonic as an option for --pdf-engine (#5345, Cormac Relf).
    Runs tectonic on STDIN instead of a temporary .tex file, so that it
    looks in the working directory for `\include` and `\input` like the rest
    of the engines.  Allows overriding the output directory
    args with `--pdf-engine-opt=--outdir --pdf-engine-opt="$DIR"`.

  * Allow `-o/--output` to be used with `--print-default-data-file`,
    `--print-highlighting-style`, `--print-default-template`.
    Note that `-o` must occur BEFORE the `--print*` command on the command
    line (this is documented, #5357).

  * LaTeX reader:

    + Support `\underline`, `\ul`, `\uline` (#5359, Paul
      Tilley).  These are parsed as a Span with class `underline`, as
      with other readers.
    + Ensure that `\Footcite` and `\Footcites` get put in a note.

  * ipynb reader:

    + Remove sensitivity to `raw_html`, `raw_tex` extensions.
      We now include every output format. Pruning is handled by
      `--ipynb-output`.
    + Better handling of cell metadata.  We now include even complex cell
      metadata in the Div's attributes (as JSON, in complex cases, or
      as plain strings in simple cases).

  * ipynb writer:

    + Recurse into native divs for output cell data (#5354).
    + Render cell metadata fields from div attributes.

  * Docx writer: avoid extra copy of abstractNum and num elements
    in numbering.xml.  This caused pandoc-produced docx files to
    be uneditable using Word Online (#5358).

  * Markdown writer: improve handling of raw blocks/inline.
    We now emit raw content using `raw_attribute` when no more
    direct method is available.  Use of `raw_attribute` can be
    forced by disabling `raw_html` and `raw_tex`.

  * LaTeX writer: Add classes for frontmatter support (#5353, Andrew Dunning)
    and remove frontmatter from `scrreprt`.

  * LaTeX template:

    + Improve readability (#5363, Andrew Dunning).
    + Robust section numbering removal (#5351, Andrew Dunning).
      Ensures that section numbering does not reappear with custom
      section levels.  See <https://tex.stackexchange.com/questions/473653/>.
    + Better handling of front/main/backmatter (#5348).
      In pandoc 2.7 we assumed that every class with chapters would accept
      `\frontmatter`, `\mainmatter`, and `\backmatter`.  This is not so (e.g.
      report does not).  So pandoc 2.7 breaks on report class by including an
      unsupported command.  Instead of the `book-class` variable, we use two
      variables, `has-chapters` and `has-frontmatter`, and set these
      intelligently in the writer.

  * Text.Pandoc.Shared: Improve `filterIpynbOutput`.  Ensure that
    images are prioritized over text. `best` should include everything
    for ipynb.

  * Tests.Old: specify `--data-dir=../data` to ensure tests can find
    data files even if they haven't been installed.  Remove old
    `pandoc_datadir` environment variable, which hasn't done anything for a
    long time.

  * MANUAL.txt: Add recommendation to use `raw_attribute` with ipynb (#5354).

  * Use cmark-gfm-hs 0.1.8 (note that 0.1.7 is buggy).

  * Use latest pandoc-citeproc, texmath.


## pandoc 2.7 (2019-03-03)

  * Use XDG data directory for user data directory (#3582). Instead of
    `$HOME/.pandoc`, the default user data directory is now
    `$XDG_DATA_HOME/pandoc`, where `XDG_DATA_HOME` defaults to
    `$HOME/.local/share` but can be overridden by setting the
    environment variable. If this directory is missing, then
    `$HOME/.pandoc` is searched instead, for backwards compatibility.
    However, we recommend moving local pandoc data files from
    `$HOME/.pandoc` to `$HOME/.local/share/pandoc`. On Windows the
    default user data directory remains the same.

  * Slide show formats behavior change:  content under headers
    less than slide level is no longer ignored, but included in
    the title slide (for HTML slide shows) or in a slide after
    the title slide (for beamer).  This change makes possible
    2D reveal.js slideshows with content in the top slide on
    each stack (#4317, #5237).

  * Add command line option `--ipynb-output=all|none|best` (#5339).
    Output cells in ipynb notebooks often contain several different
    versions of an output, with different MIME types, e.g. an HTML
    table and a plain-text fallback.  Specifying `--ipynb-output=best`
    (the default) ensures that the best version for the output format
    is used.  `all` includes all versions, and `none` suppresses them
    all, leaving output cells empty.

  * `asciidoctor` is now an output format separate from `asciidoc`,
    to accommodate some minor implementation-specific differences
    (currently just in the treatment of display math).

  * Add `latexmk` as an option for `--pdf-engine` (#3195).
    Note that you can use `--pdf-engine-opt=-outdir=bar` to specify
    a persistent temp directory.

  * Markdown reader:

    + Improve tight/loose list handling (#5285). Previously the
      algorithm allowed list items with a mix of Para and Plain, which
      is never wanted.
    + Add newline when parsing blocks in YAML (#5271). Otherwise last
      block gets parsed as a Plain rather than a Para. This is a
      regression in pandoc 2.x. This patch restores pandoc 1.19
      behavior.
    + Make `yamlToMeta` respect extensions (#5272, Mauro Bieg).
      This adds a `ReaderOptions` parameter to `yamlToMeta` [API change].
    + Fix bug parsing fenced code blocks (#5304). Previously parsing
      would break if the code block contained a string of backticks of
      sufficient length followed by something other than end of line.

  * LaTeX reader: don't let `\egroup` match `{`. `braced` now actually
    requires nested braces. Otherwise some legitimate command and
    environment definitions can break.

  * Docx reader (Jesse Rosenthal):

    + Rename `getDocumentPath` as `getDocumentXmlPath`.
    + Use field notation for setting `ReaderEnv`.
    + Figure out `document.xml` path once at the beginning of parsing, and
      add it to the environment, so we can avoid repeated lookups.
    + Dynamically determine main document xml path (#5277).
      The desktop Word program places the main document file in
      `word/document.xml`, but the online word places it in
      `word/document2.xml`. This file path is actually stated in the root
      `_rels/.rels` file, in the `Relationship` element with an
      `http://../officedocument` type.
    + Fix paths in archive to prevent Windows failure (#5277).
      Some paths in archives are absolute (have an opening slash) which, for
      reasons unknown, produces a failure in the test suite on MS
      Windows. This fixes that by removing the leading slash if it exists.
    + Add comments to aid code readability.
    + Trim space inside the last inline (#5273).
    + Unwrap sdt elements in footnotes and comments (#5302).

  * Muse reader (Alexander Krotov):

    + Test that block level markup does not break `<verbatim>`.
    + Add secondary note support.

  * ipynb reader: handle images referring to attachments. Previously
    we didn't strip off the attachment: prefix, so even though the
    attachment was available in the mediabag, pandoc couldn't find it.

  * JATS reader:

    + Fix parsing of figures (#5321). This ensures that a figure
      containing a single image is parsed as a pandoc "implicit
      figure" (i.e., a Para with a single Image whose title attribute
      begins with `fig:`). More complex figures will still be parsed
      as divs.
    + Support `fig-group` block element (#5317).
    + Handle citations with multiple references (#5310). The `rid`
      attribute can have a space-separated list of ids.

  * AsciiDoc Writer: Add `writeAsciiDoctor` [API change, Tarik Graba].
    Handle display math appropriately for Asciidoctor.

  * JATS writer: wrap figure caption in `<p>` to fix validation (#5290,
    Mauro Bieg).

  * HTML writer:

    + Implement WAI-ARIA roles for (end)notes, citations, and
      bibliography (#4213). Note that doc-biblioref is only used when
      link-citations produces links, since it belongs on links.
    + Include content (including speaker notes) in title slides
      (#4317, #5237).

  * ipynb writer:

    + Ensure final newline.
    + Only include metadata under `jupyter` field.
    + Don't create attachments for images with absolute URIs,
      including data: URIs (#5303).
    + Keep plain text fallbacks in output even if a richer format is
      included (#5293). We don't know what output format will be needed.
      See the `--ipynb-output` command line option for a way to control
      what formats are included in the output.

  * Markdown writer: use `markdown="1"` when appropriate for Divs:
    when `native_divs` and `markdown_in_html_blocks` are disabled
    but `raw_html` and `markdown_attribute` are enabled.

  * LaTeX writer:

    + Use right fold for `escapeString`.  This is more elegant than
      the explicit recursive code we were using.
    + Avoid `{}` after control sequences when escaping.
      `\ldots{}.` doesn't behave as well as `\ldots.` with the latex
      ellipsis package.  This patch causes pandoc to avoid emitting
      the `{}` when it is not necessary.  Now `\ldots` and other
      control sequences used in escaping will be followed by either
      a `{}`, a space, or nothing, depending on context.
    + For beamer, include contents under headers superordinate to
      slidelevel (#4317). Currently we keep the fancy title slide, and
      add a new slide with the same title and whatever content was
      under the header.

  * Powerpoint writer (Jesse Rosenthal): support underlines.
    Use span with single class "underline" as in docx writer.

  * Muse writer: escape secondary notes (Alexander Krotov).

  * FB2 writer: add section identifiers support (#5229, John KetzerX).

  * Make `--fail-if-warnings` work for PDF output (#5343).

  * Lua filters (Albert Krewinkel):

    + Load module `pandoc` before calling `init.lua` (#5287). The file
      `init.lua` in pandoc's data directory is run as part of pandoc's
      Lua initialization process. Previously, the `pandoc` module was
      loaded in `init.lua`, and the structure for marshaling was
      set up after. This allowed simple patching of element
      marshaling, but made using `init.lua` more difficult. Now, all
      required modules are now loaded before calling `init.lua`. The
      file can be used entirely for user customization. Patching
      marshaling functions, while discouraged, is still possible via
      the `debug` module.
    + All Lua modules bundled with pandoc, i.e., `pandoc.List`,
      `pandoc.mediabag`, `pandoc.utils`, and `text` are re-exported from the
      `pandoc` module. They are assigned to the fields `List`, `mediabag`,
      `utils`, and `text`, respectively.

  * Text.Pandoc.Lua (Albert Krewinkel):

    + Split `StackInstances` into smaller Marshaling modules.
    + Get `CommonState` from Lua global. This allows more control over
      the common state from within Lua scripts.

  * LaTeX template:

    + Support the `subject` metadata variable (#5289, Pascal Wagler).
    + Add `\frontmatter`, `\mainmatter`, `\backmatter`
      for book classes (#5306).

  * epub3 template: Add titlepage class to section (#5269).

  * HTML5 template: Add ARIA role `doc-toc` for table of contents (#4213).

  * Make `--metadata-file` use selected extensions (#5279, #5272, Mauro Bieg).

  * Text.Pandoc.Shared:

    + Remove `withTempDir` [API change].
    + Add new exported function `defaultUserDataDirs` [API change].
    + Add `filterIpynbOutput` [API change].
    + `compactify`: Avoid lists with a mix of Plain and Para elements
      (#5285).

  * Text.Pandoc.Translations: reorder alphabetically and remove `Author`
    (#5334, Mauro Bieg).

  * Text.Pandoc.Extensions:

    + More carefully groom ipynb default extensions.
    + Add `all_symbols_escapable` to `githubMarkdownExtensions`.

  * Text.Pandoc.PDF:

    + Use system temp directory when possible (#1192). Previously we
      created temp dirs in the working directory, partly (a) because
      there were problems using the system temp directory on Windows,
      when their pathnames included tildes, and partly (b) because
      programs like `epstopdf.pl` would not be allowed to write to
      directories outside the working directory in restricted mode. We
      now (a) use the system temp dir except when the path includes
      tildes, and (b) set `TEXMFOUTPUT` when creating the PDF, so that
      subsidiary programs can use the system temp directory. This
      addresses problems that occurred when pandoc was used in a
      synced directory (such as Dropbox).
    + Change types of subsidiary functions to PandocIO, to allow
      warnings to be threaded through (#5343).

  * Text.Pandoc.MIME: add WebP (#5267, Mauro Bieg).

  * Tests: avoid calling `findPandoc` multiple times.

  * Old tests: remove need for temp files by using `pipeProcess`.

  * Added simple ipynb reader/writer tests (#5274).

  * Rearrange `--help` output in a more rational way, with common
    options at the beginning and options grouped by function (#5336).

  * trypandoc: Add JATS and other missing formats (Arfon Smith, #5291).

  * Add missing copyright notices and remove license boilerplate (#4592,
    Albert Krewinkel).

  * Use latest basement/foundation on 32bit windows.

  * Use latest skylighting (#5328). Custom syntax definitions no
    longer try to load `language.dtd`.

  * Require texmath 0.11.2.1

  * Use latest pandoc-citeproc (0.16.1.1).

  * MANUAL.txt:

    + Clarify variable substitution indentation in templates (#5338,
      Agustín Martín Barbero).
    + Reorder custom-styles section (#5324, Mauro Bieg).

## pandoc 2.6 (2019-01-30)

  * Support ipynb (Jupyter notebook) as input and output format.

    + Add `ipynb` as input and output format (extension `.ipynb`).
    + Added Text.Pandoc.Readers.Ipynb [API change].
    + Added Text.Pandoc.Writers.Ipynb [API change].
    + Add `PandocIpynbDecodingError` constructor to Text.Pandoc.Error.Error
      [API change].
    + Depend on ipynb library.
    + Note: there is no template for ipynb.

  * Add DokuWiki reader (#1792, Alexander Krotov).  This adds
    Text.Pandoc.Readers.DokuWiki [API change], and adds `dokuwiki`
    as an input format.

  * Implement task lists (#3051, Mauro Bieg). Added `task_lists`
    extension. Task lists are supported from markdown and gfm input.
    They should work, to some degree, in all output formats, though in
    most formats you'll get a bullet list with a unicode character for
    the box. In HTML, you get checkboxes and in LaTeX/PDF output, a
    box is used as the list marker. API changes:

    + Added constructor `Ext_task_lists` to `Extension`.
    + Added `taskListItemFromAscii` and `taskListItemToAscii` to
      Text.Pandoc.Shared.

  * Allow some command line options to take URL in addition to FILE.
    `--include-in-header`, `--include-before-body`, `--include-after-body`.

  * HTML reader:

    + Handle empty `start` attribute (see #5162).
    + Treat `textarea` as a verbatim environment (#5241) and preserve
      spacing.

  * RST reader:

    + Change treatment of `number-lines` directive (Brian Leung,
      #5207). Directives of this type without numeric inputs should
      not have a `startFrom` attribute; with a blank value, the
      writers can produce extra whitespace.
    + Removed superfluous `sourceCode` class on code blocks (#5047).
    + Handle `sourcecode` directive as synonynm for `code` (#5204).

  * Markdown reader:

    + Remove `sourceCode` class for literate Haskell code blocks
      (#5047). Reverse order of `literate` and `haskell` classes on
      code blocks when parsing literate Haskell, so `haskell` is
      first.
    + Treat `<textarea>` as a verbatim environment (#5241).

  * Org reader:

    + Handle `minlevel` option differently (#5190, Brian Leung).
      When `minlevel` exceeds the original minimum level observed in
      the file to be included, every heading should be shifted
      rightward.
    + Allow for case of `:minlevel == 0` (#5190).
    + Fix treatment of links to images (#5191, Albert Krewinkel).
      Links with descriptions which are pointing to images are no
      longer parsed as inline images, but as links.
    + Add support for #+SELECT_TAGS (Brian Leung).
    + Separate filtering logic from conversion function (Brian Leung).

  * TWiki reader: Fix performance issue with underscores (#3921).

  * MediaWiki reader: use `_` instead of `-` in auto-identifiers (#4731).
    We may not still be exactly matching mediawiki's algorithm.

  * LaTeX reader:

    + Remove `sourceCode` class for literate Haskell code blocks
      (#5047). Reverse order of `literate` and `haskell` classes on
      code blocks when parsing literate Haskell, so `haskell` is
      first.
    + Support `\DeclareMathOperator` (#5149).
    + Support `\inputminted` (#5103).
    + Support `\endinput` (#5233).
    + Allow includes with dots like `cc_by_4.0`.  Previously the `.0`
      was interpreted as a file extension, leading pandoc not to add
      `.tex` (and thus not to find the file).  The new behavior matches
      tex more closely.

  * Man reader:

    + Use `mapLeft` from Shared instead of defining own.

  * Docx reader (Jesse Rosenthal):

    + Handle level overrides (#5134).

  * Docx writer:

    + Support custom properties (#3024, #5252, Agustín Martín Barbero).
      Also supports additional core properties:  `subject`, `lang`,
      `category`, `description`.
    + Make Level into a real type, instead of an alias for a tuple
      (Jesse Rosenthal).

  * ICML writer (Mauro Bieg):

    + Support custom-styles (#5137, see #2106).
    + Support unnumbered headers (#5140).

  * Texinfo writer: Use header identifier for anchor if present (#4731).
    Previously we were overwriting an existing identifier with a new one.

  * Org writer: Preserve line-numbering for example and code blocks
    (Brian Leung).

  * Man/Ms writers: Don't escape `-` as `\-`. The `\-` gets rendered
    in HTML and PDF as a unicode minus sign.

  * Ms writer: Ensure we have a newline after .EN in display math (#5251).

  * RST writer: Don't wrap simple table header lines (#5128).

  * Asciidoc writer: Shorter delimiters for tables, blockquotes
    (#4364). This matches asciidoctor reference docs.

  * Dokuwiki writer: Remove automatic `:` prefix before internal image
    links (#5183, Damien Clochard).  This prevented users from making
    relative image links.

 *  Zimwiki writer: remove automatic colon prefix before internal
    images (#5183, Damien Clochard).

  * MediaWiki writer: fix caption, use 'thumb' instead of 'frame'
    (#5105). Captions used to have the word 'caption' prepended; this
    has been removed. Also, 'thumb' is used instead of 'frame' to
    allow images to be resized.

  * reveal.js writer:

    + Ensure that we don't get > 2 levels of section nesting,
      even with slide level > 2 (#5168).
    + If slide level == N but there is no N-level header, make
      sure the next header with level > N gets treated as a slide
      and put in a section, rather than remaining loose (#5168).

  * Markdown writer:

    + Make `plain` RawBlocks pass through in `plain` output.
    + Include needed whitespace after HTML figure (#5121).
      We use HTML for a figure in markdown dialects that can't
      represent it natively.

  * Commonmark writer:

    + Fix handling of SoftBreak with `hard_line_breaks` (#5195).
    + Implement `--toc` (`writerTableOfContents`)
      in commonmark/gfm writers (#5172).

  * EPUB writer:

    + Ensure that picture transforms are done on metadata too.
    + Small fixes to `nav.xhtml`:
      Add 'landmarks' id attribute to the landmarks nav.
      Replace old default CSS removing numbers from ol.toc li
      with new rules that match `nav#toc ol, nav#landmarks ol`.
      We keep the `toc` class on `ol` for backwards compatibility.

  * LaTeX writer:

    + Make raw content marked `beamer` pass through in `beamer`
      output (pandoc/lua-filters#40).
    + Beamer: avoid duplicated `fragile` property in some cases
      (#5208).
    + Add `#` special characters for listings (#4939).
      This character needs special handling in `\lstinline`.

 *  RTF writer: use `toTableOfContents` from Shared to replace
    old duplicated code.

  * Pptx writer:

    + Support custom properties.  Also supports additional core
      properties: `subject`, `category`, `description` (#5252,
      Agustín Martín Barbero).
    + Use `toTableOfContents` from Shared to replace old duplicated code.

  * ODT writer (Augustín Martín Barbero):

    + Fix typo in custom properties (#2839).
    + Improve standard properties, including the following core
      properties: `generator` (Pandoc/VERSION), `description`, `subject`,
      `keywords`, `initial-creator` (from authors), `creation-date`
      (actual creation date) (#5252).

  * Custom writers:

    + Allow '-' in filenames for custom lua writers (#5187).
    + sample.lua: add `SingleQuoted`, `DoubleQuoted` (#5104).
    + sample.lua: Add a missing `>` (MichaWiedenmann).

  * reveal.js template:  Add `zoomKey` config (#4249).

  * HTML5 template: Remove unnecessary type="text/css" on style and
    link for HTML5 (#5146).

  * LaTeX template (Andrew Dunning, except where noted):

    + Prevent fontspec from scaling `mainfont` to match the default
      font, Latin Modern. A main font set to 12pt could
      previously appear between 11pt to 13pt depending on its design.
      To return to the earlier rendering, use `-V
      mainfontoptions="Scale=MatchLowercase"` (#5212, #5218).
    + Display monospaced fonts without TeX ligatures when using
      `--pdf-engine=lualatex`. It now matches the behaviour of other
      engines (#5212, #5218).
    + Remove the deprecated `romanfont` variable. The functionality of
      `mainfont` is identical (#5218).
    + Render `\subtitle` with the standard document classes.
      Previously, `subtitle` only appeared when using the KOMA-Script
      classes or Beamer (#5213, #5244).
    + Use Babel instead of Polyglossia for LuaLaTeX. This avoids
      several language selection problems, notably with retaining
      French spacing conventions when switching to a verbatim
      environment or another language; and in printing Greek text
      without hyphenation (#5193).
    + Use the [`xurl`](https://ctan.org/pkg/xurl) package if
      available, improving the appearance of URLs by allowing them to
      break at additional points (#5193).
    + Use [`bookmark`](https://ctan.org/pkg/bookmark) if available
      to correct heading levels in PDF bookmarks: see the [KOMA-Script
      3.26 release notes](https://komascript.de/release3.26) (#5193).
    + Require the [`xcolor`](https://ctan.org/pkg/xcolor) package to
      avoid a possible error when using additional packages alongside
      footnotes in tables (#5193, closes #4861).
    + Remove obsolete `fixltx2e` package, which has no functionality
      with TeX Live 2015 or later (#5193).
    + Allow multiple `fontfamilies.options` (#5193, closes #5194).
    + Restrict `institute` variable to Beamer (#5219).
    + Use [`footnotehyper`](https://ctan.org/pkg/footnotehyper)
      package if available to make footnotes in tables compatible
      with `hyperref` (#5234).
    + Number parts and chapters in book classes only if the
      `numbersections` variable is set, for consistency with other
      output formats. To return to the previous behaviour, use
      `-V numbersections -V secnumdepth=0` (#5235).
    + Reindent file (#5193).
    + Use built-in parskip handling with KOMA-Script classes (#5143, Enno).
    + Set default listings language for lua, assembler (#5227, John
      MacFarlane).  Otherwise we get an error when trying to compile code
      with lua or assembler code.  To change the default dialect
      (currently 5.3 for lua and x86masm for assembler), you can use
      `--include-in-header` to inject something like
      `\lstset{defaultdialect=[5.2]Lua}`.

  * Text.Pandoc.Readers: Changed types of `readJSON`; it now runs
    in an instance of PandocMonad, like the other readers and
    writers.  [API change]

  * Text.Pandoc.Writers: Changed types of `writeJSON`; it now runs
    in an instance of PandocMonad, like the other readers and
    writers.  [API change]

  * Text.Pandoc.Error: Added `PandocUTF8DecodingError` constructor
    for `PandocError`. [API change]

  * Text.Pandoc.Writers.Shared - add `toTableOfContents`. [API change]
    This is refactored out from the Markdown writer.
    It can be used in other writers to create a table of contents.

  * Improve error messages for UTF-8 decoding errors. Now we give
    the filename and byte offset (#4765).

  * Text.Pandoc.XML: Strip out illegal XML characters in
    `escapeXMLString` (#5119).

  * Text.Pandoc.Process: update `pipeProcess` (Albert Krewinkel).
    The implementation of `pipeProcess` was rewritten to fix sporadic
    failures caused by prematurely closed pipes.

  * Use `safeRead` instead of `read` everywhere in the code
    (John MacFarlane, Mauro Bieg, #5162, #5180).

  * Text.Pandoc.SelfContained: Decompress `.svgz` when
    converting to `data:` URI (#5163, Alexander Krotov).

  * Text.Pandoc.Parsing: Remove unused `HasHeaderMap`
    (#5175, Alexander Krotov). [API change]

  * Normalize Windows paths to account for change in ghc 8.6 (#5127).
    When pandoc is compiled with ghc 8.6, Windows paths are treated
    differently, and paths beginning `\\server` no longer work.
    This commit rewrites such patsh to `\\?\UNC\server` which works.
    The change operates at the level of argument parsing, so it
    only affects the command line program.

  * Simplify/fix reading of `--metadata` values on command line
    to avoid problems relating to hvr/HsYAML#7 (#5177).

  * data/pandoc.lua: auto-fix nested constructor arguments (Albert
    Krewinkel).  Incorrect types to pandoc element constructors are
    automatically converted to the correct types when possible. This was
    already done for most constructors, but conversions are now also done
    for nested types (like lists of lists).

  * Removed custom Setup.hs, use build-type: simple. The only thing we
    gained from the custom build was automatic installation of the man
    page when using 'cabal install'. But custom builds cause problems,
    e.g., with cross-compilation. Installation of the man page is
    better handled by packagers. Note to packagers (e.g. Debian): it
    may be necessary to add a step installing the man page with the
    next release.

  * Allow latest http-client, tasty, zip-archive, Glob.

  * Require skylighting >= 0.7.5, adding support for sml, J,
    typescript.

   * Tests:  Cleaned up `findPandoc` in `Tests.Helpers`, so it
     works well with cabal v2.

  * INSTALL.md:

    + Use button for installer links (John MacFarlane, Mauro Bieg,
      #5167).
    + Fix links and bump required stack version (max).
    + Improve installation notes on associated software (Andrew
      Dunning). Includes explanation of how to install related tools
      with package managers (since the method of installing
      `rsvg-convert` is not obvious).

  * doc/org.md: improve documentation of org features (Albert
    Krewinkel).

  * doc/lua-filters.md:  use 3rd level headers for module fields.

  * MANUAL:

    + Clarify automatic identifiers (#5201). We remove
      non-alphanumerics. This includes, e.g., emojis.
    + Fix example for Div with id (Geoffrey Ely).
    + Update list of LaTeX packages used.
    + Make it clear that `hard_line_breaks` works in `gfm` (see #5195).
    + Mention `raw_attribute` in documentation for `raw_html` and
      `raw_tex` (#5240, thanks to @eiro).
    + Clarify that `$sep$` must come right before `$endfor$` in templates
      (#5243, Lev Givon).
    + Document metadata support for docx, odt, pptx writers (Agustín
      Martín Barbero).
    + Reorganize template variables (#5249, Andrew Dunning).
      Add additional headings to categorize variables, and
      alphabetize when there is large number; add more examples.
    + Document `date-meta` template variable (#5260, Tristan Stenner).

  * trypandoc: Fix CSS and viewport.

## pandoc 2.5 (2018-11-27)

  * Text.Pandoc.App: split into several unexported submodules (Albert
    Krewinkel):  Text.Pandoc.App.FormatHeuristics,
    Text.Pandoc.App.Opt, Text.Pandoc.App.CommandLineOptions,
    Text.Pandoc.App.OutputSettings.  This is motivated partly by the
    desire to reduce recompilations when something is modified,
    since App previously depended on virtually every other module.

  * Text.Pandoc.Extensions

    + Semantically, `gfm_auto_identifiers` is now a modifier of
      `auto_identifiers`; for identifiers to be set, `auto_identifiers` must
      be turned on, and then the type of identifier produced depends on
      `gfm_auto_identifiers` and `ascii_identifiers` are set. Accordingly,
      `auto_identifiers` is now added to `githubMarkdownExtensions` (#5057).
    + Remove `ascii_identifiers` from `githubMarkdownExtensions`.
      GitHub doesn't seem to strip non-ascii characters any more.

  * Text.Pandoc.Lua.Module.Utils (Albert Krewinkel)

    + Test AST object equality via Haskell (#5092).  Equality of Lua
      objects representing pandoc AST elements is tested by unmarshalling
      the objects and comparing the result in Haskell.
      A new function `equals` which performs this test has been added to the
      `pandoc.utils` module.
    + Improve stringify.  Meta value strings (MetaString)
      and booleans (MetaBool) are now converted to the literal string and the
      lowercase boolean name, respectively. Previously, all values of these
      types were converted to the empty string.

  * Text.Pandoc.Parsing: Remove Functor and Applicative constraints where Monad
    already exists (Alexander Krotov).

  * Text.Pandoc.Pretty: Don't render BreakingSpace at end of line
    or beginning of line (#5050).

  * Text.Pandoc.Readers.Markdown

    + Fix parsing of citations, quotes, and underline emphasis
      after symbols.  Starting with pandoc 2.4, citations, quoted inlines,
      and underline emphasis were no longer recognized after certain
      symbols, like parentheses (#5099, #5053).
    + In pandoc 2.4, a soft break after an abbreviation would be
      relocated before it to allow for insertion of a nonbreaking
      space after the abbreviation. This behavior is here reverted.
      A soft break after an abbreviation will remain, and no nonbreaking
      space will be added. Those who care about this issue should take care not
      to end lines with an abbreviation, or to insert nonbreaking spaces
      manually.

  * Text.Pandoc.Readers.FB2: Do not throw error for unknown elements in
    `<body>` (Alexander Krotov).  Some libraries include custom elements
    in their FB2 files.

  * Text.Pandoc.Readers.HTML

    + Allow `tfoot` before body rows (#5079).
    + Parse `<small>` as a Span with class "small" (#5080).
    + Allow thead containing a row with `td` rather than `th` (#5014).

  * Text.Pandoc.Readers.LaTeX

    + Cleaned up handling of dimension arguments.  Allow decimal points,
      preceding space.
    + Don't allow arguments for verbatim, etc.
    + Allow space before bracketed options.
    + Allow optional arguments after `\\` in tables.
    + Improve parsing of `\tiny`, `\scriptsize`, etc.  Parse as raw,
      but know that these font changing commands take no arguments.

  * Text.Pandoc.Readers.Muse

    + Trim whitespace before parsing grid table cells (Alexander Krotov).
    + Add grid tables support (Alexander Krotov).

  * Text.Pandoc.Shared

    + For bibliography match Div with id `refs`, not class `references`.
      This was a mismatch between pandoc's docx, epub, latex, and markdown
      writers and the behavior of pandoc-citeproc, which actually looks for a
      div with id `refs` rather than one with class `references`.
    + Exactly match GitHub's identifier generating algorithm (#5057).
    + Add parameter for `Extensions` to `uniqueIdent` and
      `inlineListToIdentifier` (#5057).  [API change]
      This allows these functions to be sensitive to the settings of
      `Ext_gfm_auto_identifiers` and `Ext_ascii_identifiers`, and allows us to
      use `uniqueIdent` in the CommonMark reader, replacing custom code.  It
      also means that `gfm_auto_identifiers` can now be used in all formats.

  * Text.Pandoc.Writers.AsciiDoc

    + Use `.`+ as list markers to support nested ordered lists (#5087).
    + Support list number styles (#5089).
    + Render Spans using `[#id .class]#contents#` (#5080).

  * Text.Pandoc.Writers.CommonMark

    + Respect `--ascii` (#5043, quasicomputational).
    + Make sure `--ascii` affects quotes, super/subscript.

  * Text.Pandoc.Writers.Docx

    + Fix bookmarks to headers with long titles (#5091).
      Word has a 40 character limit for bookmark names.  In addition, bookmarks
      must begin with a letter.  Since pandoc's auto-generated identifiers may
      not respect these constraints, some internal links did not work.  With
      this change, pandoc uses a bookmark name based on the SHA1 hash of the
      identifier when the identifier isn't a legal bookmark name.
    + Add bookmarks to code blocks (Nikolay Yakimov).
    + Add bookmarks to images (Nikolay Yakimov).
    + Refactor common bookmark creation code into a function (Nikolay Yakimov).

  * Text.Pandoc.Writers.EPUB: Handle calibre metadata (#5098).
    Nodes of the form

          <meta name="calibre:series" content="Classics on War and Politics"/>

      are now included from an epub XML metadata file.  You can also include
      this information in your YAML metadata, like so:

          calibre:
           series: Classics on War and Policitics

      In addition, ibooks-specific metadata can now be included via an XML
      file. (Previously, it could only be included via YAML metadata, see
      #2693.)

  * Text.Pandoc.Writers.HTML: Use plain `"` instead of `&quot;` outside of
    attributes.

  * Text.Pandoc.Writers.ICML: Consolidate adjacent strings, inc. spaces.
    This avoids splitting up the output unnecessarily into separate elements.

  * Text.Pandoc.Writers.LaTeX: Don't emit `[<+->]` unless beamer output, even
    if `writerIncremental` is True (#5072).

  * Text.Pandoc.Writers.Muse (Alexander Krotov).

    + Output tables as grid tables if they have multi-line cells.
    + Indent simple tables only on the top level.
    + Output tables with one column as grid tables.
    + Add support for `--reference-location`.
    + Internal improvements.

  * Text.Pandoc.Writers.OpenDocument: Fix list indentation (Nils Carlson,
    #5095).  This was a regression in pandoc 2.4.

  * Text.Pandoc.Writers.RTF: Fix warnings for skipped raw inlines.

  * Text.Pandoc.Writers.Texinfo: Add blank line before `@menu` section (#5055).

  * Text.Pandoc.XML: in `toHtml5Entities`, prefer shorter entities
    when there are several choices for a particular character.

  * data/abbreviations

    + Add additional abbreviations (Andrew Dunning)
      Many of these borrowed from the Chicago Manual of Style 10.42,
      'Scholarly abbreviations'.

  * Templates

    + Asciidoc template:  add :lang: to title header is lang is set in
      metadata (#5088).

  * pandoc.cabal: Add cabal flag `derive_json_via_th` (Albert Krewinkel)
    Disabling the flag will cause derivation of ToJSON and FromJSON
    instances via GHC Generics instead of Template Haskell. The flag is
    enabled by default, as deriving via Generics can be slow (see #4083).

  * trypandoc:

    + Tweaked drop-down lists.
    + Put link to site in footer.
    + Preselect output format.
    + Update on change of in or out format.
    + Add man input format.

  * MANUAL.txt:

    + Fix outdated description of latex_macros extension.
    + Clarified placement of bibliography.
    + Added "A note on security."
    + Fix note on curly brace syntx for locators.
    + Document new explicit syntax for citeproc locators.
    + Remove confusing cross-links for some extensions.
    + Don't put pandoc in code ticks in heading.
    + Document that `--ascii` works for gfm and commonmark too.
    + Add `man` to `--from` options.

  * doc/customizing-pandoc.md: various improvements (Mauro Bieg).


## pandoc 2.4 (2018-11-03)

### New features

  * New input format `man` (Yan Pashkovsky, John MacFarlane).

### Behavior changes

  * `--ascii` is now implemented in the writers, not in Text.Pandoc.App,
    via the new `writerPreferAscii` field in `WriterOptions`.
    Now the `write*` functions for Docbook, HTML, ICML, JATS, LaTeX,
    Ms, Markdown, and OPML are sensitive to `writerPreferAscii`.
    Previously the to-ascii translation was done in Text.Pandoc.App,
    and thus not available to those using the writer functions
    directly.

  * `--ascii` now works with Markdown output.  HTML5 character reference
    entities are used.

  * `--ascii` now works with LaTeX output. 100% ASCII output can't be
    guaranteed, but the writer will use commands like `\"{a}` and `\l`
    whenever possible, to avoid emitting a non-ASCII character.

  * For HTML5 output, `--ascii` now uses HTML5 character reference
    entities rather than numerical entities.

  * Improved detection of format based on extension (in Text.Pandoc.App).
    We now ensure that if someone tries to convert a file for a
    format that has a pandoc writer but not a reader, it won't just
    default to markdown.

  * Add viz. to abbreviations file (#5007, Nick Fleisher).

  * AsciiDoc writer:  always use single-line section headers,
    instead of the old underline style (#5038).  Previously the single-line
    style would be used if `--atx-headers` was specified, but
    now it is always used.

  * RST writer: Use simple tables when possible (#4750).

  * CommonMark (and gfm) writer: Add plain text fallbacks. (#4528,
    quasicomputational). Previously, the writer would unconditionally
    emit HTML output for subscripts, superscripts, strikeouts (if the
    strikeout extension is disabled) and small caps, even with
    `raw_html` disabled. Now there are plain-text (and, where
    possible, fancy Unicode) fallbacks for all of these corresponding
    (mostly) to the Markdown fallbacks, and the HTML output is only
    used when `raw_html` is enabled.

  * Powerpoint writer: support raw openxml (Jesse Rosenthal, #4976).
    This allows raw openxml blocks and inlines to be used in the pptx
    writer. Caveats: (1) It's up to the user to write
    well-formed openxml. The chances for corruption, especially with
    such a brittle format as pptx, is high. (2) Because of
    the tricky way that blocks map onto shapes, if you are using
    a raw block, it should be the only block on a slide
    (otherwise other text might end up overlapping it). (3) The
    pptx ooxml namespace abbreviations are different from the
    docx ooxml namespaces. Again, it's up to the user to get it
    right. Unzipped document and ooxml specification should be
    consulted.

  * With `--katex` in HTML formats, do not use the autorenderer (#4946).
    We no longer surround formulas with `\(..\)` or `\[..\]`. Instead,
    we tell katex to convert the contents of span elements with
    class "math". Since math has already been identified, this
    avoids wasted time parsing for LaTeX delimiters. Note, however,
    that this may yield unexpected results if you have span elements
    with class "math" that don't contain LaTeX math.
    Also, use latest version of KaTeX by default (0.9.0).

  * The man writer now produces ASCII-only output, using groff escapes,
    for portability.

  * ODT writer:

    + Add title, author and date to metadata; any remaining
      metadata fields are added as `meta:user-defined` tags.
    + Implement table caption numbering (#4949, Nils Carlson).
      Captioned tables are numbered and labeled with format "Table 1:
      caption", where "Table" is replaced by a translation, depending
      on the value of `lang` in metadata. Uncaptioned tables are not
      enumerated.
    + OpenDocument writer: Implement figure numbering in captions (#4944,
      Nils Carlson). Figure captions are now numbered 1, 2, 3, ...
      The format in the caption is "Figure 1: caption" and so on
      (where "Figure" is replaced by a translation, depending on the
      value of `lang` in the metadata). Captioned figures are numbered
      consecutively and uncaptioned figures are not enumerated. This
      is necessary in order for LibreOffice to generate an
      Illustration Index (Table of Figures) for included figures.

  * RST reader: Pass through fields in unknown directives as div attributes
    (#4715). Support `class` and `name` attributes for all directives.

  * Org reader: Add partial support for `#+EXCLUDE_TAGS` option. (#4284,
    Brian Leung). Headers with the corresponding tags should not
    appear in the output.

  * Log warnings about missing title attributes now include a
    suggestion about how to fix the problem (#4909).

  * Lua filter changes (Albert Krewinkel):

    + Report traceback when an error occurs. A proper Lua traceback is
      added if either loading of a file or execution of a filter
      function fails. This should be of help to authors of Lua filters
      who need to debug their code.

    + Allow access to pandoc state (#5015). Lua filters and custom
      writers now have read-only access to most fields of pandoc's
      internal state via the global variable `PANDOC_STATE`.

    + Push ListAttributes via constructor (Albert Krewinkel).
      This ensures that ListAttributes, as present in OrderedList elements,
      have additional accessors (viz. `start`, `style`, and `delimiter`).

    + Rename ReaderOptions fields, use snake_case. Snake case is used
      in most variable names, using camelCase for these fields was an
      oversight. A metatable is added to ensure that the old field
      names remain functional.

    + Iterate over AST element fields when using `pairs`. This makes
      it possible to iterate over all ield names of an AST element by
      using a generic `for` loop with pairs`:

          for field_name, field_content in pairs(element) do
          ...
          end

      Raw table fields of AST elements should be considered an
      implementation detail and might change in the future. Accessing
      element properties should always happen through the fields
      listed in the Lua filter docs.

      Note that the iterator currently excludes the `t`/`tag` field.

    + Ensure that MetaList elements behave like Lists. Methods usable
      on Lists can also be used on MetaList objects.

    + Fix MetaList constructor (Albert Krewinkel). Passing a MetaList
      object to the constructor `pandoc.MetaList` now returns the
      passed list as a MetaList. This is consistent with the
      constructor behavior when passed an (untagged) list.

  * Custom writers: Custom writers have access to the global variable
    `PANDOC_DOCUMENT`(Albert Krewinkel, #4957). The variable contains
    a userdata wrapper around the full pandoc AST and exposes two
    fields, `meta` and `blocks`. The field content is only
    marshaled on-demand, performance of scripts not accessing the
    fields remains unaffected.

### API changes

  * Text.Pandoc.Options: add `writerPreferAscii` to `WriterOptions`.

  * Text.Pandoc.Shared:

    + Export `splitSentences`. This was previously duplicated in the Man and
      Ms writers.
    + Add `ToString` typeclass (Alexander Krotov).

  * New exported module Text.Pandoc.Filter (Albert Krewinkel).

  * Text.Pandoc.Parsing

    + Generalize `gridTableWith` to any `Char` Stream (Alexander Krotov).
    + Generalize `readWithM` from `[Char]` to any `Char` Stream
      that is a `ToString` instance (Alexander Krotov).

  * New exposed module Text.Pandoc.Filter (Albert Krewinkel).

  * Text.Pandoc.XML: add `toHtml5Entities`.

  * New exported module Text.Pandoc.Readers.Man (Yan Pashkovsky, John
    MacFarlane).

  * Text.Pandoc.Writers.Shared

    + Add exported functions `toSuperscript` and
      `toSubscript` (quasicomputational, #4528).
    + Remove exported functions `metaValueToInlines`,
      `metaValueToString`. Add new exported functions
      `lookupMetaBool`, `lookupMetaBlocks`, `lookupMetaInlines`,
      `lookupMetaString`. Use these whenever possible for uniformity
      in writers (Mauro Bieg, #4907). (Note that
      removed function `metaValueToInlines` was in previous
      released versions.)
    + Add `metaValueToString`.

  * Text.Pandoc.Lua

    + Expose more useful internals (Albert Krewinkel):

      - `runFilterFile` to run a Lua filter from file;
      - data type `Global` and its constructors; and
      - `setGlobals` to add globals to a Lua environment.

      This module also contains `Pushable` and `Peekable` instances
      required to get pandoc's data types to and from Lua. Low-level
      Lua operation remain hidden in Text.Pandoc.Lua.

    + Rename `runPandocLua` to `runLua` (Albert Krewinkel).

    + Remove `runLuaFilter`, merging this into Text.Pandoc.Filter.Lua's
      `apply` (Albert Krewinkel).

### Bug fixes and under-the-hood improvements

  * Text.Pandoc.Parsing

    + Make `uri` accept any stream with Char tokens (Alexander Krotov).
    + Rewrite `uri` without `withRaw` (Alexander Krotov).
    + Generalize `parseFromString` and `parseFromString'` to any
      streams with Char token (Alexander Krotov)
    + Rewrite `nonspaceChar` using `noneOf` (Alexander Krotov)

  * Text.Pandoc.Shared: Reimplement `mapLeft` using `Bifunctor.first`
    (Alexander Krotov).

  * Text.Pandoc.Pretty: Simplify `Text.Pandoc.Pretty.offset`
    (Alexander Krotov).

  * Text.Pandoc.App

    + Work around HXT limitation for --syntax-definition with windows
      drive (#4836).
    + Always preserve tabs for man format. We need it for tables.
    + Split command line parsing code into a separate unexported
      module, Text.Pandoc.App.CommandLineOptions (Albert Krewinkel).

  * Text.Pandoc.Readers.Roff: new unexported module for tokenizing
    roff documents.

  * New unexported module Text.Pandoc.RoffChar, provided character
    escape tables for roff formats.

  * Text.Pandoc.Readers.HTML: Fix `htmlTag` and `isInlineTag` to
    accept processing instructions (#3123, regression since 2.0).

  * Text.Pandoc.Readers.JATS: Use `foldl'` instead of `maximum` to
    account for empty lists (Alexander Krotov).

  * Text.Pandoc.Readers.RST: Don't allow single-dash separator in
    headerless table (#4382).

  * Text.Pandoc.Readers.Org: Parse empty argument array in inline src
    blocks (Brian Leung).

  * Text.Pandoc.Readers.Vimwiki:

    + Get rid of `F`, `runF` and `stateMeta'` in favor of `stateMeta`
      (Alexander Krotov).
    + Parse `Text` without converting to `[Char]` (Alexander Krotov).

  * Text.Pandoc.Readers.Creole: Parse `Text` without converting to
    `[Char]` (Alexander Krotov).

  * Text.Pandoc.Readers.LaTeX

    + Allow space at end of math after `\` (#5010).
    + Add support for `nolinkurl` command (#4992, Brian Leung).
    + Simplified type on `doMacros'`.
    + Tokenize before pulling tokens, rather than after (#4408). This
      has some performance penalty but is more reliable.
    + Make macroDef polymorphic and allow in inline context.
      Otherwise we can't parse something like `\lowercase{\def\x{Foo}}`.
     I have actually seen tex like this in the wild.
    + Improved parsing of `\def`, `\let`. We now correctly parse:
      ```
      \def\bar{hello}
      \let\fooi\bar
      \def\fooii{\bar}
      \fooi +\fooii

      \def\bar{goodbye}
      \fooi +\fooii
      ```
    + Improve parsing of `\def` argspec.
    + Skip `\PackageError` commands (see #4408).
    + Fix bugs omitting raw tex (#4527). The default is `-raw_tex`,
      so no raw tex should result unless we explicitly say `+raw_tex`.
      Previously some raw commands did make it through.
    + Moved `isArgTok` to Text.Pandoc.Readers.LaTeX.Parsing.
    + Moved `babelLangToBCP`, `polyglossiaLangToBCP` to new module,
      Text.Pandoc.Readers.LaTeX.Lang (unexported).
    + Simplified accent code using unicode-transforms.
      New dependency on unicode-transforms package for normalization.
    + Allow verbatim blocks ending with blank lines (#4624).
    + Support `breq` math environments: `dmath`, `dgroup`, `darray`.
      This collects some of the general-purpose code from the LaTeX reader,
      with the aim of making the module smaller.

  * Text.Pandoc.Readers.Markdown

    + Fix awkward soft break movements before abbreviations (#4635).
    + Add updateStrPos in a couple places where needed.

  * Text.Pandoc.Readers.Docx: Trigger bold/italic with bCs, iCs
    (#4947). These are variants for "complex scripts" like Arabic
    and are now treated just like b, i (bold, italic).

  * Text.Pandoc.Readers.Muse (Alexander Krotov)

    + Try to parse lists before trying to parse table.
      This ensures that tables inside lists are parsed correctly.
    + Forbid whitespace after opening and before closing markup
      elements.
    + Parse page breaks.
    + Simplify `museToPandocTable` to get rid of partial functions.
    + Allow footnotes to start with empty line.
    + Make sure that the whole text is parsed.
    + Allow empty headers. Previously empty headers caused parser to
      terminate without parsing the rest of the document.
    + Allow examples to be indented with tabs.
    + Remove indentation from examples indicated by `{{{` and `}}}`.
    + Fix parsing of empty cells.
    + Various changes to internals.
    + Rewrite some parsers in applicative style.
    + Avoid tagsoup dependency.
    + Allow table caption to contain `+`.

  * Text.Pandoc.Writers.LaTeX

    + Add newline if math ends in a comment (#4880). This prevents the
      closing delimiter from being swalled up in the comment.
    + With `--listings`, don't pass through org-babel attributes (#4889).
    + With `--biblatex`, use `\autocite` when possible (#4960).
      `\autocites{a1}{a2}{a3}` will not collapse the entries. So, if
      we don't have prefixes and suffixes, we use instead
      `\autocite{a1,a2,a3}`.
    + Fix description lists contining highlighted code (#4662).

  * Text.Pandoc.Writers.Man

    + Don't wrap `.SH` and `.SS` lines (#5019).
    + Avoid unnecessary `.RS`/`.RE` pair in definition lists with
      one paragraph definitions.
    + Moved common groff functions to Text.Pandoc.Writers.Groff.
    * Fix strong/code combination on man (should be `\f[CB]` not
      `\f[BC]`, see #4973).
    + Man writer: use `\f[R]` instead of `\f[]` to reset font
      (Alexander Krotov, #4973).
    + Move `splitSentences` to Text.Pandoc.Shared.

  * Text.Pandoc.Writers.Docx

    + Add framework for custom properties (#3034). So far, we don't
      actually write any custom properties, but we have the
      infrastructure to add this.

    + Handle tables in table cells (#4953). Although this is not
      documented in the spec, some versions of Word require a `w:p`
      element inside every table cell. Thus, we add one when the
      contents of a cell do not already include one (e.g. when a table
      cell contains a table).

  * Text.Pandoc.Writers.AsciiDoc:  Prevent illegal nestings.
    Adjust header levels so that n+1 level headers are only
    found under n level headers, and the top level is 1.

  * Text.Pandoc.Writers.OpenDocument: Improve bullet/numbering
    alignment (#4385). This change eliminates the large gap we used
    to have between bullet and text, and also ensures that numbers
    in numbered lists will be right-aligned.

  * Text.Pandoc.Writers.ZimWiki

    + Number ordered list items sequentially, rather than always
      with 1 (#4962).
    + Remove extra indentation on lists (#4963).

  * Text.Pandoc.Writers.EPUB: Use metadata field `css` instead of
    `stylesheet` (Mauro Bieg, #4990).

  * Text.Pandoc.Writers.Markdown: Ensure blank between raw block and
    normal content (#4629). Otherwise a raw block can prevent a
    paragraph from being recognized as such.

  * Text.Pandoc.Writers.Ms

    + Removed old `escapeBar`. We don't need this now that we use
      `@` for math delim.
    + Moved common code to Text.Pandoc.Writers.Roff and to
      Text.Pandoc.RoffChar.
    + Move `splitSentences` to Text.Pandoc.Shared (to avoid duplication
      with the man writer).

   * Text.Pandoc.Writers.Muse (Alexander Krotov).

    + Add support for grid tables.
    + Fix Muse writer style.
    + Use `length` instead of `realLength` to calculate definition
      indentation. Muse parsers don't take character width into
      account when calculating indentation.
    + Do not insert newline before lists.
    + Use lightweight markup after `</em>` tag.

  * New unexported module Text.Pandoc.Writers.Roff, providing functions
    useful for all roff format writers (man, ms).

  * Text.Pandoc.Lua

    + Move globals handling to separate module Text.Pandoc.Lua.Global
      (Albert Krewinkel).

    + Lua filter internals: push Shared.Element as userdata (Albert
      Krewinkel). Hierarchical Elements were pushed to Lua as plain
      tables. This is simple, but has the disadvantage that marshaling
      is eager: all child elements will be marshaled as part of the
      object. Using a Lua userdata object instead allows lazy access
      to fields, causing content marshaling just (but also each time)
      when a field is accessed. Filters which do not traverse the full
      element contents tree become faster as a result.

### Default template changes

  * LaTeX template:

    + Add variable `hyperrefoptions` (#4925, Mathias Walter).
    + Add variable `romanfont`, `romanfontoptions` (#4665, OvidiusCicero).

  * AsciiDoc template: use single-line style for title.

  * revealjs template: Fix typo in the socket.io javascript plugin (#5006,
    Yoan Blanc).

  * Text.Pandoc.Lua.Util: add missing docstring to `defineHowTo`
    (Albert Krewinkel).

  * data/pandoc.lua: add datatype ListAttributes (Albert Krewinkel)

  * data/sample.lua: replace custom pipe function with pandoc.utils.pipe
    (Albert Krewinkel).

### Documentation improvements

  * INSTALL.md

    + Add chromeos install instructions (#4958) (Evan Pratten).
    + Add note about TinyTeX.

  * MANUAL.txt

    + Change `groff` -> `roff`.
    + Implement `--ascii` for Markdown writer.
    + Clarify LaTeX image dimensions output (Mauro Bieg).

  * doc/customizing-pandoc.md: added skeleton (Mauro Bieg, #3288).

  * doc/getting-started.md: Added title to test1.md to avoid warning.

  * doc/lua-filters.md: merge type references into main document,
    fix description of Code.text (Albert Krewinkel).

### Build infrastructure improvements

  * Makefile

    + Makefile: added quick-cabal, full-cabal targets.
    + Make .msi download targets insensitive to order of appveyor builds.

  * Update benchmarks for ghc 8.6.1.

   * pandoc.cabal:

    + Enable more compiler warnings (Albert Krewinkel).
    + Make base lower bound 4.8.
    + Bump upper bound for QuickCheck.
    + Bump upper bound for binary.
    + Updated version bounds for containers and haddock-library (#4974).
    + Added docx/docPropos/custom.xml to cabal data-files.
    + Require skylighting 0.7.4 (#4920).
    + New dependency on unicode-transforms package for normalization.

  * Improved .travis.yml testing and test with GHC 8.6.1 (Albert Krewinkel).

  * Added `tools/changelog-helper.sh`.

  * Added test/grofftest.sh for testing the man reader on real man pages.

## pandoc 2.3.1 (2018-09-28)

  * RST reader:

    + Parse RST inlines containing newlines (#4912, Francesco Occhipinti).
      This eliminates a regression introduced after pandoc 2.1.1, which
      caused inline constructions containing newlines not to be recognized.
    + Fix bug with internal link targets (#4919).  They were gobbling up
      indented content underneath.

  * Markdown reader: distinguish autolinks in the AST.  With this change,
    autolinks are parsed as Links with the `uri` class. (The same is true
    for bare links, if the `autolink_bare_uris` extension is enabled.)
    Email autolinks are parsed as Links with the `email` class.  This
    allows the distinction to be represented in the AST.

  * Org reader:

    + Force inline code blocks to honor export options (Brian Leung).
    + Parse empty argument array in inline src blocks (Brian Leung).

  * Muse reader (Alexander Krotov):

    + Added additional tests.
    + Do not allow code markup to be followed by digit.
    + Remove heading level limit.
    + Simplify `<literal>` tag parsers
    + Parse Text instead of String. Benchmark shows 7% improvement.
    + Get rid of HTML parser dependency.
    + Various code improvements.

  * ConTeXt writer: change `\` to `/` in Windows image paths (#4918).
    We do this in the LaTeX writer, and it avoids problems.
    Note that `/` works as a LaTeX path separator on Windows.

  * LaTeX writer:

    + Add support for multiprenote and multipostnote arguments
      with `--biblatex` (Brian Leung, #4930).  The multiprenotes occur before
      the first prefix of a multicite, and the multipostnotes follow the
      last suffix.
    + Fix a use of `last` that might take empty list.  If you ran with
      `--biblatex` and have an empty document (metadata but no blocks),
      pandoc would previously raise an error because of the use of
      `last` on an empty list.

  * RTF writer: Fix build failure with ghc-8.6.1 caused by missing
    MonadFail instance (Jonas Scholl).

  * ODT Writer: Improve table header row style handling (Nils Carlson).
    This changes the way styles for cells in the header row
    and normal rows are handled in ODT tables.
    Previously a new (but identical) style was generated for
    every table, specifying the style of the cells within the table.
    After this change there are two style definitions for table cells,
    one for the cells in the header row, one for all other cells.
    This doesn't change the actual styles, but makes post-processing
    changes to the table styles much simpler as it is no longer
    necessary to introduce new styles for header rows and there are
    now only two styles where there was previously one per table.

  * HTML writer:

    + Don't add `uri` class to presumed autolinks.  Formerly the `uri`
      class was added to autolinks by the HTML writer, but it had to
      guess what was an autolink and could not distinguish
      `[http://example.com](http://example.com)` from
      `<http://example.com>`.  It also incorrectly recognized
      `[pandoc](pandoc)` as an autolink.  Now the HTML writer
      simply passes through the `uri` attribute if it is present,
      but does not add anything.
    + Avoid adding extra section nestings for revealjs.
      Previously revealjs title slides at level (slidelevel - 1)
      were nested under an extra section element, even when
      the section contained no additional (vertical) content.
      That caused problems for some transition effects.
    + Omit unknown attributes in EPUB2 output.  For example,
      `epub:type` attributes should not be passed through,
      or the epub produced will not validate.

  * JATS writer: remove 'role' attribute on 'bold' and 'sc' elements (#4937).
    The JATS spec does not allow these.

  * Textile writer: don't represent `uri` class explicitly
    for autolinks (#4913).

  * Lua filters (Albert Krewinkel):

    + Cleanup filter execution code.
    + Better error on test failure.

  * HTML, Muse reader tests: reduce time taken by round-trip test.

  * Added cabal.project.

  * MANUAL: `epub:type` is only useful for epub3 (Maura Bieg).

  * Use hslua v1.0.0 (Albert Krewinkel).

  * Fix `translations/ru` to use modern Russian orthography
    (Ivan Trubach).

  * Build Windows binary using ghc 8.6.1 and cabal new-build. This
    fixes issues with segfaults in the 32-bit Windows binaries (#4283).

## pandoc 2.3 (2018-09-16)

  * Add `--metadata-file` option (Mauro Bieg, #1960), which allows
    users to specify metadata in a YAML file, regardless of the
    input format (#1960).

  * Text.Pandoc.Writers.Shared: export `isDisplayMath` (API change).

  * Text.Pandoc.Readers.Markdown: export `yamlToMeta` (API change,
    Mauro Bieg).

  * Text.Pandoc.Readers.LaTeX.Types:

    + New type `ArgSpec` (API change).
    + Second parameter of `Macro` constructor is now `[ArgSpec]` instead of
      `Int` (API change).

  * Markdown reader:

    + Use `tex` instead of `latex` for raw tex-ish content. We can't
      always tell if it's LaTeX, ConTeXt, or plain TeX. Better just to
      use `tex` always. Note that if `context` or `latex` specifically
      is desired, you can still force that in a markdown document by
      using the raw attribute. Note that this change may affect some
      filters, if they assume that raw tex parsed by the Markdown reader
      will be RawBlock (Format `latex`). In most cases it should be
      trivial to modify the filters to accept `tex` as well.
    + Refactor and reorganize YAML code (Mauro Bieg).
    + Make `example_lists` work for interrupted lists
      without `startnum` (#4908).

  * HTML reader:

    + Parse `<script type="math/tex` tags as math (#4877).
      These are used by MathJax in some configurations.
    + Extract spaces inside links instead of trimming them
      (Alexander Krotov, #4845).
    + Added round-trip tests (Alexander Krotov).
    + Make parsing sensitive to the `raw_tex` extension (#1126). This now
      allows raw LaTeX environments, `\ref`, and `\eqref` to be parsed
      (which is helpful for translation HTML documents using MathJaX).

  * Org reader (Albert Krewinkel):

    + Respect export option `p` for planning info.  Inclusion of planning info
      (`*DEADLINE*`, `*SCHEDULED*`, and `*CLOSED*`) can be controlled via the
      `p` export option: setting the option to `t` will add all planning
      information in a *Plain* block below the respective headline.
    + Org reader internals: don't rely on RecordWildCards and ViewPatterns
      ghc extensions.
    + Strip planning info from output.  Planning info is parsed, but not
      included in the output (as is the default with Emacs Org-mode, #4867).

  * LaTeX reader:

    + Handle parameter patterns for `\def` (#4768, #4771).
      For example:  `\def\foo#1[#2]{#1 and #2}`.
    + Allow `%` characters in URLs. This affects `\href` and `\url` (#4832).
    + Fixed parsing of `\texorpdfstring`. We were returning the wrong
      argument as the content.
    + Support `blockcquote`, `foreignblockquote`, `foreigncblockquote`,
      `hyphenblockquote`, `hyphencblockquote`, `enquote*`, `foreignquote`,
      `hyphenquote` from `csquotes` (#4848). Note that `foreignquote`
      will be parsed as a regular Quoted inline (not using the quotes
      appropriate to the foreign language).
    + Support more text-mode accents (#4652). Add support for `\|`, `\b`, `\G`,
      `\h`, `\d`, `\f`, `\r`, `\t`, `\U`, `\i`, `\j`, `\newtie`,
      `\textcircled`. Also fall back to combining characters when composed
      characters are not available.
    + Resolve `\ref` for figure numbers.
    + Support `mintinline` (#4365, Marc Schreiber).
    + Fix siunitx unit commands so they are only recognized in
      siunitx contexts (#4842). For example, `\l` outside of an
      siunitx context should be l-slash, not l (for liter).
    + Fix double `unnumbered` class (#4838). The `unnumbered` class
      was being included twice for starred sections.

  * RST reader: Don't skip link definitions after comments (#4860).

  * Muse reader (Alexander Krotov):

    + Close the `</quote>` in indented tag test.
      There is already a separate test for unclosed `</quote>`.
    + Autonumber sections in the correct order.  Parsing now stops
      at each section header to ensure the header is registered before
      parsing of the next section starts.
    + Move duplicate code into `headingStart` function.
    + Allow newline after opening `*` or `**`.
    + Don't allow digits after closing marker in lightweight markup
      This change makes reader more compatible with Emacs Muse.
    + Parse `<verse>` tag in one pass instead of using
      `parseFromString`. This change makes it possible to have
      verbatim `</verse>` tag inside verse.

  * ODT reader:  deal gracefully with missing `<office:font-face-decls/>`
    (#4336). This allows pandoc to parse ODT document produced by KDE's
    Calligra.

  * Muse writer (Alexander Krotov):

    + Output headers without asterisks if not on the top level.
    + Never wrap definition list terms.
    + Set `envInsideBlock = True` when rendering notes.
    + Use `""` instead of `[]` for empty String.
    + Check for whitespace in the beginning and end of Str's.
    + Escape `-`, `;` and `>` in the beginning of strings.
    + Escape list markers in the beginning of notes.
    + Normalize inline list before testing if tags should be used.
    + Use tags instead of lightweight markup for empty strings.
    + Use lightweight markup when possible.
    + Escape empty strings.  This guarantees that `conditionalEscapeString`
      never returns empty string.
    + Wrap conditionalEscapeString result into `Muse` type.
      This removes the need to pass `envInsideLinkDescription` to it.
    + Separate `shouldEscapeString` function.
    + Simplify inline list rendering.
    + Replace newlines in strings with spaces.

  * Docx writer:

    + Add MetaString case for abstract, subtitle (#4900, Mauro Bieg).
    + Properly handle display math in spans (#4826). This isn't a
      complete solution, since other nestings of display math may still
      cause problems, but it should work for what is by far the most
      common case.

  * HTML writer:

    + Always output `<dt>` element, even if it is empty (#4883,
      Alexander Krotov).
    + Don't prefix `epub:` attributes with `data-`.

  * Org writer: Don't escape literal `_`, `^` (#4882). Org doesn't
    recognize these escapes.

  * ODT writer: Fix percentage image scaling (#4881, Nils Carlson).
    Image scaling was broken when a width was set to a percentage.

  * EPUB writer: set `epub:type` on body element in each chapter,
    depending on the `epub:type` of the first section (#4823). This
    only affects epub3. See
    http://www.idpf.org/epub/profiles/edu/structure/#h.l0bzsloklt10

  * FB2 writer: put `coverpage` element between title and date rather than in
    `document-info` element (#4854).

  * Markdown writer: Escape `~` if strikeout extension enabled (#4840).

  * Haddock writer: Use proper format for latex math in haddock (#4571, Joe
    Hermaszewski). Inline math in `\(..\)`, display math in `\[..\]`,
    tex is now used. Previously we'd "fake it with unicode" and fall
    back to tex when that didn't work. But newer haddock versions
    support latex math.

  * TEI writer:

    + Ensure that title element is always present, even if empty (#4839).
    + Put author tags in the template, rather than adding them in
      the writer (#4839).

  * LaTeX writer/template: be sensitive to `filecolor` variable (#4822).
    `linkcolor` only affects internal links, and `urlcolor` only
    affects linked URLs.  For external links, the option to use is
    `filecolor`.

  * ConTeXt writer: output raw `tex` blocks as well as `context` (#969).

  * RST writer:

    + Use `.. raw:: latex` for `tex` content.
    + Use `.. container` for generic Divs, instead of raw HTML.
    + Render Divs with admonition classes as admonitions (#4833).
      Also omit Div with class `admonition-title`. These are generated
      by the RST reader and should be omitted on round-trip.

  * Text.Pandoc.PDF: fix message printed when rsvg-convert is not available
    (#4855, Antonio Terceiro).

  * HTML5 template: add the `title-block-header` identifier to the
    `header` element, to make it easier to style precisely (#4767,
    J. B. Rainsberger).

  * OpenDocument template: Remove unnecessary indenting of TOC title (#4798,
    José de Mattos Neto).

  * latex template: Add support for $toc-title$ to LaTeX (and PDF)
    (#4853, Wandmalfarbe).

  * TEI template: improve `publicationStmt`. Add support for
    `publisher`, `address`, `pubPlace`, and `date` variables.

  * beamer template: Support "toc-title" (#4835, Cyril Roelandt).

  * Text.Pandoc.Extensions: Fix haddock on `Ext_footnotes` (Chris Martin).

  * Lua: cleanup Lua utils, remove unused functions (Albert Krewinkel).

  * MANUAL.txt:

    + Clarify that `--biblatex/--natbib` don't work directly for PDF
      (#4904).
    + Document `epub:type` attribute (Mauro Bieg, #4901)
    + Clarify when `--resource-path` has an effect.
    + More detail on customization in syntax highlighting section.
    + Document encoding issue with `--listings` (#4871, Damien Clochard).
    + Remove docs on removed `--katex-stylesheet` (Mauro Bieg, #4862).
    + Use https for context wiki links (#4910).

  * CONTRIBUTING.md:

    + Link to lua-filters repository (#4874).
    + Fix mistake in REPL instructions for stack. (#4849, Brian Leung).

  * lua-filters.md: add links to filters, and to lua-filters repository
    (#4874).

  * INSTALL.md:

    + Indicate that cabal >= 2.0 is needed.
    + Added chocolatey installation method (#4844, Miodrag Milić).

  * Travis: exclude round-trip tests, except for nightly test which can fail.

  * Use latest texmath, pandoc-citeproc.

  * Use a patched version of foundation until
    https://github.com/haskell-foundation/foundation/pull/503
    is fixed.

  * Clean up appveyor build and Windows package creation.
    We now use 64-bit stack and ghc 8.4.3, lts-12 for the 64-bit build. The
    WiX-based msi is now 64-bit for 64-bit builds (fixing #4795).

  * Remove obsolete RELEASE-CHECKLIST.md.

  * Added additional compiler warnings in Makefile and CI builds.

## pandoc 2.2.3.2 (2018-08-07)

  * Markdown reader: Properly handle boolean values in YAML metadata (#4819).
    This fixes a regression in 2.2.3, which cause boolean values to
    be parsed as MetaInlines instead of MetaBool.

    We here record another undocumented (but desirable) change in 2.2.3:
    numerical metadata fields are now parsed as MetaInlines rather than
    MetaString.

## pandoc 2.2.3.1 (2018-08-06)

  * Markdown reader: Fix parsing of embedded mappings in YAML metadata
    (#4817).  This fixes a regression in 2.2.3 which caused embedded
    mappings (e.g. mappings in sequences) not to work in YAML metadata.

## pandoc 2.2.3 (2018-08-05)

  * RST reader: improve parsing of inline interpreted text roles (#4811).

    + Use a Span with class "title-reference" for the default
      title-reference role.
    + Use `B.text` to split up contents into `Space`s, `SoftBreak`s,
      and `Str`s for `title-reference`.
    + Use Code with class "interpreted-text" instead of Span and Str for
      unknown roles.  (The RST writer has also been modified to round-trip
      this properly.)
    + Disallow blank lines in interpreted text.
    + Backslash-escape now works in interpreted text.
    + Backticks followed by alphanumerics no longer end interpreted text.
    + Remove support for nested inlines (Francesco Occhipinti).
      RST does not allow nested emphasis, links, or other inline
      constructs.  This fixes several bugs (#4581, #4561, #4792).

  * Org reader: fix parsers relying on `parseFromString` (#4784, Albert
    Krewinkel).  Emphasis was not parsed when it followed directly after
    some block types (e.g., lists).

  * Markdown reader: Allow unquoted numbers and booleans as YAML mapping
    keys.  Previously in 2.2.2 you could not do
    ```
    ---
    0: bar
    ...
    ```
    but only
    ```
    ---
    '0': bar
    ...
    ```
    With this change, both forms work.

  * DocBook reader: metadata handling improvements.
    Now we properly parse title and subtitle elements that are direct
    children of book and article (as well as children of bookinfo,
    articleinfo, or info).  We also now use the `subtitle` metadata
    field for subtitles, rather than tacking the subtitle on to the
    title.

  * RST writer:

    + Allow images to be directly nested within links (#4810, Francesco
      Occhipinti).
    + Use `titleblock` instead of `title` variable for title block (#4803,
      Francesco Occhipinti).  `titleblock` contains a properly formatted
      title and subtitle (using top-level headers).  `title` and
      `subtitle` variables are still available and just contain the
      title and subtitle text.  Note that this change will require an
      update to custom rst templates.
    + Render Code with class "interpreted-text" as interpreted text role.

  * MediaWiki writer: Avoid extra blank line in tables with empty cells
    (#4794).  Note that the old output is semantically identical, but the
    new output looks better.

  * Lua Utils module: add function `blocks_to_inlines` (#4799, Albert
    Krewinkel).  Exposes a function converting which flattenes a list of
    blocks into a list of inlines. An example use case would be the
    conversion of Note elements into other inlines.

  * RST template:  use `titleblock` instead of `title`.  Users of
    custom RST templates will want to update this.

  * LaTeX template: Moved some beamer code in default.latex template.
    This change allows beamer themes to change the template and font (as
    Metropolis does) (#4450).

  * Better error message on `-t pdf -o out.pdf` (#1155, Mauro Bieg).

  * Added test case for #4669 to repository.

  * INSTALL.md: Fix broken link for cabal-install (#4806, ChanHoHo).

  * MANUAL.txt:

    + Add beamer info for slide backgrounds (#4802, John Muccigrosso).
    + Clarify when `csquotes` is used in LaTeX writer (#4514).
    + Add `commonmark` to list of output formats where `raw_tex` has an
      effect (see #4527).


## pandoc 2.2.2.1 (2018-07-19)

  * Fix regression finding templates in user data directory (#4777).
    Under version 2.2.1 and prior pandoc found latex templates in the
    templates directory under the data directory, but this broke in 2.2.2.

  * Fix for bug in parsing `\input` in `rawLaTeXBlock` and
    `rawLaTeXInline` (#4781). (This primarily affects the markdown
    reader, and other readers that accept raw tex.)
    Starting in 2.2.2, everything after an `\input` (or `\include`)
    in a markdown file would be parsed as raw LaTeX.

  * MANUAL:

    + Clarify `gfm` vs `markdown_github` (#4783, Mauro Bieg).
    * Use `keywords` instead of `tags` in YAML metadata example (#4779).
      Unlike `tags`, `keywords` is used in some of the writers
      and default templates.

  * Add missing `rollingLinks` option to revealjs template (#4778,
    Igor Khorlo).

## pandoc 2.2.2 (2018-07-16)

  * Use HsYAML instead of yaml for translations and YAML metadata (#4747).
    yaml wraps a C library; HsYAML is pure Haskell.  Advances #4535.

    Note: HsYAML implements YAML 1.2, in which the valid true
    values are `true`, `True`, `TRUE`.  This means a change in
    the semantics of YAML metadata that could affect users:
    `y`, `yes`, and `on` no longer count as true values.

  * Fix regression: make `--pdf-engine` work with full paths (#4681, Mauro
    Bieg).

  * CommonMark reader: Handle `ascii_identifiers` extension (#4742,
    Anders Waldenborg).  Non-ascii characters were not stripped from
    identifiers even if the `ascii_identifiers` extension was
    enabled (which is is by default for gfm).

  * TikiWiki reader: Improve list parsing (#4722, Mauro Bieg).
    Remove trailing Space from list items.  Parse lists that have no space
    after marker.

  * LaTeX reader:

    + Treat `lilypond` as a verbatim environment (#4725).
    + Parse figure label into Image id (#4700, Mauro Bieg).
    + Beamer: Allow "noframenumbering" option (#4696, Raymond Ehlers).
    + Allow spaces around `\graphicspath` arguments (#4698).
    + Handle includes without surrounding blanklines (#4553).
      In addition, `\input` can now be used in an inline context,
      e.g. to provide part of a paragraph, as it can in LaTeX.
    + In `rawLaTeXBlock`, handle macros that resolve to a
      `\begin` or `\end` (#4667).
    + In `rawLaTeXBlock`, don't expand macros in macro definitions (#4653).
      Note that this only affected LaTeX in markdown.
    + Tighten up reading of beamer overlay specifications (#4669).
      Ideally we'd turn these on only when reading beamer, but currently
      beamer is not distinguished from latex as an input format.
      This commit also activates parsing of overlay specifications
      after commands in general (e.g. `\item`), since they can occur
      in many contexts in beamer.
    + Parse more siunitx unit commands (#4296, #4773).
    + Be more forgiving in key/value option parsing (#4761).

  * Markdown reader:

    + Allow empty code spans, e.g. `` ` ` ``.
    + Emojis are now wrapped in Spans with class `emoji` and
      attribute `data-emoji` (Anders Waldenborg, #4743).
      This allows the writer to handle them in a special way
      (e.g. using a special font, or just rendering the
      emoji name).

  * Muse reader (Alexander Krotov, except where indicated):

    + Get rid of non-exhaustive pattern match warning (Mauro Bieg).
    + Add support for floating images.
    + Add support for images with specified width.
    + Parse image URLs without "guard" and "takeExtension".
    + Split link and image parsing into separate functions.
    + Parse links starting with "URL:" explicitly instead of trying to strip
      "URL:" prefix after parsing.

  * Texinfo writer: Use `@sup` and `@sub` instead of custom macros (#4728,
    Alexander Krotov).

  * Markdown writer: Preserve `implicit_figures` with attributes, even if
    `implicit_attributes` is not set, by rendering in raw HTML (#4677).

  * Markdown and commonmark/github writers now respect the `emoji`
    extension.  So, `-f markdown+emoji -t markdown+emoji` now leaves
    `:smile:` as `:smile:` rather than converting it to a smile
    character.

  * Docx writer: Be sensitive to `toc` in YAML metadata (#4645).

  * ODT/OpenDocument writer: Make internal links work (#4358).
    This adds proper bookmarks to the headers with non-null IDs.

  * EPUB writer: Properly escape pagetitle.  Previously we weren't
    escaping `&` and other XML characters in the pagetitle, so a title
    containing a `&` would be invalid.

  * AsciiDoc Writer: Eescape square brackets at start of line (#4545,
    Mauro Bieg).

  * RST writer:

    + Don't treat 'example' as a syntax name (#4748).
      This fixes conversions from org with example blocks.
    + Support `--number-sections` via the `section-numbering`
      directive in standalone output.

  * reveal.js writer and template: reuse mathjax URL
    provided by the argument to `--mathjax` or the normal pandoc default,
    rather than a hard-coded one in the template (#4701).

  * LaTeX writer:

    + Properly handle footnotes in table captions (#4683).
      Refactored code from figure captions to use in both places.
    + In beamer output, fix single digit column percentage (#4690, Mauro
      Bieg).

  * FB2 writer (Alexander Krotov):

    + Convert Plain to Para in annotation (#2424).
    + Fix order of items in title-info (#2424).

  * Custom writer: fix error message on script failure (Albert Krewinkel).
    Error messages produced by Lua were not displayed by Pandoc.

  * Text.Pandoc.Emoji now exports `emojiToInline`, which returns a Span
    inline containing the emoji character and some attributes with metadata
    (class `emoji`, attribute `data-emoji` with emoji name).  (API change,
    Anders Waldenborg, #4743).

  * Text.Pandoc.PDF:

    + Revert fix for #4484 (only compress images on last run, #4755).
      This will mean some increase in the time it takes to
      produce an image-heavy PDF with xelatex, but it will
      make tables of contents correct, which is more important.
    + Fix logic error in `runTeXProgram`.  We were running the tex program
      one more time than requested.  This should speed up PDF production.

  * Allow `--template` to take a URL as argument.

  * Text.Pandoc.Highlighting: Add missing re-export of `breezeDark`
    highlighting style (#4687, Adrian Sieber, API change).

  * Clarify macOS install in INSTALL.md (#4661).  Make the binary package
    installer the recommended method, and note that on some older versions of
    macOS, homebrew installs from source and takes a lot of disk space
    (#4664, Ian).

  * MANUAL:

    + Clarify EPUB linked media (#4756, Mauro Bieg)
    + Update manual for "true" YAML values.  Now that we're using HsYAML and
      YAML 1.2, the valid true values are `true`, `True`, `TRUE`.  NOTE!
      `y`, `yes`, `on` no longer count as true values.
    + Document `-F` as alias for `--filter` (thanks to Gandalf Saxe).
    + Update manual on how math is rendered in LaTeX.
    + Add proxy description (#4131, Mauro Bieg).
    + Clarify that `--toc` requires `--standalone` (#4703).
    + Update citation styles link (#4699, wiefling).

  * In API docs, clarify how `Ext_east_asian_line_breaks` extension works
    (kaizshang91).  Note that it will not take effect when readers/writers
    are called as libraries (#4674).

  * Improved translations/fr (#4766, lux-lth).

  * Removed inadvertently added `.orig` files from repository (#4648).

  * Remove `network-uri` flag and use 'Network.Socket'.
    This removes a compiler warning.  There is no need for the old
    `network-uri` flag, since network 2.6 was released in 2014.

  * Add stack.lts10.yaml, stack.lts11.yaml.  use lts-12 in stack.yaml.

  * Bump upper bounds for dependent packages.

  * Exclude foundation 0.0.21 for ghc 7.10.  Otherwise cabal gets
    confused because of the way ghc 7.10 is excluded in foundation's
    cabal file.  This can be removed when haskell-foundation/foundation#500
    is fixed.

  * Require cabal-version >= 2.0.  This is needed for haddock-library.




## pandoc 2.2.1 (2018-05-10)

  * Restored and undeprecated gladtex for HTML math (#4607).

    + Added `GladTeX` constructor to `Text.Pandoc.Options.HTMLMathMethod`
      [API change, reverts removal in v2.2]
    + Restored and undeprecated `--gladtex` option, removed in v2.2.

  * LaTeX reader:  Handle `$` in `/text{..}` inside math (#4576).

  * Org reader: Fix image filename recognition (Albert Krewinkel).
    Use a function from the filepath library to check whether a string is a
    valid file name.  The custom validity checker that was used before gave
    wrong results (e.g. for absolute file paths on Windows,
    kawabata/ox-pandoc#52).

  * FB2 reader: Replace some errors with warnings (Alexander Krotov).

  * HTML writer:

    + Strip links from headers when creating TOC (#4340).
      Otherwise the TOC entries will not link to the sections.
    + Fix regression with tex math environments in HTML + MathJax (#4639).

  * Muse writer (Alexander Krotov): Add support for left-align and
    right-align classes (#4542).

  * Docx writer: Support underline (#4633).

  * Text.Pandoc.Parsing: Lookahead for non-whitespace after
    `singleQuoteStart` and `doubleQuoteStart` (#4637).

  * `test-pandoc-utils.lua`:  more robust testing on both windows
    and \*nix. Previously the pipe tests were only run if
    `\bin/false` and `/bin/sed` were present, which they aren't
    in default MacOS and Windows systems.  Fixed by using `tr`
    and `false`, which should always be in the path on a \*nix
    system, and `find` and `echo` for Windows.

  * Text.Pandoc.Shared: add `uriPathToPath`.
    This adjusts the path from a file: URI in a way that is sensitive
    to Windows/Linux differences.  Thus, on Windows,
    `/c:/foo` gets interpreted as `c:/foo`, but on Linux,
    `/c:/foo` gets interpreted as `/c:/foo`.  See #4613.

  * Use `uriPathToPath` with file: URIs (#4613).

  * Revert piping HTML to pdf-engine (Mauro Bieg, #4413).  Use a temp
    file as before.

  * Text.Pandoc.Class: Catch IO errors when writing media files
    and issue a warning, rather than an error (Francesco Occhipinti, #4559).

  * Don't lowercase custom writer filename (Alexander Krotov, #4610).

  * MANUAL (Mauro Bieg):

    + Clarify truthiness in template variables (#2281).
    + Clarify pipe table width calculation (#4520).

  * ConTeXt template: New Greek fallback typeface (Pablo Rodríguez, #4405).
    CMU Serif gives better typographic results than the previous
    Greek fallback DejaVu Serif.

  * Make HTML template polyglot (#4606, OvidiusCicero), by making
    `<link rel="stylesheet" href="$css$">` self-closing.

  * Use texmath 0.11, allowing better translation of non-ASCII
    characters in math (#4642).


## pandoc 2.2 (2018-04-27)

  * New input format: `fb2` (FictionBook2) (Alexander Krotov).

  * Make `--ascii` work for all XML formats (ICML, OPML, JATS,...),
    and for `ms` and `man`.

  * Remove deprecated `--latexmathml`, `--gladtex`, `--mimetex`, `--jsmath`, `-m`,
    `--asciimathml` options.

  * New module Text.Pandoc.Readers.FB2, exporting readFB2 (Alexander
    Krotov, API change).

  * Markdown reader:

    + Allow empty key-value attributes, like `title=""` (#2944).
    + Handle table w/o following blank line in fenced div (#4560).
    + Remove "fallback" for `doubleQuote` parser.  Previously the
      parser tried to be efficient -- if no end double quote was found,
      it would just return the contents.  But this could backfire in a
      case `**this should "be bold**`, since the fallback would return
      the content `"be bold**` and the closing boldface delimiter
      would never be encountered.
    + Improve computation of the relative width of the last column in a
      multiline table, so we can round-trip tables without constantly
      shrinking the last column.

  * EPUB reader:

    + Fix images with space in file path (#4344).

  * LaTeX reader:

    + Properly resolve section numbers with `\ref` and chapters (#4529).
    + Parse sloppypar environment (#4517, Marc Schreiber).
    + Improve handling of raw LaTeX (for markdown etc.) (#4589, #4594).
      Previously there were some bugs in how macros were handled.
    + Support `\MakeUppercase`, `\MakeLowercase`, `\uppercase`,
      `\lowercase`, and also `\MakeTextUppercase` and
      `\MakeTextLowercase` from textcase (#4959).

  * Textile reader:

    + Fixed tables with no body rows (#4513).
      Previously these raised an exception.

  * Mediawiki reader:

    + Improve table parsing (#4508).  This fixes detection of table
      attributes and also handles `!` characters in cells.

  * DocBook reader:

    + Properly handle title in `section` element (#4526).
      Previously we just got `section_title` for `section` (though `sect1`,
      `sect2`, etc. were handled properly).
    + Read tex math as output by asciidoctor (#4569, Joe Hermaszewski).

  * Docx reader:

    + Combine adjacent CodeBlocks with the same attributes into
      a single CodeBlock.  This prevents a multiline codeblock in
      Word from being read as different paragraphs.

  * RST reader:

    + Allow < 3 spaces indent under directives (#4579).
    + Fix anonymous redirects with backticks (#4598).

  * Muse reader (Alexander Krotov):

    + Add support for Text::Amuse multiline headings.
    + Add `<math>` tag support.
    + Add support for `<biblio>` and `<play>` tags.
    + Allow links to have empty descriptions.
    + Require block `<literal>` tags to be on separate lines.
    + Allow `-` in anchors.
    + Allow verse to be indented.
    + Allow nested footnotes.
    + Internal improvements.

  * Muse writer (Alexander Krotov):

    + Escape `>` only at the beginning of a line.
    + Escape `]` in image title.
    + Escape `]` brackets in URLs as `%5D`.
    + Only escape brackets when necessary.
    + Escape ordered list markers.
    + Do not escape list markers unless preceded by space.
    + Escape strings starting with space.
    + Escape semicolons and markers after line break.
    + Escape `;` to avoid accidental comments.
    + Don't break headers, line blocks and tables with line breaks.
    + Correctly output empty headings.
    + Escape horizontal rule only if at the beginning of the line.
    + Escape definition list terms starting with list markers.
    + Place header IDs before header.
    + Improve span writing.
    + Do not join Spans in normalization.
    + Don't align ordered list items.
    + Remove key-value pairs from attributes before normalization.
    + Enable `--wrap=preserve` for all tests by default.
    + Reduced `<verbatim>` tags in output.
    + Internal changes.

  * RST writer:

    + Use more consistent indentation (#4563).  Previously we
      used an odd mix of 3- and 4-space indentation.  Now we use 3-space
      indentation, except for ordered lists, where indentation must
      depend on the width of the list marker.
    + Flatten nested inlines (#4368, Francesco Occhipinti).
      Nested inlines are not valid RST syntax, so we flatten them following
      some readability criteria discussed in #4368.

  * EPUB writer:

    + Ensure that `pagetitle` is always set, even when structured titles
      are used.  This prevents spurious warnings about empty title
      elements (#4486).

  * FB2 writer (Alexander Krotov):

    + Output links inline instead of producing notes.  Previously all links
      were turned into footnotes with unclickable URLs inside.
    + Allow emphasis and notes in titles.
    + Don't intersperse paragraph with empty lines.
    + Convert metadata value `abstract` to book annotation.
    + Use `<empty-line />` for `HorizontalRule` rather than `LineBreak`.
      FB2 does not have a way to represent line breaks inside paragraphs;
      previously we used `<empty-line />` elements, but these are not allowed
      inside paragraphs.

  * Powerpoint writer (Jesse Rosenthal):

    + Handle Quoted Inlines (#4532).
    + Simplify code with `ParseXml`.
    + Allow fallback options when looking for placeholder type.
    + Check reference-doc for all layouts.
    + Simplify speaker notes logic.
    + Change notes state to a simpler per-slide value.
    + Remove `Maybe` from `SpeakerNotes` in `Slide`. `mempty`
      means no speaker notes.
    + Add tests for improved speaker notes.
    + Handle speaker notes earlier in the conversion process.
    + Keep notes with related blocks (#4477).  Some blocks automatically
      split slides (imgs, tables, `column` divs). We assume that any
      speaker notes immediately following these are connected to these
      elements, and keep them with the related blocks, splitting after them.
    + Remove `docProps/thumbnail.jpeg` in data dir (Jesse Rosenthal, #4588).
      It contained a nonfree ICC color calibration profile and is not needed
      for production of a powerpoint document.

  * Markdown writer:

    + Include a blank line at the end of the row in a single-row multiline
      table, to prevent it from being interpreted as a simple table (#4578).

  * CommonMark writer:

    + Correctly ignore LaTeX raw blocks when `raw_tex` is not
      enabled (#4527, quasicomputational).

  * EPUB writer:

    + Add `epub:type="footnotes"` to notes section in EPUB3 (#4489).

  * LaTeX writer:

    + In beamer, don't use format specifier for default ordered lists
      (#4556).  This gives better results for styles that put ordered list
      markers in boxes or circles.
    + Update `\lstinline` delimiters (#4369, Tim Parenti).

  * Ms writer:

    + Use `\f[R]` rather than `\f[]` to reset font (#4552).
    + Use `\f[BI]` and `\f[CB]` in headers, instead of `\f[I]` and `\f[C]`,
      since the header font is automatically bold (#4552).
    + Use `\f[CB]` rather than `\f[BC]` for monospace bold (#4552).
    + Create pdf anchor for a Div with an identifier (#4515).
    + Escape `/` character in anchor ids (#4515).
    + Improve escaping for anchor ids: we now use _uNNN_ instead of uNNN
      to avoid ambiguity.

  * Man writer:

    + Don't escape U+2019 as `'` (#4550).

  * Text.Pandoc.Options:

    + Removed `JsMath`, `LaTeXMathML`, and `GladTeX` constructors from
    `Text.Pandoc.Options.HTMLMathMethod` [API change].

  * Text.Pandoc.Class:

    + `writeMedia`: unescape URI-escaping in file path.  This avoids
      writing things like `file%20one.png` to the file system.

  * Text.Pandoc.Parsing:

    + Fix `romanNumeral` parser (#4480).  We previously accepted 'DDC'
      as 1100.
    + `uri`: don't treat `*` characters at end as part of URI (#4561).

  * Text.Pandoc.MIME:

    + Use the alias `application/eps` for EPS (#2067).
      This will ensure that we retain the eps extension after reading the
      image into a mediabag and writing it again.

  * Text.Pandoc.PDF:

    + Use `withTempDir` in `html2pdf`.
    + With `xelatex`, don't compress images til the last run (#4484).
      This saves time for image-heavy documents.
    + Don't try to convert EPS files (#2067).  `pdflatex` converts them
      itself, and JuicyPixels can't do it.
    + For `pdflatex`, use a temp directory in the working directory.
      Otherwise we can have problems with the EPS conversion pdflatex
      tries to do, which can't operate on a file above the working
      directory without `--shell-escape`.

  * Changes to tests to accommodate changes in pandoc-types.
    In <https://github.com/jgm/pandoc-types/pull/36> we changed
    the table builder to pad cells.  This commit changes tests
    (and two readers) to accord with this behavior.

  * Set default extensions for `beamer` same as `latex`.

  * LaTeX template:

    + Add `beameroption` variable (#4359, Étienne Bersac).
    + Use `pgfpages` package; this is needed for notes on second
      screen in beamer (Étienne Bersac).
    + Add `background-image` variable (#4601, John Muccigrosso).

  * reveal.js template: Add `background-image` variable (#4600,
    John Muccigrosso).

  * ms template: Fix date.  Previously `.ND` was used, but this only
    works if you have a title page, which we don't.  Thanks to @teoric.

  * Removed pragmas for unused extensions (#4506, Anabra).

  * Fix bash completion for `--print-default-data-file` (#4549).
    Previously this looked in the filesystem, even if pandoc
    was compiled with `embed_data_files` (and sometimes it looked
    in a nonexistent build directory).  Now the bash completion
    script just includes a hard-coded list of data file names.

  * MANUAL:

    + Clarify template vs metadata variables (#4501, Mauro Bieg).
    + Fix raw content example (#4479, Mauro Bieg).
    + Specify that you use html for raw output in epub.
    + Add examples for raw docx blocks (#4472, Tristan Stenner).
      The documentation states that the target format name should match
      the output format, which isn't the case for `docx`/`openxml` and
      some others.
    + Don't say that `empty_paragraphs` affects markdown output (#4540).
    + Consolidate input/output format documentation (#4577, Mauro Bieg).

  * New README template. Take in/out formats from manual.

  * Fix example in lua-filters docs (#4459, HeirOfNorton).

  * Use the `-threaded` GHC flag when building benchmarks (#4587,
    Francesco Occhipinti).

  * Bump temporary upper bound to 1.4.

  * Use pandoc-citeproc 0.14.3.1.

  * Use texmath-0.10.1.2 (fixes escapes in math in ms, #4597).

  * Removed old lib directory.  This was used for something long ago,
    but plays no role now.

  * Removed unneeded data file `LaTeXMathML.js`.

  * Create 64- and 32-bit versions of Windows binary packages.

## pandoc 2.1.3 (2018-03-18)

  * Docx reader (Jesse Rosenthal):

    + Add tests for nested smart tags.
    + Parse nested smart tags.
    + Make unwrapSDT into a general `unwrap` function that can unwrap both
      nested SDT tags and smartTags. This makes the SmartTags constructor in
      the Docx type unnecessary, so we remove it (#4446).
    + Remove unused `docxWarnings` (Alexander Krotov).

  * RST reader: Allow unicode bullet characters (#4454).

  * Haddock reader:  Better table handling, using haddock-library's
    new table support, if compiled against a version that
    includes it.  Note that tables with col/rowspans will not
    translate well into Pandoc.

  * Muse reader (Alexander Krotov):

    + Various internal improvements.
    + Require closing tag to have the same indentation as opening.
    + Do not reparse blocks inside unclosed block tag (#4425).
    + Parse `<class>` tag (supported by Emacs Muse).
    + Do not produce empty Str element for unindented verse lines.

  * LaTeX reader:

    + Add support to parse unit string of `\SI` command (closes #4296,
      Marc Schreiber).

  * Haddock writer:  In the writer, we now render tables always as
    grid tables, since Haddock supports these.

  * DokuWiki writer: rewrite backSlashLineBreaks (#4445, Mauro Bieg).

  * Docx writer: Fixed formatting of `DefaultStyle` ordered lists in
    docx writer.  We want decimal for the top level, not lower roman.

  * RST writer:

    + Strip whitespace at beginning and ending of inline containers
      (#4327, Francesco Occhipinti).
    + Filter out empty inline containers (#4434).  There is nothing in
      RST that corresponds to e.g. `Emph []`, so we just ignore elements
      like this (Francesco Occhipinti).

  * Muse writer (Alexander Krotov):

    + Support spans with anchors.
    + Replace smallcaps with emphasis before normalization.
    + Output smallcaps as emphasis.
    + Expand Cite before list normalization.
    + Write empty inline lists as `<verbatim></verbatim>`.
    + Remove empty Str from the beginning of inline lists during normalization.
    + Escape "-" to avoid creating bullet lists.
    + Fix math expansion for more than one expression per paragraph.
    + Expand math before inline list normalization.

  * Dokuwiki writer: fix LineBreaks in Tables (#4313, Mauro Bieg).

  * Ms writer:

    + Asciify pdf anchors, since unicode anchors don't work (#4436).
      Internal links should be converted automatically, so this shouldn't
      affect users directly.
    + Don't escape hyphens as `\-`; that's for a minus sign (#4467).

  * Beamer writer: put hyperlink after `\begin{frame}` and not in the title
    (#4307).  If it's in the title, then we get a titlebar on slides with
    the `plain` attribute, when the id is non-null.  This fixes a regression
    in 2.0.

  * EPUB writer: Remove notes from TOC in nav.xhtml (#4453, Mauro Bieg).

  * JATS writer: Remove extraneous, significant whitespace (#4335,
    Nokome Bentley).

  * html2pdf: inject base tag with current working directory (#4413, Mauro
    Bieg).  This helps ensure that linked resources are included.

  * Add Semigroup instances for everything for which we defined a
    Monoid instance previously (API change):

    + Text.Pandoc.Class.FileTree.
    + Text.Pandoc.Translations.Translations.
    + Text.Pandoc.Extensions.Extensions.
    + Text.Pandoc.Readers.Odt.StyleReader.Styles.
    + Text.Pandoc.Pretty.Doc.
    + Text.Pandoc.MediaBag.MediaBag.

  * Add custom Prelude to give clean code for Monoid and Semigroup
    that works with ghc 7.10-8.4.  The custom Prelude (`prelude/Prelude`)
    is used for ghc versions < 8.4.  `NoImplicitPrelude` is used
    in all source files, and Prelude is explicitly imported
    (this is necessary for ghci to work properly with the custom prelude).

  * Text.Pandoc.Writers.Shared (Francesco Occhipinti):

    + Export `stripLeadingTrailingSpace`.
    + Don't wrap lines in grid tables when `--wrap=none` (#4320).
    + `gridTable`: Don't wrap lines in tables when `--wrap=none`.  Instead,
      expand cells, even if it results in cells that don't respect relative
      widths or surpass page column width.  This change affects RST,
      Markdown, and Haddock writers.

  * Raise error if someone tries to print docx, odt, etc. template (#4441).

  * LaTeX template: Provide `bidi` package's option using
    `\PassOptionsToPackage`  (#4357, Václav Haisman).  This avoid a
    clash when   `polyglossia` loads it first and then it is loaded again
    for XeLaTeX.

  * ConTeXt template: Added `pdfa` variable to generate PDF/A (#4294, Henri
    Menke).  Instructions on how to install the ICC profiles on ConTeXt
    standalone can be found in the wiki:
    <http://wiki.contextgarden.net/PDFX#ICC_profiles>.
    If the ICC profiles are not available the log will contain error
    messages.

  * Use latest pandoc-types, skylighting

  * Use latest pandoc-citeproc in binary package.

  * Bump upper bound for time, criterion, haddock-library, exceptions,
    http-types, aeson, haddock-library.

  * Bump upper bound tasty-quickcheck 0.10 (#4429, Felix Yan).

  * pandoc.cabal:  fix up other-extensions and language fields.
    Language is now consistently `Haskell2010`, and other-extensions
    is consistently `NoImplicitPrelude`. Everything else to be specified
    in the module header as needed.

  * Removed `old-locale` flag and Text.Pandoc.Compat.Time.
    This is no longer necessary since we no longer support ghc 7.8.

  * Make `weigh-pandoc` into a benchmark program.
    Remove `weigh-pandoc` flag.  `weigh-pandoc` is now built (and run)
    automatically when you build (and run) benchmarks.

  * MANUAL: add instructions for background images reveal.js (#4325, John
    Muccigrosso).

  * appveyor: use VS 2013 environment instead of VS 2015 for Windows builds.

## pandoc 2.1.2 (2018-03-02)

  * Markdown reader:

    + Fix parsing bug with nested fenced divs (#4281).  Previously we allowed
      "nonindent spaces" before the opening and closing `:::`, but this
      interfered with list parsing, so now we require the fences to be flush with
      the margin of the containing block.

  * Commonmark reader:

    + `raw_html` is now on by default.  It can be disabled explicitly
      using `-f commonmark-raw_html`.

  * Org reader (Albert Krewinkel):

    + Move citation tests to separate module.
    + Allow changing emphasis syntax (#4378).  The characters allowed before
      and after emphasis can be configured via `#+pandoc-emphasis-pre` and
      `#+pandoc-emphasis-post`, respectively. This allows to change which
      strings are recognized as emphasized text on a per-document or even
      per-paragraph basis.  Example:

          #+pandoc-emphasis-pre: "-\t ('\"{"
          #+pandoc-emphasis-post: "-\t\n .,:!?;'\")}["

  * LaTeX reader:

    + Fixed comments inside citations (#4374).
    + Fix regression in package options including underscore (#4424).
    + Make `--trace` work.
    + Fixed parsing of `tabular*` environment (#4279).

  * RST reader:

    + Fix regression in parsing of headers with trailing space (#4280).

  * Muse reader (Alexander Krotov):

    + Enable `<literal>` tags even if amuse extension is enabled.
      Amusewiki disables <literal> tags for security reasons.
      If user wants similar behavior in pandoc, RawBlocks and RawInlines
      can be removed or replaced with filters.
    + Remove space prefix from `<literal>` tag contents.
    + Do not consume whitespace while looking for closing end tag.
    + Convert alphabetical list markers to decimal in round-trip test.
      Alphabetical lists are an addition of Text::Amuse.
      They are not present in Emacs Muse and can be ambiguous
      when list starts with "i.", "c." etc.
    + Allow `<quote>` and other tags to be indented.
    + Allow single colon in definition list term.
    + Fix parsing of verse in lists.
    + Improved parsing efficiency.  Avoid `parseFromString`.
      Lists are parsed in linear instead of exponential time now.
    + Replace ParserState with MuseState.
    + Prioritize lists with roman numerals over alphabetical lists.
      This is to make sure "i." starts a roman numbered list,
      instead of a list with letter "i" (followed by "j", "k", ...").
    + Fix directive parsing.
    + Parse definition lists with multiple descriptions.
    + Parse next list item before parsing more item contents.
    + Fixed a bug: headers did not terminate lists.
    + Move indentation parsing from `definitionListItem` to `definitionList`.
    + Paragraph indentation does not indicate nested quote.
      Muse allows indentation to indicate quotation or alignment,
      but only on the top level, not within a <quote> or list.
    + Require that block tags are on separate lines.
      Text::Amuse already explicitly requires it anyway.
    + Fix matching of closing inline tags.
    + Various internal changes.
    + Fix parsing of nested definition lists.
    + Require only one space for nested definition list indentation.
    + Do not remove trailing whitespace from `<code>`.
    + Fix parsing of trailing whitespace.  Newline after whitespace now
      results in softbreak instead of space.

  * Docx reader (Jesse Rosenthal, except where noted):

    + Handle nested sdt tags (#4415).
    + Don't look up dependant run styles if `+styles` is enabled.
    + Move pandoc inline styling inside custom-style span.
    + Read custom styles (#1843).  This will read all paragraph and
      character classes as divs and spans, respectively. Dependent styles
      will still be resolved, but will be wrapped with appropriate style
      tags. It is controlled by the `+styles` extension (`-f docx+styles`).
      This can be used in conjunction with the `custom-style` feature in the
      docx writer for a pandoc-docx editing workflow. Users can convert from
      an input docx, reading the custom-styles, and then use that same input
      docx file as a reference-doc for producing an output docx file. Styles
      will be maintained across the conversion, even if pandoc doesn't
      understand them.
    + Small change to Fields hyperlink parser.  Previously, unquoted string
      required a space at the end of the line (and consumed it). Now we
      either take a space (and don't consume it), or end of input.
    + Pick table width from the longest row or header (Francesco Occhipinti,
      #4360).

  * Muse writer (Alexander Krotov):

    + Change verse markup: `> ` instead of `<verse>` tag.
    + Remove empty strings during inline normalization.
    + Don't indent nested definition lists.
    + Use unicode quotes for quoted text.
    + Write image width specified in percent in Text::Amuse mode.
    + Don't wrap displayMath into `<verse>`.
    + Escape nonbreaking space (`~~`).
    + Join code with different attributes during normalization.
    + Indent lists inside Div.
    + Support definitions with multiple descriptions.

  * Powerpoint writer (Jesse Rosenthal):

    + Use table styles This will use the default table style in the
      reference-doc file. As a result they will be easier when using
      in a template, and match the color scheme.
    + Remove empty slides.  Because of the way that slides were split, these
      could be accidentally produced by comments after images.  When animations
      are added, there will be a way to add an empty slide with either
      incremental lists or pauses.
    + Implement syntax highlighting.  Note that background colors can't
      be implemented in PowerPoint, so highlighting styles that require
      these will be incomplete.
    + New test framework for pptx.  We now compare the output of the
      Powerpoint writer with files that we know to (a) not be corrupt,
      and (b) to show the desired output behavior (details below).
    + Add `notesMaster` to `presentation.xml` if necessary.
    + Ignore links and (end)notes in speaker notes.
    + Output speaker notes.
    + Read speaker note templates conditionally.  If there are speaker
      notes in the presentation, we read in the notesMasters templates
      from the reference pptx file.
    + Fix deletion track changes (#4303, Jesse Rosenthal).

  * Markdown writer: properly escape @ to avoid capture as citation
    (#4366).

  * LaTeX writer:

    + Put hypertarget inside figure environment (#4388).
      This works around a problem with the endfloat package and
      makes pandoc's output compatible with it.
    + Fix image height with percentage (#4389).  This previously caused
      the image to be resized to a percentage of textwidth, rather than
      textheight.

  * ConTeXt writer (Henri Menke):

    + New section syntax and support `--section-divs` (#2609).
      `\section[my-header]{My Header}` ->
      `\section[title={My Header},reference={my-header}]`.
      The ConTeXt writer now supports the `--section-divs` option to
      write sections in the fenced style, with `\startsection` and
      `\stopsection`.
    + xtables: correct wrong usage of caption (Henri Menke).

  * Docx writer:

    + Fix image resizing with multiple images (#3930, Andrew Pritchard).
    + Use new golden framework (Jesse Rosenthal).
    + Make more deterministic to facilitate testing (Jesse Rosenthal).
      - `getUniqueId` now calls to the state to get an incremented digit,
        instead of calling to P.uniqueHash.
      - we always start the PRNG in mkNumbering/mkAbstractNum with the same
        seed (1848), so our randoms should be the same each time.
    + Fix ids in comment writing (Jesse Rosenthal).  Comments from
      `--track-changes=all` were producing corrupt docx, because the
      writer was trying to get id from the `(ID,_,_)` field of
      the attributes, and ignoring the "id" entry in the key-value pairs. We
      now check both.

  * Ms writer: Added papersize variable.

  * TEI writer:

    + Use `height` instead of `depth` for images (#4331).
    + Ensure that id prefix is always used.
    + Don't emit `role` attribute; that was a leftover from the
      Docbook writer.
    + Use 'xml:id', not 'id' attribute (#4371).

  * AsciiDoc writer:

    + Do not output implicit heading IDs (#4363, Alexander
      Krotov).  Convert to `asciidoc-auto_identifiers` for old behaviour.

  * RST writer:

    + Remove `blockToRST'` moving its logic into `fixBlocks`
      (Francesco Occhipinti).
    + Insert comment between lists and quotes (#4248, Francesco Occchipinti).

  * RST template: remove definition of 'math' role as raw.
    This used to be needed prior to v 0.8 of docutils, but
    now math support is built-in.

  * Slides: Use divs to set incremental/non-incremental (#4381,
    Jesse Rosenthal).  The old method (list inside blockquote) still
    works, but we are encouraging the use of divs with class
    `incremental` or `nonincremental`.

  * Text.Pandoc.ImageSize:

    + Make image size detection for PDFs more robust (#4322).
    + Determine image size for PDFs (#4322).
    + EMF Image size support (#4375, Andrew Pritchard).

  * Text.Pandoc.Extensions:

    + Add `Ext_styles` (Jesse Rosenthal, API change).  This will be used in
      the docx reader (defaulting to off) to read pargraph and character
      styles not understood by pandoc (as divs and spans, respectively).
    + Made `Ext_raw_html` default for `commonmark` format.

  * Text.Pandoc.Parsing:

    + Export `manyUntil` (Alexander Krotov, API change).
    + Export improved `sepBy1` (Alexander Krotov).
    + Export list marker parsers: `upperRoman`, `lowerRoman`,
      `decimal`, `lowerAlpha`, `upperAlpha` (Alexander Krotov, API change).

  * Tests/Lua: fix tests on windows (Albert Krewinkel).

  * Lua: register script name in global variable (#4393).  The name of the Lua
    script which is executed is made available in the global Lua variable
    `PANDOC_SCRIPT_FILE`, both for Lua filters and custom writers.

  * Tests: Abstract powerpoint tests out to OOXML tests (Jesse Rosenthal).
    There is very little pptx-specific in these tests, so we abstract out
    the basic testing function so it can be used for docx as well. This
    should allow us to catch some errors in the docx writer that slipped
    by the roundtrip testing.

  * Lua filters: store constructors in registry (Albert Krewinkel).  Lua
    functions used to construct AST element values are stored in the Lua
    registry for quicker access. Getting a value from the registry is much
    faster than getting a global value (partly to idiosyncrasies of hslua);
    this change results in a considerable performance boost.

  * Documentation:

    + `doc/org.md` Add draft of Org-mode documentation (Albert Krewinkel).
    + `doc/lua-filters.md`: document global vars set for filters
      (Albert Krewinkel).
    + INSTALL.md: mention Stack version. (#4343, Adam Brandizzi).
    + MANUAL: add documentation on custom styles (Jesse Rosenthal).
    + MANUAL.txt: Document incremental and nonincremental divs (Jesse
      Rosenthal).  Blockquoted lists are still described, but fenced divs are
      presented in preference.
    + MANUAL.txt: document header and footer variables (newmana).
    + MANUAL.txt: self-contained implies standalone (#4304, Daniel Lublin).
    + CONTRIBUTING.md: label was renamed. (#4310, Alexander Brandizzi).

  * Require tagsoup 0.14.3 (#4282), fixing HTML tokenization bug.

  * Use latest texmath.

  * Use latest pandoc-citeproc.

  * Allow exceptions 0.9.

  * Require aeson-pretty 0.8.5 (#4394).

  * Bump blaze-markup, blaze-html lower bounds to 0.8, 0.9 (#4334).

  * Update tagsoup to 0.14.6 (Alexander Krotov, #4282).

  * Removed ghc-prof-options.  As of cabal 1.24, sensible defaults are used.

  * Update default.nix to current nixpkgs-unstable for hslua-0.9.5 (#4348,
    jarlg).

## pandoc 2.1.1 (2018-01-18)

  * Markdown reader:

    + Don't coalesce adjacent raw LaTeX blocks if they are separated by a
      blank line.  See lierdakil/pandoc-crossref#160.
    + Improved `inlinesInBalancedBrackets` (#4272, jgm/pandoc-citeproc#315).
      The change both improves performance and fixes a regression whereby
      normal citations inside inline notes and figure captions were not
      parsed correctly.

  * RST reader:

    + Better handling for headers with an anchor (#4240).  Instead of creating a
      Div containing the header, we put the id directly on the header.
      This way header promotion will work properly.
    + Add aligned environment when needed in math (#4254).  `rst2latex.py`
      uses an `align*` environment for math in `.. math::` blocks, so this
      math may contain line breaks.  If it does, we put the math in an
      `aligned` environment to simulate `rst2latex.py`'s behavior.

  * HTML reader:

    + Fix col width parsing for percentages < 10% (#4262, n3fariox).

  * LaTeX reader:

    + Advance source position at end of stream.
    + Pass through macro defs in `rawLaTeXBlock` even if the `latex_macros`
      extension is set (#4246).  This reverts to earlier behavior and is
      probably safer on the whole, since some macros only modify things in
      included packages, which pandoc's macro expansion can't modify.
    + Fixed pos calculation in tokenizing escaped space.
    + Allow macro definitions inside macros (#4253).  Previously we went into
      an infinite loop with
      ```
      \newcommand{\noop}[1]{#1}
      \noop{\newcommand{\foo}[1]{#1}}
      \foo{hi}
      ```
    + Fix inconsistent column widths (#4238).  This fixes a bug whereby column
      widths for the body were different from widths for the header in some
      tables.

  * Docx reader (Jesse Rosenthal):

    + Parse hyperlinks in `instrText` tags (#3389, #4266).  This was a form of
      hyperlink found in older versions of word. The changes introduced for
      this, though, create a framework for parsing further fields in MS Word
      (see the spec, ECMA-376-1:2016, §17.16.5, for more on these fields).
      We introduce a new module, `Text.Pandoc.Readers.Docx.Fields` which
      contains a simple parsec parser. At the moment, only simple hyperlink
      fields are accepted, but that can be extended in the future.

  * Muse reader (Alexander Krotov):

    + Parse `~~` as non-breaking space in Text::Amuse mode.
    + Refactor list parsing.

  * Powerpoint writer (Jesse Rosenthal):

    + Change reference to `notesSlide` to `endNotesSlide`.
    + Move image sizing into `picProps`.
    + Improve table placement.
    + Make our own `_rels/.rels` file.
    + Import reference-doc images properly.
    + Move `Presentation.hs` out of `PandocMonad`.
    + Refactor into separate modules.  T.P.W.Powerpoint.Presentation
      defines the Presentation datatype and goes Pandoc->Presentation;
      T.P.W.Pandoc.Output goes Presentation->Archive.
      Text.Pandoc.Writers.Powerpoint a thin wrapper around the two modules.
    + Avoid overlapping blocks in column output.
    + Position images correctly in two-column layout.
    + Make content shape retrieval environment-aware.
    + Improve image handling.  We now determine image and caption placement
      by getting the dimensions of the content box in a given layout.
      This allows for images to be correctly sized and positioned in a
      different template.  Note that images without captions and headers are
      no longer full-screened. We can't do this dependably in different
      layouts, because we don't know where the header is (it could be to
      the side of the content, for example).
    + Read presentation size from reference file.  Our presentation size is
      now dependent on the reference/template file we use.
    + Handle (sub)headers above slidelevel correctly.  Above the slidelevel,
      subheaders will be printed in bold and given a bit of extra space
      before them. Note that at the moment, no distinction is made between
      levels of headers above the slide header, though that can be changed.
    + Check for required files.  Since we now import from reference/dist
      file by glob, we need to make sure that we're getting the files we
      need to make a non-corrupt Powerpoint. This performs that check.
    + Improve templating using `--reference-doc`.  Templating should work
      much more reliably now.
    + Include Notes slide in TOC.
    + Set notes slide header to slide-level.
    + Add table of contents.  This is triggered by the `--toc` flag. Note
      that in a long slide deck this risks overrunning the text box. The user
      can address this by setting `--toc-depth=1`.
    + Set notes slide number correctly.
    + Clean up adding metadata slide.  We want to count the slide numbers
      correctly if it's in there.
    + Add anchor links.  For anchor-type links (`[foo](#bar)`) we produce
      an anchor link. In powerpoint these are links to slides, so we keep
      track of a map relating anchors to the slides they occur on.
    + Make the slide number available to the blocks.  For anchors,
      block-processing functions need to know what slide number
      they're in. We make the `envCurSlideId` available to blocks.
    + Move `curSlideId` to environment.
    + Allow setting `toc-title` in metadata.
    + Link notes to endnotes slide.

  * Markdown writer:

    + Fix cell width calculation (#4265).  Previously we could get
      ever-lengthening cell widths when a table was run repeatedly through
      `pandoc -f markdown -t markdown`.

  * LaTeX writer:

    + Escape `&` in lstinline (Robert Schütz).

  * ConTeXt writer:

    + Use xtables instead of Tables (#4223, Henri Menke).
      Default to xtables for context output.  Natural Tables are used
      if the new `ntb` extension is set.

  * HTML writer:

     + Fixed footnote backlinks with `--id-prefix` (#4235).

  * `Text.Pandoc.Extensions`:  Added `Ext_ntb` constructor (API change,
    Henri Menke).

  * `Text.Pandoc.ImageSize`: add derived `Eq` instance to `Dimension`
    (Jesse Rosenthal, API change).

  * Lua filters (Albert Krewinkel):

    + Make `PANDOC_READER_OPTIONS` available.
      The options which were used to read the document are made available to
      Lua filters via the `PANDOC_READER_OPTIONS` global.
    + Add lua module `pandoc.utils.run_json_filter`, which runs a JSON filter
      on a Pandoc document.
    + Refactor filter-handling code into `Text.Pandoc.Filter.JSON`,
      `Text.Pandoc.Filter.Lua`, and `Text.Pandoc.Filter.Path`.
    + Improve error messages.  Provide more context about the task
      which caused an error.

  * data/pandoc.lua (Albert Krewinkel):

    + Accept singleton inline as a list.  Every constructor which accepts a
      list of inlines now also accepts a single inline element for
      convenience.
    + Accept single block as singleton list. Every constructor which accepts
      a list of blocks now also accepts a single block element for
      convenience.  Furthermore, strings are accepted as shorthand for
      `{pandoc.Str "text"}` in constructors.
    + Add attr, listAttributes accessors.  Elements with
      attributes got an additional `attr` accessor. Attributes were
      accessible only via the `identifier`, `classes`, and `attributes`,
      which was in conflict with the documentation, which indirectly states
      that such elements have the an `attr` property.
    + Drop `_VERSION`.  Having a `_VERSION` became superfluous, as this
      module is closely tied to the pandoc version, which is available via
      `PANDOC_VERSION`.
    + Fix access to Attr components.  Accessing an Attr value (e.g.,
      ` Attr().classes`) was broken; the more common case of accessing it via
      an Inline or Block element was unaffected by this.

  * Move `metaValueToInlines` to from Docx writer to
    `Text.Pandoc.Writers.Shared`, so it can be used by other writers
    (Jesse Rosenthal).

  * MANUAL.txt:

    + Clarify otherlangs in LaTeX (#4072).
    + Clarify `latex_macros` extension.
    + Recommend use of `raw_attribute` extension in header includes (#4253).

  * Allow latest QuickCheck, tasty, criterion.

  * Remove custom prelude and ghc 7.8 support.

  * Reduce compiler noise (exact paths for compiled modules).

## pandoc 2.1 (2018-01-07)

  * Allow filters and lua filters to be interspersed (#4196).  Previously
    we ran all lua filters before JSON filters.  Now we run filters in
    the order they are presented on the command line, whether lua or JSON.
    There are two incompatible API changes: The type of `applyFilters`
    has changed, and `applyLuaFilters` has been removed.  `Filter` is
    also now exported.

  * Use latest skylighting and omit the `missingIncludes` check, fixing
    a major performance regression in earlier releases of the 2.x series
    (#4226).  Behavior change: If you use a custom syntax definition that
    refers to a syntax you haven't loaded, pandoc will now complain when
    it is highlighting the text, rather than doing a check at the start.
    This change dramatically speeds up invocations of pandoc on short
    inputs.

  * Text.Pandoc.Class: make `FileTree` opaque (don't export
    `FileTree` constructor).  This forces users to interact with it using
    `insertInFileTree` and `getFileInfo`, which normalize file names.

  * Markdown reader:

    + Rewrite `inlinesInBalancedBrackets`.  The rewrite is much more
      direct, avoiding `parseFromString`.  And it performs significantly
      better; unfortunately, parsing time still increases exponentially
      (see #1735).
    + Avoid parsing raw tex unless `\` + letter seen.  This seems to
      help with the performance problem, #4216.

  * LaTeX reader: Simplified a check for raw tex command.

  * Muse reader (Alexander Krotov):

    + Enable round trip test (#4107).
    + Automatically translate `#cover` into `#cover-image`.
      Amusewiki uses #cover directive to specify cover image.

  * Docx reader (Jesse Rosenthal):

    + Allow for insertion/deletion of paragraphs (#3927).
      If the paragraph has a deleted or inserted paragraph break (depending
      on the track-changes setting) we hold onto it until the next
      paragraph. This takes care of accept and reject. For this we introduce
      a new state which holds the ils from the previous para if necessary.
      For `--track-changes=all`, we add an empty span with class
      `paragraph-insertion`/`paragraph-deletion` at the end of the paragraph
      prior to the break to be inserted or deleted.
    + Remove unused anchors (#3679).  Docx produces a lot of anchors with
      nothing pointing to them---we now remove these to produce cleaner
      output. Note that this has to occur at the end of the process
      because it has to follow link/anchor rewriting.
    + Read multiple children of `w:sdtContents`.
    + Combine adjacent anchors.  There isn't any reason to have numerous
      anchors in the same place, since we can't maintain docx's
      non-nesting overlapping. So we reduce to a single anchor.
    + Improved tests.

  * Muse writer (Alexander Krotov): don't escape URIs from AST

  * Docx writer:

    + Removed redundant subtitle in title (Sebastian Talmon).
    + `firstRow` table definition compatibility for Word 2016 (Sebastian
      Talmon).  Word 2016 seems to use a default value of "1" for table
      headers, if there is no firstRow definition (although a default
      value of 0 is documented), so all tables get the first Row formatted
      as header.  Setting the parameter to 0 if the table has no header
      row fixes this for Word 2016
    + Fix custom styles with spaces in the name (#3290).

  * Powerpoint writer (Jesse Rosenthal):

    + Ignore Notes div for parity with other slide outputs.
    + Set default slidelevel correctly.  We had previously defaulted to
      slideLevel 2. Now we use the correct behavior of defaulting to the
      highest level header followed by content. We change an expected test
      result to match this behavior.
    + Split blocks correctly for linked images.
    + Combine adjacent runs.
    + Make inline code inherit code size.  Previously (a) the code size
      wasn't set when we force size, and (b) the properties was set from
      the default, instead of inheriting.
    + Simplify `replaceNamedChildren` function.
    + Allow linked images.  The following markdown:
      `[![Image Title](image.jpg)](http://www.example.com)`
      will now produce a linked image in the resulting PowerPoint file.
    + Fix error with empty table cell.  We require an empty `<a:p>` tag,
      even if the cell contains no paragraphs---otherwise PowerPoint
      complains of corruption.
    + Implement two-column slides.  This uses the columns/column div
      format described in the pandoc manual. At the moment, only two
      columns (half the screen each) are allowed. Custom widths are not
      supported.
    + Added more tests.

  * OpenDocument/ODT writers: improved rendering of formulas (#4170, oltolm).

  * Lua filters (Albert Krewinkel):

    + `data/pandoc.lua`: drop 'pandoc-api-version' from Pandoc objects
    + The current pandoc-types version is made available to Lua programs in
      the global `PANDOC_API_VERSION`. It contains the version as a list of
      numbers.
    + The pandoc version available as a global `PANDOC_VERSION` (a list
      of numbers).
    + `data/pandoc.lua`: make `Attr` an `AstElement`.
    + `data/pandoc.lua`: make all types subtypes of `AstElement`.
      `Pandoc`, `Meta`, and `Citation` were just plain functions and did
      not set a metatable on the returned value, which made it difficult
      to amend objects of these types with new behavior. They are now
      subtypes of AstElement, meaning that all their objects can gain
      new features when a method is added to the behavior object
      (e.g., `pandoc.Pandoc.behavior`).
    + `data/pandoc.lua`: split type and behavior tables.  Clearly distinguish
      between a type and the behavioral properties of an instance of that
      type. The behavior of a type (and all its subtypes) can now be
      amended by adding methods to that types `behavior` object, without
      exposing the type objects internals.  E.g.:
      ```lua
      pandoc.Inline.behavior.frob = function () print'42' end
      local str = pandoc.Str'hello'
      str.frob() -- outputs '42'
      ```
    + `data/pandoc.lua`: fix Element inheritance.  Extending all elements
      of a given type (e.g., all inline elements) was difficult, as the
      table used to lookup unknown methods would be reset every time a
      new element of that type was created, preventing recursive property
      lookup. This is was changed in that all methods and attributes of
      supertypes are now available to their subtypes.
    + `data/pandoc.lua`: fix attribute names of Citation (#4222).  The
      fields were named like the Haskell fields, not like the documented,
      shorter version.  The names are changed to match the documentation
      and Citations are given a shared metatable to enable simple
      extensibility.
    + `data/pandoc.lua`: drop function `pandoc.global_filter`.
    + Bump `hslua` version to 0.9.5.  This version fixes a bug that made it
      difficult to handle failures while getting lists or a Map from Lua.
      A bug in pandoc, which made it necessary to always pass a tag when
      using MetaList or MetaBlock, is fixed as a result. Using the pandoc
      module's constructor functions for these values is now optional
      (if still recommended).
    + Stop exporting `pushPandocModule` (API change).  The introduction
      of `runPandocLua` renders direct use of this function obsolete.
    + Update generation of module docs for lua filters.
    + `Lua.Module.Utils`: make stringify work on `MetaValues` (John
      MacFarlane).  I'm sure this was intended in the first place,
      but currently only `Meta` is supported.

  * Improve benchmarks.

    + Set the default extensions properly.
    + Improve benchmark argument parsing.  You can now say
      `make bench BENCHARGS="markdown latex reader"` and both the
      markdown and latex readers will be benchmarked.

  * MANUAL.txt simplify and add more structure (Mauro Bieg).

  * Generate README.md from template and MANUAL.txt.
    `make README.md` will generate the README.md after changes
    to MANUAL.txt have been made.

  * Update copyright notices to include 2018 (Albert Krewinkel).

## pandoc 2.0.6 (2017-12-28)

  * Added `jats` as an input format.

    + Add Text.Pandoc.Readers.JATS, exporting `readJATS` (API
      change) (Hamish Mackenzie).
    + Improved citation handling in JATS reader.  JATS citations
      are now converted to pandoc citations, and JATS ref-lists
      are converted into a `references` field in metadata, suitable
      for use with pandoc-citeproc. Thus a JATS article with embedded
      bibliographic information can be processed with pandoc and
      pandoc-citeproc to produce a formatted bibliography.

  * Allow `--list-extensions` to take an optional FORMAT argument.
    This lists the extensions set by default for the selected FORMAT.
    The extensions are now alphabetized, and the `+` or `-`
    indicating the default setting comes before, rather than after,
    the extension.

  * Markdown reader:

    + Preserve original whitespace between blocks.
    + Recognize `\placeformula` as context.
    + Be pickier about table captions.  A caption starts with a `:` which
      can't be followed by punctuation.  Otherwise we can falsely interpret
      the start of a fenced div, or even a table header line like
      `:--:|:--:`, as a caption.
    + Always use four space rule for example lists.  It would be awkward
      to indent example list contents to the first non-space character after
      the label, since example list labels are often long.  Thanks to
      Bernhard Fisseni for the suggestion.
    + Improve raw tex parsing.  Note that the Markdown reader is also
      affected by the `latex_macros` extension changes described below
      under the LaTeX reader.

  * LaTeX reader:

    + `latex_macros` extension changes (#4179).  Don't pass through macro
      definitions themselves when `latex_macros` is set.  The macros
      have already been applied.  If `latex_macros` is enabled, then
      `rawLaTeXBlock` in Text.Pandoc.Readers.LaTeX will succeed in parsing
      a macro definition, and will update pandoc's internal macro map
      accordingly, but the empty string will be returned.
    + Export `tokenize`, `untokenize` (API change).
    + Use `applyMacros` in `rawLaTeXBlock`, `rawLaTeXInline`.
    + Refactored `inlineCommand`.
    + Fix bug in tokenizer.  Material following `^^` was
      dropped if it wasn't a character escape.  This only affected
      invalid LaTeX, so we didn't see it in the wild, but it appeared
      in a QuickCheck test failure.
    + Fix regression in LateX tokenization (#4159).  This mainly affects the
      Markdown reader when parsing raw LaTeX with escaped spaces.
    + Add tests of LaTeX tokenizer.
    + Support `\foreignlanguage` from babel.
    + Be more tolerant of `&` character (#4208).  This allows us to parse
      unknown tabular environments as raw LaTeX.

  * Muse reader (Alexander Krotov):

    + Parse anchors immediately after headings as IDs.
    + Require that note references does not start with 0.
    + Parse empty comments correctly.

  * Org reader (Albert Krewinkel):

    + Fix asterisks-related parsing error (#4180).
    + Support minlevel option for includes (#4154).  The level of headers
      in included files can be shifted to a higher level by specifying a
      minimum header level via the `:minlevel` parameter. E.g.
      `#+include: "tour.org" :minlevel 1` will shift the headers in
      tour.org such that the topmost headers become level 1 headers.
    + Break-up org reader test file into multiple modules.

  * OPML reader:

    + Enable raw HTML and other extensions by default for notes
      (#4164).  This fixes a regression in 2.0.  Note that extensions can
      now be individually disabled, e.g.  `-f opml-smart-raw_html`.

  * RST reader:

    + Allow empty list items (#4193).
    + More accurate parsing of references (#4156).  Previously we erroneously
      included the enclosing backticks in a reference ID (#4156).  This
      change also disables interpretation of syntax inside references, as
      in docutils.  So, there is no emphasis in `` `my *link*`_ ``.

  * Docx reader:

    + Continue lists after interruption (#4025, Jesse Rosenthal).
      Docx expects that lists will continue where they left off after an
      interruption and introduces a new id if a list is starting again. So
      we keep track of the state of lists and use them to define a "start"
      attribute, if necessary.
    + Add tests for structured document tags unwrapping (Jesse Rosenthal).
    + Preprocess Document body to unwrap `w:sdt` elements (Jesse Rosenthal,
      #4190).

  * Plain writer:

    + Don't linkify table of contents.

  * RST writer:

    + Fix anchors for headers (#4188).  We were missing an `_`.

  * PowerPoint writer (Jesse Rosenthal):

    + Treat lists inside BlockQuotes as lists.  We don't yet produce
      incremental lists in PowerPoint, but we should at least treat lists
      inside BlockQuotes as lists, for compatibility with other slide formats.
    + Add ability to force size.  This replaces the more specific
      `blockQuote runProp`, which only affected the size of blockquotes. We
      can use this for notes, etc.
    + Implement notes.  This currently prints all notes on a final slide.
      Note that at the moment, there is a danger of text overflowing the
      note slide, since there is no logic for adding further slides.
    + Implement basic definition list functionality to PowerPoint writer.
    + Don't look for default template file for Powerpoint (#4181).
    + Add pptx to isTextFormat list.  This is used to check standalone
      and not writing to the terminal.
    + Obey slide level option (Jesse Rosenthal).
    + Introduce tests.

  * Docx writer:

    + Ensure that `distArchive` is the one that comes with pandoc
      (#4182).  Previously a `reference.docx` in `~/.pandoc` (or the user data
      dir) would be used instead, and this could cause problems because a
      user-modified docx sometimes lacks vital sections that we count
      on the `distArchive` to supply.

  * Org writer:

    + Do not wrap "-" to avoid accidental bullet lists (Alexander Krotov).
    + Don't allow fn refs to wrap to beginning of line (#4171, with help from
      Alexander Krotov).  Otherwise they can be interpreted as footnote
      definitions.

  * Muse writer (Alexander Krotov):

    + Don't wrap note references to the next line (#4172).

  * HTML writer:

    + Use br elements in line blocks instead of relying on CSS
      (#4162).  HTML-based templates have had the custom CSS for
      `div.line-block` removed.  Those maintaining custom templates will want
      to remove this too.  We still enclose line blocks in a div with class
      `line-block`.

  * LaTeX writer:

    + Use `\renewcommand` for `\textlatin` with babel (#4161).
      This avoids a clash with a deprecated `\textlatin` command defined
      in Babel.
    + Allow fragile=singleslide attribute in beamer slides (#4169).
    + Use `\endhead` after `\toprule` in headerless tables (#4207).

  * FB2 writer:

    + Add cover image specified by `cover-image` meta (Alexander Krotov,
      #4195).

  * JATS writer (Hamish Mackenzie):

    + Support writing `<fig>` and `<table-wrap>` elements
      with `<title>` and `<caption>` inside them by using Divs with class set
      to one of `fig`, `table-wrap` or `caption` (Hamish Mackenzie).  The
      title is included as a Heading so the constraint on where Heading can
      occur is also relaxed.
    + Leave out empty alt attributes on links.
    + Deduplicate image mime type code.
    + Make `<p>` optional in `<td>` and `<th>` (#4178).
    + Self closing tags for empty xref (#4187).
    + Improve support for code language.

  * Custom writer:

    + Use init file to setup Lua interpreter (Albert Krewinkel).
      The same init file (`data/init`) that is used to setup the Lua
      interpreter for Lua filters is also used to setup the interpreter of
      custom writers.lua.
    + Define instances for newtype wrapper (Albert Krewinkel).  The custom
      writer used its own `ToLuaStack` instance definitions, which made
      it difficult to share code with Lua filters, as this could result
      in conflicting instances.  A `Stringify` wrapper is introduced to
      avoid this problem.
    + Added tests for custom writer.
    + Fixed definition lists and tables in `data/sample.lua`.

  * Fixed regression: when target is PDF, writer extensions were being
    ignored.  So, for example, `pandoc -t latex-smart -o file.pdf`
    did not work properly.

  * Lua modules (Albert Krewinkel):

    + Add `pandoc.utils` module, to hold utility functions.
    + Create a Haskell module Text.Pandoc.Lua.Module.Pandoc to
      define the `pandoc` lua module.
    + Make a Haskell module for each Lua module. Move definitions for the
      `pandoc.mediabag` modules to a separate Haskell module.
    + Move `sha1` from the main `pandoc` module to `pandoc.utils`.
    + Add function `pandoc.utils.hierarchicalize` (convert list of
      Pandoc blocks into (hierarchical) list of Elements).
    + Add function `pandoc.utils.normalize_date` (parses a date and
      converts it (if possible) to "YYYY-MM-DD" format).
    + Add function `pandoc.utils.to_roman_numeral` (allows conversion
      of numbers below 4000 into roman numerals).
    + Add function `pandoc.utils.stringify` (converts any AST element
      to a string with formatting removed).
    + `data/init.lua`: load `pandoc.utils` by default
    + Turn pipe, read into full Haskell functions.  The `pipe` and `read`
      utility functions are converted from hybrid lua/haskell functions
      into full Haskell functions. This avoids the need for intermediate
      `_pipe`/`_read` helper functions, which have dropped.
    + pandoc.lua: re-add missing MetaMap function.  This was a bug
      introduced in version 2.0.4.

  * Text.Pandoc.Class: Add `insertInFileTree` [API change].  This gives
    a pure way to insert an ersatz file into a `FileTree`.  In addition, we
    normalize paths both on insertion and on lookup.

  * Text.Pandoc.Shared: export `blocksToInlines'` (API change, Maura Bieg).

  * Text.Pandoc.MIME: Add opus to MIME type table as audio/ogg (#4198).

  * Text.Pandoc.Extensions:   Alphabetical order constructors for
    `Extension`.  This makes them appear in order in `--list-extensions`.

  * Allow lenient decoding of latex error logs, which are not always
    properly UTF8-encoded (#4200).

  * Update latex template to work with recent versions of beamer.
    The old template produced numbered sections with some recent
    versions of beamer.  Thanks to Thomas Hodgson.

  * Updated reference.docx (#4175).  Instead of just "Hello, world", the
    document now contains exemplars of most of the styles that have an
    effect on pandoc documents.  This makes it easier to see the effect
    of style changes.

  * Removed `default.theme` data file (#4096).  It is no longer needed now
    that we have `--print-highlight-style`.

  * Added `stack.lts9.yaml` for building with lts 9 and ghc 8.0.2.
    We still need this for the alpine static linux build, since
    we don't have ghc 8.2.2 for that yet.

  * Removed `stack.pkg.yaml`.  We only really need `stack.yaml`; we
    can put flag settings for pandoc-citeproc there.

  * Makefile: Add 'trypandoc' and 'pandoc-templates' targets to
    make releases easier.

  * MANUAL.txt:

    + Add note on what formats have `+smart` by default.
    + Use native syntax for custom-style (#4174, Mauro Bieg).
    + Introduce dedicated Extensions section, since some extensions
      affect formats other than markdown (Mauro Bieg, #4204).
    + Clarify default html output for `--section-divs` (Richard Edwards).

  * filters.md: say that Text.Pandoc.JSON comes form pandoc-types.
    Closes jgm/pandoc-website#16.

  * epub.md: Delete removed `-S` option from command (#4151, Georger Araújo).

## pandoc 2.0.5 (2017-12-12)

  * Fix a bug in 2.0.4, whereby pandoc could not read the theme files
    generated with `--print-highlight-style` (#4133).  Improve JSON
    serialization of styles.

  * Fix CSS issues involving line numbers (#4128).
    Highlighted code blocks are now enclosed in a div with class `sourceCode`.
    Highlighting CSS no longer sets a generic color for pre and code; we only
    set these for class `sourceCode`.

  * `--pdf-engine-opt`: fix bug where option order was reversed (#4137).

  * Add PowerPoint (pptx) writer (Jesse Rosenthal).
    It works following the standard Pandoc conventions for making other
    sorts of slides. Caveats:

    + Syntax highlighting is not yet implemented. (This is difficult
      because there are no character classes in Powerpoint.)
    + Footnotes and Definition lists are not yet implemented. (Notes will
      usually take the form of a final slide.
    + Image placement and auto-resizing has a few glitches.
    + Reference powerpoint files don't work dependably from the command
      line. This will be implemented, but at the moment users are advised
      to change themes from within Powerpoint.

  * Create shared Text.Pandoc.Writers.OOXML module (Jesse Rosenthal).
    This is for functions used by both Powerpoint and Docx writers.

  * Add default pptx data for Powerpoint writer (Jesse Rosenthal).

  * Add `empty_paragraphs` extension.

    + Deprecate `--strip-empty-paragraphs` option.  Instead we now
      use an `empty_paragraphs` extension that can be enabled on
      the reader or writer.  By default, disabled.
    + Add `Ext_empty_paragraphs` constructor to `Extension`.
    + Revert "Docx reader: don't strip out empty paragraphs."
      This reverts commit d6c58eb836f033a48955796de4d9ffb3b30e297b.
    + Implement `empty_paragraphs` extension in docx reader and writer,
      opendocument writer, html reader and writer.
    + Add tests for `empty_paragraphs` extension.

  * Markdown reader:

    + Don't parse native div as table caption (#4119).
    + Improved computation of column widths in pipe tables.
      Pipe tables with lines longer than the text width (as set
      by `--columns`) are now scaled to text width, with the relative
      widths of columns determined by the ratios between the
      header lines.  Previously we computed column widths using
      the ratio of header line lengths to column width, so that
      tables with narrow header lines were extremely thin, which
      was very rarely the desired result.

  * LaTeX reader: fix `\` before newline (#4134).  This should be a space,
    as long as it's not followed by a blank line. This has been fixed at the
    tokenizer level.

  * Muse reader (Alexander Krotov):

    + Add test for `#disable-tables` directive in Emacs mode.
    + Don't allow emphasis to be preceded by letter.
    + Add underline support in Emacs Muse mode..
    + Support multiline directives in Amusewiki mode

  * Man writer: omit internal links (#4136).  That is, just print the link
    text without the URL.

  * Markdown reader: accept processing instructions as raw HTML (#4125).

  * Lua filters (Albert Krewinkel):

    + Use script to initialize the interpreter.  The file `init.lua` is
      used to initialize the Lua interpreter which is used in Lua filters.
      This gives users the option to require libraries which they want to
      use in all of their filters, and to extend default modules.
    + Fix package loading for Lua 5.1.  The list of package searchers is
      named `package.loaders` in Lua 5.1 and LuaJIT, and `package.searchers`
      in Lua 5.2 and later.
    + Refactor lua module handling.  The integration with Lua's package/module
      system is improved: A pandoc-specific package searcher is prepended to
      the searchers in `package.searchers`. The modules `pandoc` and
      `pandoc.mediabag` can now be loaded via `require`.
    + Bump lower bound of hslua.  The release hslua 0.9.3 contains a new
      function which makes using Haskell functions as package loaders much
      easier.

  * reveal.js template:  add title-slide identifier to title slide (#4120).
    This allows it to be styled more easily.

  * LaTeX template: Added support for `pagestyle` variable (#4135,
    Thomas Hodgson)

  * Add `-threaded` to ghc-options for executable (#4130, fixes a build
    error on linux).


## pandoc 2.0.4 (2017-12-03)

  * Add `--print-highlight-style` option.  This generates a JSON version
    of a highlighting style, which can be saved as a `.theme` file, modified,
    and used with `--highlight-style` (#4106, #4096).

  * Add `--strip-empty-paragraphs` option.  This works for any input format.
    It is primarily intended for use with docx and odt documents where
    empty paragraphs have been used for inter-paragraph spaces.

  * Support `--webtex` for `gfm` output.

  * Recognize `.muse` file extension.

  * Support beamer `\alert` in LaTeX reader. Closes #4091.

  * Docx reader: don't strip out empty paragraphs (#2252).
    Users who have a conversion pipeline from docx may want to consider adding
    `--strip-empty-paragraphs` to the command line.

  * Org reader (Albert Krewinkel): Allow empty list items (#4090).

  * Muse reader (Alexander Krotov):

    + Parse markup in definition list terms.
    + Allow definition to end with EOF.
    + Make code blocks round trip.
    + Drop common space prefix from list items.
    + Add partial round trip test.
    + Don't interpret XML entities.
    + Remove `nested`.
    + Parse `~~` as non-breaking space in Emacs mode.
    + Correctly remove indentation from notes.  Exactly one space is
      required and considered to be part of the marker.
    + Allow list items to be empty.
    + Add ordered list test.
    + Add more multiline definition tests.
    + Don't allow blockquotes within lists.
    + Fix reading of multiline definitions.
    + Add inline `<literal>` support.
    + Concatenate inlines of the same type

  * Docx writer: allow empty paragraphs (#2252).

  * CommonMark/gfm writer:

    + Use raw html for native divs/spans (#4113).  This allows a pandoc
      markdown native div or span to be rendered in gfm using raw html tags.
    + Implement `raw_html` and `raw_tex` extensions.  Note that `raw_html`
      is enabled by default for `gfm`, while `raw_tex` is disabled by default.

  * Muse writer (Alexander Krotov):

    + Test that inline math conversion result is normalized.
      Without normalization this test produced
      `<em>a</em><em>b</em><em>c</em>`.
    + Improve inline list normalization and move to writer.
    + Escape hash symbol.
    + Escape `----` to avoid accidental horizontal rules.
    + Escape only `</code>` inside code tag.
    + Additional `<verbatim>` is not needed as `<code>` is verbatim already.

  * LaTeX writer:

    + Allow specifying just width or height for image size.
      Previously both needed to be specified (unless the image was
      being resized to be smaller than its original size).
      If height but not width is specified, we now set width to
      textwidth. If width but not height is specified, we now set
      height to textheight.  Since we have `keepaspectratio`, this
      yields the desired result.
    + Escape `~` and `_` in code with `--listings` (#4111).

  * HTML writer: export `tagWithAttributes`.  This is a helper allowing
    other writers to create single HTML tags.

  * Let papersizes `a0`, `a1`, `a2`, ... be case-insensitive by
    converting the case as needed in LaTeX and ConTeXt writers.

  * Change `fixDisplayMath` from `Text.Pandoc.Writers.Shared`
    so that it no longer produces empty `Para`'s as an artifact.

  * `Text.Pandoc.Shared.blocksToInlines`:  rewrote using builder.
    This gives us automatic normalization, so we don't get
    for example two consecutive Spaces.

  * Include default CSS for 'underline' class in HTML-based templates.

  * revealjs template:  add `tex2jax` configuration for the
    math plugin.  With the next release of reveal.js, this will
    fix the problem of `$`s outside of math contexts being
    interpreted as math delimiters (#4027).

  * `pandoc.lua` module for use in lua filters (Albert Krewinkel):

    + Add basic lua List module (#4099, #4081).  The List module is
      automatically loaded, but not assigned to a global variable. It can be
      included in filters by calling `List = require 'List'`.  Lists of blocks,
      lists of inlines, and lists of classes are now given `List` as a metatable,
      making working with them more convenient.  E.g., it is now possible to
      concatenate lists of inlines using Lua's concatenation operator `..`
      (requires at least one of the operants to have `List` as a metatable):

          function Emph (emph)
            local s = {pandoc.Space(), pandoc.Str 'emphasized'}
            return pandoc.Span(emph.content .. s)
          end

      The `List` metatable is assigned to the tables which get passed to
      the constructors `MetaBlocks`, `MetaInline`, and `MetaList`. This
      enables the use of the resulting objects as lists.
    + `Lua/StackInstances`: push Pandoc and Meta via constructor.
      Pandoc and Meta elements are now pushed by calling the respective
      constructor functions of the pandoc Lua module. This makes serialization
      consistent with the way blocks and inlines are pushed to lua and allows
      to use List methods with the `blocks` value.
    + Add documentation for pandoc.List in `lua-filters.md`.

  * Use latest tagsoup.  This fixes a bug in parsing HTML tags with
    `&` (but not a valid entity) following them (#4094, #4088).

  * Use skylighting 0.4.4.1, fixing the color of unmarked code text
    when `numberLines` is used (#4103).

  * Make `normalizeDate` more forgiving (Mauro Bieg, #4101), not
    requiring a leading 0 on single-digit days.

  * Fix `--help` output for `--highlight-style` to include `FILE` (Mauro
    Bieg, #4095).

  * Clearer deprecation warning for `--latexmathml, --asciimathml, -m`.
    Previously we only mentioned `--latexmathml`, even if `-m` was
    used.

  * Changelog: fix description of lua filters in 2.0 release
    (Albert Krewinkel).  Lua filters were initially run *after* conventional
    (JSON) filters.  However, this was changed later to make it easier to deal
    with files in the mediabag. The changelog is updated to describe that
    feature of the 2.0 release correctly.

  * Change Generic JSON instances to TemplateHaskell (Jasper Van der Jeugt,
    #4085).  This reduces compile time and memory usage significantly.

  * `lua-filters.md`: Added tikz filter example.

  * Create alternative zip file for macOS binaries.

  * Create alternative zip file for Windows binaries.

  * Update INSTALL.md since we now provide zips for binaries.

  * Relax `http-types` dependency (Justus Sagemüller, #4084).

  * Add `epub.md`, `getting-started.md` to docs.  These used to live in
    the website repo.

  * Add `packages` target to Makefile.

  * Bump bounds for binary, http-types, tasty-hunit

## pandoc 2.0.3 (2017-11-20)

  * Lua filters: preload text module (Albert Krewinkel, #4077).
    The `text` module is preloaded in lua. The module contains some UTF-8
    aware string functions, implemented in Haskell.  The module is loaded on
    request only, e.g.:

        text = require 'text'
        function Str (s)
          s.text = text.upper(s.text)
          return s
        end

  * Allow table-like access to attributes in lua filters (Albert Krewinkel,
    #4071).  Attribute lists are represented as associative lists in Lua. Pure
    associative lists are awkward to work with. A metatable is attached to
    attribute lists, allowing to access and use the associative list as if
    the attributes were stored in as normal key-value pair in table.
    Note that this changes the way `pairs` works on attribute lists. Instead
    of producing integer keys and two-element tables, the resulting iterator
    function now returns the key and value of those pairs.  Use `ipairs` to
    get the old behavior.  Warning: the new iteration mechanism only works if
    pandoc has been compiled with Lua 5.2 or later (current default: 5.3).

  * Text.Pandoc.Parsing.uri:  allow `&` and `=` as word characters (#4068).
    This fixes a bug where pandoc would stop parsing a URI with an
    empty attribute:  for example, `&a=&b=` wolud stop at `a`.
    (The uri parser tries to guess which punctuation characters
    are part of the URI and which might be punctuation after it.)

  * Introduce `HasSyntaxExtensions` typeclass (Alexander Krotov, #4074).

    + Added new `HasSyntaxExtensions` typeclass for `ReaderOptions` and
      `WriterOptions`.
    + Reimplemented `isEnabled` function from `Options.hs` to accept both
      `ReaderOptions` and `WriterOptions`.
    + Replaced `enabled` from `CommonMark.hs` with new `isEnabled`.

  * Add `amuse` extension (Alexander Krotov) to enable Amuse wiki
    behavior for `muse`.  New `Ext_amuse` constructor for
    `Extension`. Note: this is switched on by default; for
    Emacs behavior, use `muse-amuse`.

  * Muse reader (Alexander Krotov):

    + Count only one space as part of list item marker.
    + Produce SoftBreaks on newlines. Now wrapping can be preserved
      with `--wrap=preserve`.
    + Add Text::Amuse footnote extensions.  Footnote end is indicated by
      indentation, so footnotes can be placed anywhere in the text,
      not just at the end of it.
    + Accept Emacs Muse definition lists when `-amuse`.
      Emacs Muse does not require indentation.

  * HTML reader:

    + Ensure we don't produce level 0 headers (#4076), even for chapter
      sections in epubs.  This causes problems because writers aren't set
      up to expect these.
    + Allow spaces after `\(` and before `\)` with `tex_math_single_backslash`.
      Previously `\( \frac{1}{a} < \frac{1}{b} \)` was not parsed as math in
      `markdown` or `html` `+tex_math_single_backslash`.

  * MANUAL: clarify that math extensions work with HTML.
    Clarify that `tex_math_dollars` and `tex_math_single_backslash`
    will work with HTML as well as Markdown.

  * Creole reader: Fix performance issue for longer lists (Sascha Wilde,
    #4067).

  * RST reader: better support for 'container' directive (#4066).
    Create a div, incorporate name attribute and classes.

  * LaTeX reader:

    + Support column specs like `*{2}{r}` (#4056).  This is equivalent to
      `rr`.  We now expand it like a macro.
    + Allow optional args for parbox (#4056).
    + Allow optional arguments on `\footnote` (#4062).

  * EPUB writer: Fixed path for cover image (#4069).  It was previously
    `media/media/imagename`, and should have been `media/imagename`.

  * Markdown writer: fix bug with doubled footnotes in grid tables
    (#4061).

  * LaTeX template:  include natbib/biblatex after polyglossia (#4073).
    Otherwise we seem to get an error; biblatex wants polyglossia
    language to be defined.

  * Added examples to lua filters documentation.


## pandoc 2.0.2 (2017-11-12)

  * Deprecated ancient HTML math methods: `--latexmathml`, `--gladtex`,
    `--mimetex`, `--jsmath`.

  * Fixed URIs in `data/jats.csl`.  They were being rendered twice,
    leading to invalid XML in default JATS output with pandoc-citeproc.

  * `lua-filters.md`: use real-world man page filter as example.

  * Add lua filter functions `walk_inline` and `walk_block`
    in the pandoc module, to apply filters inside particular
    inline and block elements.

  * Refactored some code from `Text.Pandoc.Lua.PandocModule`
    into new internal module `Text.Pandoc.Lua.Filter`.

  * Markdown reader:

    + Allow fenced code blocks to be indented 1-3 spaces (#4011).
      This brings our handling of them into alignment with CommonMark's.
    + Fix YAML metadata with "chomp" (`|-`).  Previously if a
      YAML block under `|-` contained a blank line, pandoc would
      not parse it as metadata.

  * Removed `etc.` from abbreviations file.  Often `etc.` ends a
    sentence, and we want the period to be treated as a
    sentence-ending period.

  * Fix regression with `--metadata` (#4054).  Values specified with
    `--metadata` should replace a metadata value set in the document
    itself, rather than creating a list including a new value.

  * EPUB writer:

    + Fix EPUB OCF structure.  #3720 had been improperly implemented.
    + Fix modified paths for raw HTML tags (src, poster, etc.)
      (#4050, #4055). This had not been updated for the new EPUB
      container layout, with a separate text/ subdirectory.
    + Fix image paths with empty `--epub-subdirectory`.

  * Miscellaneous code cleanup (Alexander Krotov).

  * Use pandoc-types 1.17.3, which adds `Walkable` instances
    for `[Block] Block` and `[Inline] Inline`.

  * Remove obsolete `stack.full.yaml` (#4052).

  * Change to using pandoc-citeproc 0.12.1 in binary packages.

  * Consolidate math output method documentation (#4049, Mauro Bieg).

  * `MANUAL.txt`: fix header level of "Extension: emoji" (Albert Krewinkel).

  * Use lua filter to generate man page from `MANUAL.txt`, replacing old
    Haskell filters.  This is easier and faster.

  * Improved `INSTALL.md`.

  * Update commands to extract deb archive on Linux (#4043, Salim B).


## pandoc 2.0.1.1 (2017-11-04)

  * Improved fix to #3989 (parsing of HTML tags containing
    `>` in an attribute or comment). The previous fix (in 2.0.1) only
    worked in certain cases.

  * FB2 writer (Alexander Krotov):

    + Add `unrecognised` genre to `<title-info>`
      (Alexander Krotov).  XML schema requires at least one genre.
    + Remove `<annotation>` from `<body>`.

  * CommonMark writer: fix strikethrough for `gfm` (#4038).

  * Use texmath 0.10, which adds support for a wider range of
    symbols and fixes default column alignments in MathML
    and OMML.

  * Highlighting fixes, using skylighting 0.4.3.2:

    + Fix invalid CSS.
    + Support `lineAnchors` (or `line-anchors`) in HTML code blocks.
    + Ensure that code lines don't get duplicate identifiers (#4031).
      The line identifiers are built using the code block's identifier
      as a prefix. If the code block has null identifier, we use
      `cb1`, `cb2`, etc.

  * Added a few abbreviations to `data/abbreviations`,
    and sorted the list (#3984, Wandmalfarbe).

  * Improved support for columns in HTML writer (#4028).

    + Remove `width` attribute from the `div`.
    + Remove space between `<div class="column">` elements,
      since this prevents columns whose widths sum to 100%
      (the space takes up space).
    + Move as much as possible of the CSS to the template.
    + Ensure that all the HTML-based templates (including epub)
      contain the CSS for columns.
    + Columns default to 50% width unless they are given a width
      attribute.  So if you want two equal-width columns, you
      can use a div with class `column` and no `width` attribute.

  * SelfContained: use `base64` for css links with media attribute (#4026).
    This fixes `--self-contained` with S5.

  * Improve `pandoc-template-mode.el` (Vaclav Haisman).

  * INSTALL.md: MacOS instructions needed xar -f (adam234).

  * MANUAL.txt:

    + Clarify that --setext-headers doesn't affect gfm output (#4035).
    + Clarify what is needed to open and close a div in `fenced_divs`
      (#4039, Tristano Ajmone).
    + Removed reference to `default.beamer` in docs (#4024).
      Also added mention of other templates affecting PDF output
      with different settings.


## pandoc 2.0.1 (2017-10-31)

  * Fixed regression in parsing of HTML comments in markdown and other
    non-HTML formats (`Text.Pandoc.Readers.HTML.htmlTag`) (#4019).
    The parser stopped at the first `>` character, even if it wasn't
    the end of the comment.

  * Creole reader (Sascha Wilde):

    + Fix some minor typos and formatting.
    + Add additional test on nowiki-block after para.
    + Fix lists with trailing white space.

  * LaTeX reader: handle `%` comment right after command.
    For example, `\emph%`.

  * Markdown reader:  make sure fenced div closers work in lists.
    Previously the following failed:

        ::: {.class}
        1. one
        2. two
        :::

    and you needed a blank line before the closing `:::`.

  * Make `fenced_divs` affect the Markdown writer.  If `fenced_divs` is
    enabled, Divs will be rendered as fenced divs.

  * LaTeX/Beamer writer: support "blocks" inside columns and other Divs
    (#4016).

  * HTML Writer: consistently use dashed class-names (Mauro Bieg, #3556).
    Note: this change may require some changes in CSS rules.
    `footnoteRef` has become `footnote-ref`, `titleslide` has
    become `title-slide`, and `footnoteBack` has become `footnote-back`.

  * JATS writer: Properly pass through author metadata (#4020).

  * FB2 writer (Alexander Krotov):

    + Write blocks outside of `<p>` in definitions.
    + Make bullet lists consistent with ordered lists, repeating
      the marker for the outer list rather than indenting sublists,
      since indentation does not work in readers.
    + Add new style FB2 tests.

  * `Text.Pandoc.ImageSize`: Add `Millimeter` constructor to `Dimension`
    (#4012) [API change]. Now sizes given in 'mm' are no longer converted
    to 'cm'.

  * Revise documentation of small caps syntax (Andrew Dunning, #4013).

  * Fix broken reference links in manual (Andrew Dunning, #4014)

  * Fixed example of slide columns structure in changelog (#4015).
    Also documented this feature in MANUAL.txt.


## pandoc 2.0.0.1 (2017-10-30)

  * EPUB writer:

    + Fixed filepaths for nonstandard epub-subdirectory values.
    + Ensure that epub2 is recognized as a non-text format,
      so that a template is used.
    + Don't include "prefix" attribute for ibooks for epub2.
      It doesn't validate.
    + Fix stylesheet paths; previously we had an incorrect
      stylesheet path for the cover page and nav page.

  * LaTeX reader:

    + Insert space when needed in macro expansion (#4007).
      Sometimes we need to insert a space after a control sequence
      to prevent it merging with a following letter.
    + Allow unbraced arguments for macros (#4007).
    + Allow body of macro definition to be unbraced (#4007).

  * Linux package build: ensure that pandoc-citeproc is statically linked.

  * trypandoc: add native, ms.


## pandoc 2.0 (2017-10-29)

### New features

  * New output format `ms` (groff ms). Complete support, including
    tables, math, syntax highlighting, and PDF bookmarks. The writer uses
    texmath's new eqn writer to convert math to eqn format, so a ms file
    produced with this writer should be processed with `groff -ms -e` if
    it contains math.

  * New output format `jats` (Journal Article Tag Suite).  This is an XML
    format used in archiving and publishing articles.  Note that a
    URI-encoded CSL stylesheet (`data/jats.csl`) is added automatically
    unless a stylesheet is specified using `--css`.

  * New output format `gfm` (GitHub-flavored CommonMark) (#3841).
    This uses bindings to GitHub's fork of cmark, so it should parse
    gfm exactly as GitHub does (excepting certain postprocessing
    steps, involving notifications, emojis, etc.).  `markdown_github`
    has been deprecated in favor of `gfm`.

  * New output format `muse` (Emacs Muse) (Alexander Krotov, #3489).

  * New input format `gfm` (GitHub-flavored CommonMark) (#3841).
    This uses bindings to GitHub's fork of cmark.  `markdown_github`
    has been deprecated in favor of `gfm`.

  * New input format `muse` (Emacs Muse) reader (Alexander Krotov, #3620).

  * New input format `tikiwiki` (TikiWiki markup) (rlpowell, #3800).

  * New input format `vimwiki` (Vimwiki markup) (Yuchen Pei, #3705).
    Note that there is a new data file, `data/vimwiki.css`, which can
    be used to display the HTML produced by this reader and
    pandoc's HTML writer in the style of vimwiki's own HTML
    export.

  * New input format `creole` (Creole 1.0) (#3994, Sascha Wilde).

  * New syntax for Divs, with `fenced_divs` extension enabled by
    default (#168).  This gives an attractive, plain-text way to create
    containers for block-level content.

  * Added new syntax for including raw content in any output format,
    enabled by the `raw_attribute` extension (which is on by default
    for `markdown` and `multimarkdown`).  The syntax is the same as
    for fenced code blocks or code inlines, only with `{=FORMAT}` for
    attributes, where `FORMAT` is the name of the output format
    (e.g., `ms`, `html`).

  * Implement multicolumn support for slide formats (#1710).
    The structure expected is:

        :::::::::::::: {.columns}
        ::: {.column width="40%"}
        contents...
        :::
        ::: {.column width="60%"}
        contents...
        :::
        ::::::::::::::

    Support has been added for beamer and all HTML slide formats.

  * Allows line comments in templates, beginning with `$--` (#3806).
    (Requires doctemplates 0.2.1.)

  * Add `--eol=crlf|lf|native` flag and writer option to control line endings
    (Stefan Dresselhaus, #3663, #2097).

  * Add `--log` option to save log messages in JSON format to a file (#3392).

  * Add `--request-header` option, to set request headers when pandoc
    makes HTTP requests to fetch external resources. For example:
    `--request-header User-Agent:blah`.

  * Added lua filters (Albert Krewinkel, #3514).  The new `--lua-filter`
    option works like `--filter` but takes pathnames of special lua filters
    and uses the lua interpreter baked into pandoc, so that no external
    interpreter is needed.  Note that lua filters are all applied after
    regular filters, regardless of their position on the command line.
    For documentation of lua filters, see `doc/lua-filters.md`.

  * Set `PANDOC_READER_OPTIONS` in environment where filters are run.
    This contains a JSON representation of `ReaderOptions`, so filters
    can access it.

  * Support creation of pdf via groff `ms` and pdfroff.
    `pandoc -t ms -o output.pdf input.txt`.

  * Support for PDF generation via HTML and `weasyprint` or `prince`
    (Mauro Bieg, #3909).  `pandoc -t html5 -o output.pdf --pdf-engine=prince`.

  * Added `--epub-subdirectory` option (#3720).  This specifies the
    subdirectory in the OCF container that holds the EPUB specific content.
    We now put all EPUB related content in an `EPUB/` subdirectory by default
    (later this will be configurable).

    ```
      mimetype
      META-INF/
        com.apple.ibooks.display-options.xml
        container.xml
      EPUB/ <<--configurable-->>
        fonts/ <<--static-->>
        font.otf
      media/ <<--static-->>
        cover.jpg
        fig1.jpg
      styles/ <<--static-->>
        stylesheet.css
      content.opf
      toc.ncx
      text/ <<--static-->>
        ch001.xhtml
    ```

  * Added `--resource-path=SEARCHPATH` command line option (#852).
    SEARCHPATH is separated by the usual character, depending on OS
    (: on unix, ; on windows).  Default resource path is just working
    directory.  However, the working directory must be explicitly
    specified if the `--resource-path` option is used.

  * Added --abbreviations=FILE option for custom abbreviations file
    (#256).  Dfault abbreviations file (`data/abbreviations`) contains
    a list of strings that will be recognized by pandoc's
    Markdown parser as abbreviations.  (A nonbreaking space will
    be inserted after the period, preventing a sentence space in
    formats like LaTeX.) Users can override the default by putting a file
    abbreviations in their user data directory (`~/.pandoc` on *nix).

  * Allow a theme file as argument to `--highlight-style`.
    Also include a sample, `default.theme`, in `data/`.

  * Allow `--syntax-definition` option for dynamic loading of syntax
    highlighting definitions (#3334).

  * Lists in `markdown` by default now use the CommonMark variable
    nesting rules (#3511). The indentation required for a block-level
    item to be included in a list item is no longer fixed, but is
    determined by the first line of the list item.  To be included in
    the list item, a block must be indented to the level of the first
    non-space content after the list marker. Exception: if are 5 or more
    spaces after the list marker, then the content is interpreted as an
    indented code block, and continuation paragraphs must be indented
    two spaces beyond the end of the list marker.  See the CommonMark
    spec for more details and examples.

    Documents that adhere to the four-space rule should, in most cases,
    be parsed the same way by the new rules.  Here are some examples
    of texts that will be parsed differently:

        - a
          - b

    will be parsed as a list item with a sublist; under the four-space
    rule, it would be a list with two items.

        - a

              code

    Here we have an indented code block under the list item, even though it
    is only indented six spaces from the margin, because it is four spaces
    past the point where a continuation paragraph could begin.  With the
    four-space rule, this would be a regular paragraph rather than a code
    block.

        - a

                code

    Here the code block will start with two spaces, whereas under
    the four-space rule, it would start with `code`.  With the four-space
    rule, indented code under a list item always must be indented eight
    spaces from the margin, while the new rules require only that it
    be indented four spaces from the beginning of the first non-space
    text after the list marker (here, `a`).

    This change was motivated by a slew of bug reports from people
    who expected lists to work differently (#3125, #2367, #2575, #2210,
     #1990, #1137, #744, #172, #137, #128) and by the growing prevalance
    of CommonMark (now used by GitHub, for example).  Those who
    prefer the old behavior can use `-f markdown+four_space_rule`.

  * Added `four_space_rule` extension.  This triggers the old pandoc
    parsing rule for content nested under list items (the "four space
    rule").

  * Added `spaced_reference_links` extension (#2602).  It allows whitespace
    between the two parts of a reference link:  e.g.

        [a] [b]

        [b]: url

    This was previously enabled by default; now it is forbidden by default.

  * Add `space_in_atx_header` extension (#3512).  This is enabled by default
    in pandoc and GitHub markdown but not the other flavors.
    This requirse a space between the opening #'s and the header
    text in ATX headers (as CommonMark does but many other implementations
    do not).  This is desirable to avoid falsely capturing things ilke

        #hashtag

    or

        #5

  * Add `sourcefile` and `outputfile` template variables (Roland Hieber,
    #3431).

  * Allow ibooks-specific metadata in epubs (#2693).  You can now have
    the following fields in your YAML metadata, and it will be treated
    appropriately in the generated EPUB:

    ```
      ibooks:
        version: 1.3.4
        specified-fonts: false
        ipad-orientation-lock: portrait-only
        iphone-orientation-lock: landscape-only
        binding: true
        scroll-axis: vertical
    ```


### Behavior changes

  * Reader functions no longer presuppose that CRs have been
    stripped from the input. (They strip CRs themselves, before
    parsing, to simplify the parsers.)

  * Added support for translations (localization) (#3559).
    Currently this only affects the LaTeX reader, for things
    like `\figurename`.  Translation data files for 46 languages
    can be found in `data/translations`.

  * Make `--ascii` work with DocBook output too.

  * Rename `--latex-engine` to `--pdf-engine`,
    and `--latex-engine-opt` to `--pdf-engine-opt`.

  * Removed `--parse-raw` and `readerParseRaw`.  These were confusing.
    Now we rely on the `+raw_tex` or `+raw_html` extension with latex or html
    input.  Thus, instead of `--parse-raw -f latex` we use `-f latex+raw_tex`,
    and instead of `--parse-raw -f html` we use `-f html+raw_html`.

  * With `--filter` R filters are now recognized, even if they are
    not executable (#3940, #3941, Andrie de Vries).

  * Support SVG in PDF output, converting with `rsvg2pdf` (#1793).

  * Make epub an alias for epub3, not epub2.

  * Removed `--epub-stylesheet`; use `--css` instead (#3472, #847).
    Multiple stylesheets may be used.  Stylesheets will be taken both from
    `--css` and from the `stylesheet` metadata field (which can contain
    either a file path or a list of them).

  * `--mathml` and MathML in HTMLMathMethod no longer take an argument.
    The argument was for a bridge JavaScript that used to be necessary
    in 2004.  We have removed the script already.

  * `--katex` improvements.  The latest version is used, and the
    autoload script is loaded by default.

  * Change MathJax CDN default since old one is shutting down (#3544).
    Note:  The new URL requires a version number, which we'll have
    to update manually in subsequent pandoc releases in order to
    take advantage of mathjax improvements.

  * `--self-contained`: don't incorporate elements with `data-external="1"`
    (#2656).  You can leave an external link as it is by adding the attribute
    data-external="1" to the element.  Pandoc will then not try to
    incorporate its content when `--self-contained` is used.  This is
    similar to a feature already supported by the EPUB writer.

  * Allow `--extract-media` to work with non-binary input formats
    (#1583, #2289).  If `--extract-media` is supplied with a non-binary
    input format, pandoc will attempt to extract the contents of all
    linked images, whether in local files, data: uris, or external uris.
    They will be named based on the sha1 hash of the contents.

  * Make `papersize: a4` work regardless of the case of `a4`.
    It is converted to `a4` in LaTeX and `A4` in ConTeXt.

  * Make `east_asian_line_breaks` affect all readers/writers (#3703).

  * Underlined elements are now treated consistently by readers
    (#2270, hftf); they are always put in a Span with class `underline`.
    This allows the user to treat them differently from other emphasis,
    using a filter.  Docx, Org, Textile, Txt2Tags, and HTML readers
    have been changed.

  * Improved behavior of `auto_identifiers` when there are explicit ids
    (#1745).  Previously only autogenerated ids were added to the list
    of header identifiers in state, so explicit ids weren't taken
    into account when generating unique identifiers.  Duplicated
    identifiers could result.  This simple fix ensures that explicitly given
    identifiers are also taken into account.

  * Use `table-of-contents` for contents of toc, make `toc` a boolean
    (#2872).  Changed markdown, rtf, and HTML-based templates accordingly.
    This allows you to set `toc: true` in the metadata; this
    previously produced strange results in some output formats.
    For backwards compatibility, `toc` is still set to the
    toc contents.  But it is recommended that you update templates
    to use `table-of-contents` for the toc contents and `toc`
    for a boolean flag.

  * Change behavior with binary format output to stdout.
    Previously, for binary formats, output to stdout was disabled
    unless we could detect that the output was being piped (and not
    sent to the terminal).  Unfortunately, such detection is not
    possible on Windows, leaving windows users no way to pipe binary
    output.  So we have changed the behavior in the following way:

    + Output to stdout is allowed when it can be determined that
      the output is being piped (on non-Windows platforms).
    + If the `-o` option is not used, binary output is never sent
      to stdout by default; instead, an error is raised.
    + If `-o -` is used, binary output is sent to stdout, regardless
      of whether it is being piped. This works on Windows too.

  * Better error behavior:  uses of `error` have been replaced by
    raising of `PandocError`, which can be trapped and handled by the
    calling program.

  * Removed `hard_line_breaks` extension from `markdown_github` (#3594).
    GitHub has two Markdown modes, one for long-form documents like READMEs
    and one for short things like issue comments. In issue comments, a line
    break is treated as a hard line break. In README, wikis, etc., it is
    treated as a space as in regular Markdown.  Since pandoc is more likely to
    be used to convert long-form documents from GitHub Markdown,
    `-hard_line_breaks` is a better default.

  * Include `backtick_code_blocks` extension in `mardkown_mmd` (#3637).

  * Escape `MetaString` values (as added with `-M/--metadata` flag) (#3792).
    Previously they would be transmitted to the template without any
    escaping.  Note that `--M title='*foo*'` yields a different result from

        ---
        title: *foo*
        ---

    In the latter case, we have emphasis; in the former case, just
    a string with literal asterisks (which will be escaped
    in formats, like Markdown, that require it).

  * Allow `em`, `cm`, `in` for image height/width in HTML, LaTeX (#3450).

  * HTML writer: Insert `data-` in front of unsupported attributes.  Thus,
    a span with attribute `foo` gets written to HTML5 with `data-foo`, so
    it is valid HTML5.  HTML4 is not affected.  This will allow us to use
    custom attributes in pandoc without producing invalid HTML. (With help
    from Wandmalfarbe, #3817.)

  * Plain writer:  improved super/subscript rendering.  We now
    handle more non-digit characters for which there are
    sub/superscripted unicode characters.  When unicode
    sub/superscripted characters are not available, we use
    `_(..)` or `^(..)` (#3518).

  * Docbook, JATS, TEI writers: print INFO message when omitting interior
    header (#3750).  This only applies to section headers inside list items,
    e.g., which were otherwise silently omitted.

  * Change to `--reference-links` in Markdown writer (#3701).  With
    `--reference-location` of `section` or `block`, pandoc will now repeat
    references that have been used in earlier sections.  The Markdown
    reader has also been modified, so that *exactly* repeated references
    do not generate a warning, only references with the same label but
    different targets.  The idea is that, with references after every block,
    one might want to repeat references sometimes.

  * ODT/OpenDocument writer:

    + Support `lang` attribute (#1667).
    + Added support for `--toc` (#2836).  Thanks to @anayrat.

  * Docx writer:

    + `lang` meta, see #1667 (Mauro Bieg, #3515).
    + Change `FigureWithCaption` to `CaptionedFigure` (iandol, #3658).
    + Use `Table` rather than `Table Normal` for table style (#3275).
      `Table Normal` is the default table style and can't be modified.
    + Pass through comments (#2994).  We assume that comments are defined as
      parsed by the docx reader:

        I want <span class="comment-start" id="0" author="Jesse Rosenthal"
        date="2016-05-09T16:13:00Z">I left a comment.</span>some text to
        have a comment <span class="comment-end" id="0"></span>on it.

      We assume also that the id attributes are unique and properly
      matched between comment-start and comment-end.
    + Bookmark improvements.  Bookmark start/end now surrounds content rather
      than preceding it.  Bookmarks generated for Div with id
      (jgm/pandoc-citeproc#205).
    + Add `keywords` metadata to docx document properties (Ian).

  * RST writer: support unknown interpreted text roles by
    parsing them as `Span` with `role` attributes (#3407).  This
    way they can be manipulated in the AST.

  * HTML writer:

    + Line block: Use class instead of style attribute (#1623).  We now
      issue `<div class="line-block">` and include a default definition
      for `line-block` in the default templates, instead of hard-coding a
      `style` on the div.
    + Add class `footnoteBack` to footnote back references (Timm Albers).
      This allows for easier CSS styling.
    + Render SmallCaps as span with smallcaps class (#1592), rather than
      using a style attribute directly.  This gives the user more flexibility
      in styling small caps in CSS.
    + With reveal.js we use `data-src` instead of `src` for images for
      lazy loading.
    + Special-case `.stretch` class for images in reveal.js (#1291).
      Now in reveal.js, an image with class `stretch` in a paragraph
      by itself will stretch to fill the whole screen, with no
      caption or figure environment.

  * Added warnings for non-rendered blocks to writers.

  * Writers now raise an error on template failure.

  * When creating a PDF via LaTeX, warn if the font is missing some
    characters (#3742).

  * Remove initial check for PDF-creating program (#3819).
    Instead, just try running it and raise the exception if it
    isn't found at that point.  This improves things for users of Cygwin
    on Windows, where the executable won't be found by `findExecutable`
    unless `.exe` is added.  The same exception is raised as before, but
    at a later point.

  * Readers issue warning for duplicate header identifiers (#1745).
    Autogenerated header identifiers are given suffixes so as not to clash
    with previously used header identifiers. But they may still coincide with
    an explicit identifier that is given for a header later in the document,
    or with an identifier on a div, span, link, or image. We now issue
    a warning in this case, so users can supply an explicit identifier.

  * CommonMark reader now supports `emoji`, `hard_line_breaks`, `smart`,
    and `raw_html` extensions.

  * Markdown reader:

    + Don't allow backslash + newline to affect block structure (#3730).
      Note that as a result of this change, the following, which formerly
      produced a header with two lines separated by a line break, will
      now produce a header followed by a paragraph:

        # Hi\
        there

      This may affect some existing documents that relied on
      this undocumented and unintended behavior.  This change makes pandoc
      more consistent with other Markdown implementations, and with itself
      (since the two-space version of a line break doesn't work inside ATX
      headers, and neither version works inside Setext headers).


  * Org reader (Albert Krewinkel, unless noted):

    + Support `table.el` tables (#3314).
    + Support macros (#3401).
    + Support the `#+INCLUDE:` file inclusion mechanism (#3510).
      Recognized include types are `example`, `export`, `src`, and
      normal org file inclusion.  Advanced features like line numbers
      and level selection are not implemented yet.
    + Interpret more meta value as inlines.  The values of the following
      meta variables are now interpreted using org-markup instead of
      treating them as pure strings: `keywords` (comma-separated list of
      inlines), `subtitle` (inline values), `nocite` (inline values, can
      be repeated).
    + Support `\n` export option (#3940).  This turns all newlines in the
      text into hard linebreaks.

  * RST reader:

    + Improved admonition support (#223).  We no longer add an
      `admonition` class, we just use the class for the type of admonition,
      `note` for example.  We put the word corresponding to the label in
      a paragraph inside a `Div` at the beginning of the admonition with
      class `admonition-title`.  This is about as close as we can get to
      RST's own output.
    + Initial support of `.. table` directive.  This allows adding captions
      to tables.
    + Support `.. line-block` directive.  This is deprecated but may still
      be in older documents.
    + Support scale and align attributes of images (#2662).
    + Implemented implicit internal header links (#3475).
    + Support RST-style citations (#853).  The citations appear at the end
      of the document as a definition list in a special div with id
      `citations`. Citations link to the definitions.
    + Recurse into bodies of unknown directives (#3432).
      In most cases it's better to preserve the content than
      to emit it.  This isn't guaranteed to have good results;
      it will fail spectacularly for unknown raw or verbatim directives.
    + Handle chained link definitions (#262).  For example,

          .. _hello:
          .. _goodbye: example.com

      Here both `hello` and `goodbye` should link to `example.com`.
    + Support anchors (#262).  E.g.

          `hello`

          .. _hello:

          paragraph

      This is supported by putting "paragraph" in a `Div` with id `hello`.
    + Support `:widths:` attribute for table directive.
    + Implement csv-table directive (#3533).  Most attributes are supported,
      including `:file:` and `:url:`.
    + Support unknown interpreted text roles by parsing them as Span
      with "role" attributes (#3407).  This way they can be manipulated in
      the AST.

  * HTML reader: parse a span with class `smallcaps` as `SmallCaps`.

  * LaTeX reader:

    + Implemented `\graphicspath` (#736).
    + Properly handle column prefixes/suffixes.  For example, in
      `\begin{tabular}{>{$}l<{$}>{$}l<{$} >{$}l<{$}}`
      each cell will be interpreted as if it has a `$`
      before its content and a `$` after (math mode).
    + Handle komascript `\dedication` (#1845).  It now adds a
      `dedication` field to metadata.  It is up to the user to supply
      a template that uses this variable.
    + Support all `\textXX` commands, where XX = `rm`, `tt`, `up`, `md`,
      `sf`, `bf` (#3488).  Spans with a class are used when there is
      nothing better.
    + Expand `\newenvironment` macros (#987).
    + Add support for LaTeX subfiles package (Marc Schreiber, #3530).
    + Better support for subfigure package (#3577).
      A figure with two subfigures turns into two pandoc
      figures; the subcaptions are used and the main caption
      ignored, unless there are no subcaptions.
    + Add support for `\vdots` (Marc Schreiber, #3607).
    + Add basic support for hyphenat package (Marc Schreiber, #3603).
    + Add basic `\textcolor` support (Marc Schreiber).
    + Add support for `tabularx` environment (Marc Schreiber, #3632).
    + Better handling of comments inside math environments (#3113).
      This solves a problem with commented out `\end{eqnarray}` inside
      an eqnarray (among other things).
    + Parse tikzpicture as raw verbatim environment if `raw_tex` extension
      is selected (#3692).  Otherwise skip with a warning.  This is better
      than trying to parse it as text!
    + Add `\colorbox` support (Marc Schreiber).
    + Set identifiers on Spans used for `\label`.
    + Have `\setmainlanguage` set `lang` in metadata.
    + Support etoolbox's `\ifstrequal`.
    + Support `plainbreak`, `fancybreak` et al from the memoir class
      (bucklereed, #3833).
    + Support `\let`.  Also, fix regular macros so they're expanded at the
      point of use, and NOT also the point of definition.  `\let` macros,
      by contrast, are expanded at the point of definition.  Added an
      `ExpansionPoint` field to `Macro` to track this difference.
    + Support simple `\def` macros.  Note that we still don't support
      macros with fancy parameter delimiters, like `\def\foo#1..#2{...}`.
    + Support `\chaptername`, `\partname`, `\abstractname`, etc.  (#3559,
      obsoletes #3560).
    + Put content of `\ref`, `\label`, `\eqref` commands into `Span` with
      attributes, so they can be handled in filters (Marc Schreiber, #3639)
    + Add Support for `glossaries` and `acronym` package (Marc Schreiber,
      #3589). Acronyms are not resolved by the reader, but acronym and
      glossary information is put into attributes on Spans so that they
      can be processed in filters.
    + Use `Link` instead of `Span` for `\ref`.  This makes more sense
      semantically and avoids unnecessary `Span [Link]` nestings when
      references are resolved.
    + Rudimentary support for `\hyperlink`.
    + Support `\textquoteleft|right`, `\textquotedblleft|right` (#3849).
    + Support `\lq`, `\rq`.
    + Implement `\newtoggle`, `\iftoggle`, `\toggletrue|false` from etoolbox
      (#3853).
    + Support `\RN` and `\Rn`, from biblatex (bucklereed, #3854).
    + Improved support for `\hyperlink`, `\hypertarget` (#2549).
    + Support `\k` ogonek accent.
    + Improve handling of accents.  Handle ogonek, and fall back correctly
      with forms like `\"{}`.
    + Better support for ogonek accents.
    + Support for `\faCheck` and `\faClose` (Marc Schreiber, #3727).
    + Support for `xspace` (Marc Schreiber, #3797).
    + Support `\setmainlanguage` or `\setdefaultlanguage` (polyglossia)
      and `\figurename`.
    + Better handling of `\part` in LaTeX (#1905).  Now we parse chapters as
      level 0 headers, and parts as level -1 headers.  After parsing, we
      check for the lowest header level, and if it's less than 1 we bump
      everything up so that 1 is the lowest header level.  So `\part` will
      always produce a header; no command-line options are needed.
    + Add block version of `\textcolor` (Marc Schreiber).
    + `\textcolor` works as inline and block command (Marc Schreiber).
    + `\textcolor` will be parse as span at the beginning of a paragraph
      (Marc Schreiber).
    + Read polyglossia/babel `\text(LANG){...}` (bucklereed)
    + Improved handling of include files in LaTeX reader (#3971).
      Previously `\include` wouldn't work if the included file
      contained, e.g., a begin without a matching end.
    + Support `\expandafter` (#3983).
    + Handle `\DeclareRobustCommand` (#3983).  Currently it's just treated
      as a synonym for `\newcommand`.
    + Handle `\lettrine` (Mauro Bieg).

  * Math improvements due to updates in texmath:

    + Improved handling of accents and upper/lower delimiters.
    + Support for output in GNU eqn format (used with *roff).
    + Allow `\boldsymbol` + a token without braces, and similarly
      with other styling commands.
    + Improve parsing of `\mathop` to allow multi-character operator names.
    + Add thin space after math operators when "faking it with
      unicode."

  * `walk` is now used instead of `bottomUp` in the `ToJSONFilter`
    instance for `a -> [a]` (pandoc-types).  Note that behavior
    will be slightly different, since `bottomUp`'s treatment of
    a function `[a] -> [a]` is to apply it to each sublist of a
    list, while walk applies it only to maximal sublists.
    Usually the latter behavior is what is wanted, and the
    former can be simulated when needed.  But there may be
    existing filters that need to be rewritten in light of the
    new behavior.  Performance should be improved.

  * There are some changes to syntax highlighting due to revisions
    in the `skylighting` library:

    + Support for `powershell` has been added, and many syntax
      definitions have been updated.
    + Background colors have been added to the `kate` style.
    + The way highlighted code blocks are formatted in HTML has
      been changed (David Baynard), in ways that may require
      changes in hard-coded CSS affecting highlighting.
      (If you haven't included hard-coded highlighting CSS in
      your template, you needn't change anything.)


### API changes

  * New module `Text.Pandoc.Class` (Jesse Rosenthal, John MacFarlane).
    This contains definitions of the `PandocMonad` typeclass, the
    `PandocIO` and `PandocPure` monads, and associated functions.

  * Changed types of all writers and readers.

    + We now use `Text` instead of `String` in the interface (#3731).
      (We have not yet changed the internals of most readers to work
      with `Text`, but making this change in the API now opens up a
      path to doing that.)
    + The result is now of form `m a` with constraint `PandocMonad m`.
      Readers and writers can be combined to form monadic values which
      can be run using either `runIO` or `runPure`.  If `runIO` is used,
      then both readers and writers will be able to do IO when needed
      (for include files, for example); if `runPure` is used,
      then the functions are pure and will not touch IO.
    + Where previously you used
      `writeRST def (readMarkdown def "[foo](url)")`, now you
      would use
      `runPure $ readMarkdown def (pack "[foo](url)") >>= writeRST def`.

  * New module `Text.Pandoc.Readers` (Albert Krewinkel).  This
    contains reader helper functions formerly defined in the
    top-level `Text.Pandoc` module.

    + Changed `StringReader` -> `TextReader`.
    + `getReader` now returns a pair of a reader and
      `Extensions`, instead of building the extensions into the
      reader (#3659).  The calling code must explicitly set
      `readerExtensions` using the `Extensions` returned.  The
      point of the change is to make it possible for the calling
      code to determine what extensions are being used.

  * New module `Text.Pandoc.Writers` (Albert Krewinkel).
    This contains writer helper functions formerly defined in the
    top-level `Text.Pandoc` module.

    + Changed `StringWriter` -> `TextWriter`.
    + `getWriter` now returns a pair of a reader and
      `Extensions`, instead of building the extensions into the
      reader (#3659).  The calling code must explicitly set
      `readerExtensions` using the `Extensions` returned.  The
      point of the change is to make it possible for the calling
      code to determine what extensions are being used.

  * New module `Text.Pandoc.Lua`, exporting `runLuaFilter` (Albert Krewinkel,
    #3514).

  * New module `Text.Pandoc.App`.  This abstracts out the functionality
    of the command line program (`convertWithOpts`), so it can be reproduced
    e.g. in a desktop or web application.  Instead of exiting, we throw errors
    (#3548), which are caught (leading to exit) in pandoc.hs, but allow other
    users of `Text.Pandoc.App` to recover.  `pandoc.hs` is now a 2-liner.
    The module also exports some utility functions for parsing options
    and running filters.

  * New module `Text.Pandoc.Logging` (exported module) (#3392).
    This now contains the `Verbosity` definition previously in
    `Text.Pandoc.Options`, as well as a new `LogMessage` datatype that will
    eventually be used instead of raw strings for warnings.  This will enable
    us, among other things, to provide machine-readable warnings if desired.
    Include ToJSON instance and showLogMessage.  This gives us the possibility
    of both machine-readable and human-readable output for log messages.

  * New module `Text.Pandoc.BCP47`, with `getLang`, `Lang(..)`, `parseBCP47`.

  * New module `Text.Pandoc.Translations`, exporting `Term`,
    `Translations`, `readTranslations`.

  * New module `Text.Pandoc.Readers.LaTeX.Types', exporting `Macro`, `Tok`,
    `TokType`, `Line`, `Column`.

  * `Text.Pandoc.Error`: added many new constructors for `PandocError`.

  * Expose some previously private modules (#3260).  These are often
    helpful to people writing their own reader or writer modules:

    + `Text.Pandoc.Writers.Shared`
    + `Text.Pandoc.Parsing`
    + `Text.Pandoc.Asciify`
    + `Text.Pandoc.Emoji`
    + `Text.Pandoc.ImageSize`
    + `Text.Pandoc.Highlighting`
`
  * New module `Text.Pandoc.Extensions` (Albert Krewinkel):
    Extension parsing and processing functions were defined in the top-level
    `Text.Pandoc` module.  These functions are moved to the Extensions
    submodule as to enable reuse in other submodules.

  * Add `Ext_raw_attribute` constructor for `Extension`.

  * Add `Ext_fenced_divs` constructor for `Extension'.

  * Add `Ext_four_space_rule` constructor in `Extension`.

  * Add `Ext_gfm_auto_identifiers` constructor for `Extension`.

  * Add `Monoid` instance for `Extensions`.

  * Add `Text.Pandoc.Writers.Ms`, exporting `writeMs`.

  * Add `Text.Pandoc.Writers.JATS`, exporting `writeJATS`.

  * Add `Text.Pandoc.Writers.Muse`, exporting `writeMuse`.

  * Add `Text.Pandoc.Readers.Muse`, exporting `readMuse`.

  * Add `Text.Pandoc.Readers.TikiWiki`, exporting `readTikiWiki`.

  * Add `Text.Pandoc.Readers.Vimwiki`, exporting `readVimwiki`.

  * Add `Text.Pandoc.Readers.Creole`, exporting `readCreole`.

  * Export `setVerbosity` from `Text.Pandoc`.

  * `Text.Pandoc.Pretty`: Add `Eq` instance for `Doc`.

  * `Text.Pandoc.XML`: `toEntities`: changed type to `Text -> Text`.

  * `Text.Pandoc.UTF8`:

    + Export `fromText`, `fromTextLazy`, `toText`, `toTextLazy`.
      Define `toString`, `toStringLazy` in terms of them.
    + Add new functions parameterized on `Newline`: `writeFileWith`,
      `putStrWith`, `putStrLnWith`, `hPutStrWith`, `hPutStrLnWith`.

  * `Text.Pandoc.MediaBag`: removed `extractMediaBag`.

  * `Text.Pandoc.Highlighting`:

    + `highlighting` now returns an Either rather than Maybe.
      This allows us to display error information returned by the skylighting
      library.  Display a warning if the highlighting library throws an error.
    + Add parameter for `SyntaxMap` to `highlight`.

  * `Text.Pandoc.Writers.Math`:

    + Export `defaultMathJaxURL`, `defaultKaTeXURL`.  This will ensure that
      we only need to update these in one place.

  * `Text.Pandoc.SelfContained`:

    + Removed `WriterOptions` parameter from `makeSelfContained`.
    + Put `makeSelfContained` in PandocMonad instead of IO.  This removes
      the need to pass MediaBag around and improves exceptions.  It also
      opens up the possibility of using makeSelfContained purely.
    + Export `makeDataURI`.

  * `Text.Pandoc.ImageSize`:

    + Export `lengthToDim`, new function `scaleDimension`.
    + Export `inEm` from ImageSize (#3450).
    + Change `showFl` and `show` instance for `Dimension` so
      extra decimal places are omitted.
    + Added `Em` as a constructor of `Dimension`.
    + Add `WriterOptions` parameter to `imageSize` signature (Mauro Bieg).

  * `Text.Pandoc.Templates`:

    + Change type of `renderTemplate'`.  Now it runs in `PandocMonad`
      and raises a proper `PandocTemplateError` if there are problems, rather
      than failing with uncatchable `error`.
    + Change signature of `getDefaultTemplate`.  Now it runs in any instance
      of `PandocMonad`, and returns a `String` rather than an `Either` value.
      And it no longer takes a `datadir` parameter, since this can be
      retrieved from `CommonState`.

  * `Text.Pandoc.Options`:

    + Added `writerEpubSubdirectory` to `WriterOptions` (#3720).
      The EPUB writer now takes its EPUB subdirectory from this option.
    + In `WriterOptions`, rename `writerLaTeXEngine` to `writerPdfEngine`
      and `writerLaTeXArgs` to `writerPdfArgs` (Mauro Bieg, #3909).
    + Add `writerSyntaxMap` to `WriterOptions`.
    + Removed `writerEpubStylesheet` from `WriterOptions`.
    + Remove `writerUserDataDir` from `WriterOptions`.  It is now carried
      in `CommonState` in `PandocMonad` instances.  (And thus it can be used
      by readers too.)
    + Changed `writerEpubMetadata` to a `Maybe String`.
    + Removed `readerApplyMacros` from `ReaderOptions`.  Now we just check
      the `latex_macros` reader extension.
    + FromJSON/ToJSON instances for `ReaderOptions`.
    + In `HTMLMathMethod`, the `KaTeX` contsructor now takes only
      one string (for the KaTeX base URL), rather than two.
    + Removed `writerSourceURL` from `WriterOptions`.  We now use
      `stSourceURL` in `CommonState`, which is set by `setInputFiles`.

  * `Text.Pandoc.Shared`:

    + `tabFilter` now takes a `Text`, not `String`.
    + `openURL`: Changed type from an Either.  Now it will just raise
      an exception to be trapped later.
    + Remove `normalizeSpaces` (#1530).
    + Remove `warn`.  (Use `report` from `Text.Pandoc.Class` instead.)
    + Export a new function `crFilter`.
    + Add `eastAsianLineBreakFilter` (previously in Markdown reader).
    + Provide custom `isURI` that rejects unknown schemes.
      (Albert Krewinkel, #2713).  We also export the set of known
      `schemes`.  The new function replaces the function of the same name
      from `Network.URI`, as the latter did not check whether a scheme is
      well-known.  All official IANA schemes (as of 2017-05-22) are
      included in the set of known schemes.  The four non-official schemes
      `doi`, `isbn`, `javascript`, and `pmid` are kept.
    + Remove `err`.
    + Remove `readDataFile`, `readDefaultDataFile`, `getReferenceDocx`,
      `getReferenceODT`. These now live in `Text.Pandoc.Class`,
      where they are defined in terms of `PandocMonad`
      primitives and have different signatures.
    + Remove `openURL`.  Use `openURL` from `Text.Pandoc.Class` instead.
    + Add `underlineSpan`.

  * `Text.Pandoc.Readers.HTML`: export new `NamedTag` class.

  * `Text.Pandoc.Readers.Markdown`: remove `readDocxWithWarnings`.
     With the new API one can simply use `getLog` after running
     the reader.

  * `Text.Pandoc.Readers.LaTeX`: Changed types for `rawLaTeXInline`
     and `rawLaTeXBlock`.  (Both now return a `String`, and they are
     polymorphic in state.)


### Bug fixes and under-the-hood improvements

  * TEI writer: Added identifiers on `<div>` elements.

  * DokuWiki reader: Better handling for code block in list item (#3824).

  * Custom writer: Remove old preprocesesor conditionals (Albert Krewinkel).

  * ZimWiki writer: Removed internal formatting from note and table cells,
    because ZimWiki does not support it (Alex Ivkin, #3446).

  * MediaWiki writer:

    + Updated list of syntax highlighting languages (#3461).
      Now `r` gets you `<source>` rather than `<code>` (among others).
    + Add display attribute on `<math>` tags (#3452).  This allows display
      math to be rendered properly.
    + Remove newline before `</ref>` (#2652).
    + Don't softbreak lines inside list items (#3531).

  * Org writer:

    + Reduce to two spaces after bullets (#3417, Albert Krewinkel).
    + Add unit tests (Alexander Krotov).
    + Stop using raw HTML to wrap divs (Albert Krewinkel, #3771).
    + Do not strip `#` from Org anchor links (Alexander Krotov).

  * CommonMark writer:

    + Avoid excess blank lines at end of output.
    + Prefer pipe tables to HTML tables even if it means losing relative
      column width information (#3734).
    + Support table, strikethrough extensions, when enabled (as with gfm).
      Note that we bypass the commonmark writer from cmark and construct our
      own pipe tables, with better results.
    + Properly support `--wrap=none`.
    + Use smallcaps class for `SmallCaps` (#1592).
    + Omit "fig:" prefix in image titles.  This is used internally to
      indicate internal figures.

  * RST writer:

    + Properly handle table captions.
    + Don't wrap lines in in definition list terms.  Wrapping is not allowed.
    + Implemented `+/-smart` and improved escaping with `+smart`.
    + Add empty comments when needed to avoid including a blockquote
      in the indented content of a preceding block (#3675).
    + Improve grid table output, fix bug with empty rows (#3516).
      Uses the new `gridTable` in Writers.Shared, which is here
      improved to better handle 0-width cells.
    + Remove space at beginning/end of RST code span (#3496).  Otherwise
      we get invalid RST.  There seems to be no way to escape the space.
    + Add header anchors when header has non-standard id (#3937).
    + Correctly handle inline code containing backticks, using a `:literal:`
      role (#3974).
    + Don't backslash-escape word-internal punctuation (#3978).

  * Markdown writer:

    + Don't include variables in metadata blocks.  Previously variables set
      on the command line were included in e.g. YAML metadata, contrary to
      documentation and intentions.
    + Improved escaping with `+smart`.
    + Fixed grid tables embedded in grid tables (#2834).
    + Use span with class 'smallcaps' for SmallCaps, instead of a style
      attribute as before (#1592).
    + Escape initial `%` in a paragraph if the `pandoc_title_blocks`
      extension is enabled (#3454).  Otherwise in a document starting with
      a literal `%` the first line is wrongly interpreted as a title.
    + Fixed false ordered lists in YAML metadata (#3492, #1685).  Now we
      properly escape things that would otherwise start ordered lists,
      such as

          ---
          title: 1. inline
          ...
    + Better handling of tables with empty columns (#3337).  We now
      calculate the number of columns based on the longest row (or the
      length of aligns or widths).
    + Escape unordered list markers at beginning of paragraph (#3497), to
      avoid false interpretation as a list.
    + Escape `|` appropriately.
    + Ensure space before list at top level (#3487).
    + Avoid spurious blanklines at end of document after tables and list,
      for example.
    + Fixed bugs in simple/multiline list output (#3384).
      Previously we got overlong lists with `--wrap=none`.  This is fixed.
      Previously a multiline list could become a simple list (and would
      always become one with `--wrap=none`).
    + Don't emit a simple table if `simple_tables` disabled (#3529).
    + Case-insensitive reference links (David A Roberts, #3616).
      Ensure that we do not generate reference links whose labels differ only
      by case.  Also allow implicit reference links when the link
      text and label are identical up to case.
    + Put space before reference link definitions (Mauro Bieg, #3630).
    + Better escaping for links (David A. Roberts, #3619).  Previously the
      Markdown writer would sometimes create links where there were none
      in the source.  This is now avoided by selectively escaping bracket
      characters when they occur in a place where a link might be created.
    + Added missing `\n` (David A. Roberts, #3647).
    + Fixed duplicated reference links with `--reference-links`
      and `--reference-location=section` (#3674).  Also ensure that there
      are no empty link references `[]`.
    + Avoid inline surround-marking with empty content (#3715).
      E.g. we don't want `<strong></strong>` to become `****`.
      Similarly for emphasis, super/subscript, strikeout.
    + Don't allow soft break in header (#3736).
    + Make sure `plain`, `markdown_github`, etc. work for raw.
      Previously only `markdown` worked.  Note: currently a raw block labeled
      `markdown_github` will be printed for any `markdown` format.
    + Ensure that `+` and `-` are escaped properly so they don't cause
      spurious lists (#3773).  Previously they were only
      if succeeded by a space, not if they were at end of line.
    + Use pipe tables if `raw_html` disabled and `pipe_tables` enabled,
      even if the table has relative width information (#3734).
    + Markdown writer: don't crash on `Str ""`.
    + Make `Span` with null attribute transparent.  That is, we don't use
      brackets or `<span>` tags to mark spans when there are no attributes;
      we simply output the contents.
    + Escape pipe characters when `pipe_tables` enabled (#3887).
    + Better escaping of `<` and `>`.  If `all_symbols_escapable` is set,
      we backslash escape these.  Otherwise we use entities as before.
    + When writing plain, don't use `&nbsp;` to separate list and indented
      code.  There's no need for it in this context, since this isn't to be
      interpreted using Markdown rules.
    + Preserve classes in JS obfuscated links (Timm Albers, #2989).
      HTML links containing classes originally now preserve them when using
      javascript email obfuscation.
    + Render `SmallCaps` as a native span when `native_spans` are enabled.
    + Always write attributes with `bracketed_spans` (d-dorazio).

  * Man writer:

    + Fix handling of nested font commands (#3568).  Previously pandoc emitted
      incorrect markup for bold + italic, for example, or bold + code.
    + Avoid error for definition lists with no definitions (#3832).

  * DocBook writer:

    + Fix internal links with `writerIdentifierPrefix opt`
      (#3397, Mauro Bieg).

  * Docx writer:

    + Don't include bookmarks on headers unless non-null id (#3476).
    + Support 9 levels of headers (#1642).
    + Allow 9 list levels (#3519).
    + Don't take `distArchive` from datadir (#3322).  The docx writer takes
      components from the distribution's version of `reference.docx` when it
      can't find them in a user's custom `reference.docx`.  Previously, we
      allowed a `reference.docx` in the data directory (e.g. `~/.pandoc`)
      to be used as the distribution's reference.docx.  This led to a
      bizarre situation where pandoc would produce a good docx using
      `--template ~/.pandoc/ref.docx`, but if `ref.docx` were moved to
      `~/.pandoc/reference.docx`, it would then produce a corrupted docx.
    + Fixed handling of soft hyphen (0173) (#3691).
    + Better handling of keywords (#3719).
    + Cleaner code for handling dir and style attributes for `Div`.
    + Use `Set` for dynamic styles to avoid duplicates.
    + Removed redundant element from data/docx/word/numbering.xml.
      The elements we need are generated when the document is
      compiled; this didn't do anything.
    + Activate `evenAndOddHeaders` from reference docx (#3901,
      Augustín Martín Barbero).

  * ODT/OpenDocument writer:

    + Calculate aspect ratio for percentage-sized images (Mauro Bieg, #3239).
    + Use more widely available bullet characters (#1400).  The old
      characters weren't available in some font sets.  These seem to work
      well on Windows and Linux versions of LibreOffice.
    + Wider labels for lists (#2421).  This avoids overly narrow labels for
      ordered lists with `()` delimiters.  However, arguably it creates
      overly wide labels for bullets.  Also, lists now start flush with
      the margin, rather than indented.
    + Fixed dropped elements in some ordered lists (#2434).

  * FB2 writer:

    + Don't render `RawBlock` as code.
    + Don't fail with an error on interior headers (e.g. in list) (#3750).
      Instead, omit them with an INFO message.
    + Add support for "lang" metadata (Alexander Krotov, #3625).
    + Format `LineBlock` as poem (Alexander Krotov).  Previously writer
      produced one paragraph with `<empty-line/>` elements, which are not
      allowed inside `<p>` according to FB2 schema.
    + Replace `concatMap` with `cMap` (Alexander Krotov).
    + Write FB2 lists without nesting blocks inside `<p>` (Alexander
      Krotov, #4004)

  * HTML writer:

    + Make sure `html4`, `html5` formats work for raw blocks/inlines.
    + Render raw inline environments when `--mathjax` used (#3816).
      We previously did this only with raw blocks, on the assumption
      that math environments would always be raw blocks. This has changed
      since we now parse them as inline environments.
    + Ensure we don't get two style attributes for width and height.
    + Report when not rendering raw inline/block.
    + Issue warning if no title specified and template used (#3473).
    + Info message if `lang` is unspecified (#3486).
    + Removed unused parameter in `dimensionsToAttributeList`.
    + Avoid two class attributes when adding `uri` class (#3716).
    + Fix internal links with `writerIdentifierPrefix opt` (#3397, Mauro
      Bieg).
    + Use revealjs's math plugin for mathjax (#3743).  This is a thin
      wrapper around mathjax that makes math look better on revealjs.
    + Slidy:  use h1 for all slides, even if they were originally
      level 2 headers (#3566).  Otherwise the built-in table of contents
      in Slidy breaks.

  * LaTeX writer:

    + Don't render LaTeX images with data: URIs (#3636).  Note that
      `--extract-media` can be used when the input contains data: URIs.
    + Make highlighted code blocks work in footnotes (Timm Albers).
    + Don't use figure inside table cell (#3836).
    + Use proper code for list enumerators (#3891).  This should fix problems
      with lists that don't use arabic numerals.
    + Always add hypertarget when there's a non-empty identifier (#2719).
      Previously the hypertargets were only added when there was actually
      a link to that identifier.
    + Use `%` after hypertarget before code block.
    + Add `\leavevmode` before hypertarget at start of paragraph (#2704,
      fixes formatting problems in beamer citations).
    + Don't use `lstinline` in `\item[..]` (#645).  If you do, the contents
      of item disappear or are misplaced.  Use `\texttt` instead.
    + Fix problem with escaping in `lstinline` (#1629).  Previously the
      LaTeX writer created invalid LaTeX when `--listings` was specified and
      a code span occurred inside emphasis or another construction.
    + Fix error with line breaks after empty content (#2874).  LaTeX
      requires something before a line break, so we insert a `~` if no
      printable content has yet been emitted.
    + Use BCP47 parser.
    + Fixed detection of otherlangs (#3770).  We weren't recursing into
      inline contexts.
    + Handle language in inline code with `--listings` (#3422).
    + Write euro symbol directly in LaTeX (Andrew Dunning, #3801).
      The textcomp package allows pdfLaTeX to parse `€` directly, making the
      `\euro` command unneeded.
    + Fixed footnotes in table captions (#2378).  Note that if the table has
      a first page header and a continuation page header, the notes will
      appear only on the first occurrence of the header.
    + In `writeBeamer` output, allow hyperlinks to frames (#3220).
      Previously you could link to a header above or below slide level but
      not *to* slide level.  This commit changes that.  Hypertargets are
      inserted inside frame titles; technically the reference is to just
      after the title, but in normal use (where slides are viewed full
      screen in a slide show), this does not matter.
    + Remove `\strut` at beginning of table cells (#3436).  This fixes a
      problem with alignment of lists in table cells.  The `\strut` at the
      end seems to be enough to avoid the too-close spacing that motivated
      addition of the strut  in #1573.
    + Add partial siunitx Support (Marc Schreiber, #3588).

  * ConTeXt writer:

    + Refactored to use BCP47 module.
    + Remove unnecessary `$` (Alexander Krotov, #3482).
    + Use header identifiers for chapters (#3968).

  * EPUB writer:

    + `title_page.xhtml` is now put in `text/`.
    + Don't strip formatting in TOC (#1611).

  * Textile reader:

    + Fix bug for certain links in table cells (#3667).
    + Allow 'pre' code in list item (#3916).

  * HTML reader:

    + Added warnings for ignored material (#3392).
    + Better sanity checks to avoid parsing unintended things as
      raw HTML in the Markdown reader (#3257).
    + Revise treatment of `li` with `id` attribute (#3596).  Previously we
      always added an empty div before the list item, but this created
      problems with spacing in tight lists.  Now we do this: If the list
      item contents begin with a `Plain` block, we modify the `Plain`
      block by adding a `Span` around its contents.  Otherwise, we add a
      `Div` around the contents of the list item (instead of adding an
      empty `Div` to the beginning, as before).
    + Add `details` tag to list of block tags (#3694).
    + Removed `button` from block tag list (#3717).  It is already in the
      `eitherBlockOrInlineTag` list, and should be both places.
    + Use `Set`s instead of lists for block tag lookup.
    + Rewrote to use `Text` throughout.  Effect on memory usage is modest
      (< 10%).
    + Use the lang value of `<html>` to set the lang meta value (bucklereed,
      #3765).
    + Ensure that paragraphs are closed properly when the parent block
      element closes, even without `</p>` (#3794).
    + Parse `<figure>` and `<figcaption>` (Mauro Bieg, #3813).
    + Parse `<main>` like `<div role=main>` (bucklereed, #3791).
      `<main>` closes `<p>` and behaves like a block element generally
    + Support column alignments (#1881).  These can be set either
      with a `width` attribute or with `text-width` in a `style` attribute.
    + Modified state type to be an instance of `HasLogMessages`, so
      `registerHeader` can issue warnings.
    + `</td>` or `</th>` should close any open block tag (#3991).
    + `<td>` should close an open `<th>` or `<td>`.
    + `htmlTag` improvements (#3989).  We previously failed on cases
      where an attribute contained a `>` character. This patch fixes the
      bug, which especially affects raw HTML in Markdown.

  * Txt2Tags reader:

    + Newline is not indentation (Alexander Krotov).

  * MediaWiki reader:

    + Allow extra hyphens after `|-` in tables (#2649).
    + Allow blank line after table start (#2649).
    + Fixed more table issues (#2649).
    + Ensure that list starts begin at left margin (#2606).  Including when
      they're in tables or other list items.
    + Make smart double quotes depend on `smart` extension (#3585).
    + Don't do curly quotes inside `<tt>` contexts (#3585).  Even if `+smart`.
    + Modified state type to be an instance of `HasLogMessages`, so
      `registerHeader` can issue warnings.

  * TWiki reader (Alexander Krotov):

    + Remove unnecessary `$` (#3597).
    + Simplify `linkText` (#3605).

  * EPUB reader:

    + Minor refactoring, avoiding explicit MediaBag handling.
      This all works behind the scenes in CommonState plumbing.

  * Docx reader:

    + Don't drop smartTag contents (#2242).
    + Handle local namespace declarations (#3365).  Previously we didn't
      recognize math, for example, when the xmlns declaration occurred on
      the element and not the root.
    + More efficient trimSps (#1530).  Replacing `trimLineBreaks`.  This
      does the work of `normalizeSpaces` as well, so we avoid the need for
      that function here.
    + Avoid 0-level headers (Jesse Rosenthal, #3830).  We used to parse
      paragraphs styled with "HeadingN" as "nth-level header." But if a
      document has a custom style named "Heading0", this will produce a
      0-level header, which shouldn't exist. We only parse this style
      if N>0. Otherwise we treat it as a normal style name, and
      follow its dependencies, if any.
    + Add tests for avoiding zero-level header (Jesse Rosenthal).

  * ODT reader:

    + Replaced `collectRights` with Rights from `Data.Either`.
    + Remove dead code (Albert Krewinkel).

  * Org reader (Albert Krewinkel, unless noted).

    + Don't allow tables inside list items (John MacFarlane, #3499).
    + Disallow tables on list marker lines (#3499).
    + Convert markup at beginning of footnotes (John MacFarlane, #3576).
    + Allow emphasized text to be followed by `[` (#3577).
    + Handle line numbering switch for src blocks.
      The line-numbering switch that can be given to source blocks (`-n` with
      an start number as an optional parameter) is parsed and translated to a
      class/key-value combination used by highlighting and other readers and
      writers.
    + Stop adding rundoc prefix to src params.  Source block parameter names
      are no longer prefixed with `rundoc`. This was intended to simplify
      working with the rundoc project, a babel runner. However, the rundoc
      project is unmaintained, and adding those markers is not the reader's
      job anyway.  The original language that is specified for a source
      element is now retained as the `data-org-language` attribute and only
      added if it differs from the translated language.
    + Allow multi-word arguments to src block params (#3477).  The reader now
      correctly parses src block parameter list even if parameter arguments
      contain multiple words.
    + Avoid creating `nullMeta` by applying `setMeta` directly
      (Alexander Krotov).
    + Replace `sequence . map` with `mapM`.
    + Fix smart parsing behavior.  Parsing of smart quotes and special
      characters can either be enabled via the `smart` language extension or
      the `'` and `-` export options. Smart parsing is active if either the
      extension or export option is enabled.  Only smart parsing of special
      characters (like ellipses and en and em dashes) is enabled by default,
      while smart quotes are disabled.  Previously, all smart parsing was
      disabled unless the language extension was enabled.
    + Subject full doc tree to headline transformations (Albert Krewinkel,
      #3695).  Emacs parses org documents into a tree structure, which is
      then post-processed during exporting. The reader is changed to do the
      same, turning the document into a single tree of headlines starting
      at level 0.
    + Fix cite parsing behaviour (Herwig Stuetz).  Until now, `org-ref`
      cite keys included special characters also at the end. This caused
      problems when citations occur right before colons or at the end of
      a sentence.  With this change, all non alphanumeric characters at
      the end of a cite key are ignored.  This also adds `,` to the list
      of special characters that are legal in cite keys to better mirror
      the behaviour of org-export.
    + Fix module names in haddock comments.  Copy-pasting had lead to
      haddock module descriptions containing the wrong module names.
    + Recognize babel result blocks with attributes (#3706).  Babel
      result blocks can have block attributes like captions and names.
      Result blocks with attributes were not recognized and were parsed
      as normal blocks without attributes.
    + Include tags in headlines.  The Emacs default is to include tags in the
      headline when exporting.  Instead of just empty spans, which contain the
      tag name as attribute, tags are rendered as small caps and wrapped in
      those spans.  Non-breaking spaces serve as separators for multiple tags.
    + Respect export option for tags (#3713).  Tags are appended to
      headlines by default, but will be omitted when the `tags` export option
      is set to nil.
    + Use `tag-name` attribute instead of `data-tag-name`.
    + Use `org-language` attribute rather than `data-org-language`.
    + Modified state type to be an instance of `HasLogMessages`, so
      `registerHeader` can issue warnings.
    + End footnotes after two blank lines.  Footnotes can not only be
      terminated by the start of a new footnote or a header, but also by two
      consecutive blank lines.
    + Update emphasis border chars (#3933).  The org reader was updated to
      match current org-mode behavior: the set of characters which are
      acceptable to occur as the first or last character in an org emphasis
      have been changed and now allows all non-whitespace chars at the
      inner border of emphasized text (see `org-emphasis-regexp-components`).

  * RST reader:

    + Fixed small bug in list parsing (#3432).  Previously the parser didn't
      handle properly this case:

          * - a
            - b
          * - c
            - d
    + Handle multiline cells in simple tables (#1166).
    + Parse list table directive (Keiichiro Shikano, #3432).
    + Make use of `anyLineNewline` (Alexander Krotov, #3686).
    + Use `anyLineNewline` in `rawListItem` (Alexander Krotov, #3702).
    + Reorganize block parsers for ~20% faster parsing.
    + Fixed `..include::` directive (#3880).
    + Handle blank lines correctly in line blocks (Alexander Krotov, #3881).
      Previously pandoc would sometimes combine two line blocks separated
      by blanks, and ignore trailing blank lines within the line block.
    + Fix indirect hyperlink targets (#512).

  * Markdown reader:

    + Allow attributes in reference links to start on next line (#3674).
    + Parse YAML metadata in a context that sees footnotes defined in
      the body of the document (#1279).
    + When splitting pipe table cells, skip tex math (#3481).
      You might have a `|` character inside math.  (Or for that matter
      something that the parser might mistake for raw HTML.)
    + Treat span with class `smallcaps` as SmallCaps.
      This allows users to specify small caps in Markdown this way:
      `[my text]{.smallcaps}` (#1592).
    + Fixed internal header links (#2397).
      This patch also adds `shortcut_reference_links` to the list
      of mmd extensions.
    + Treat certain environments as inline
      when they occur without space surrounding them (#3309, #2171).
      E.g. equation, math.  This avoids incorrect vertical space
      around equations.
    + Optimized `nonindentSpaces`.  Makes the benchmark go from 40 to 36 ms.
    + Allow latex macro definitions indented 1-3 spaces.
      Previously they only worked if nonindented.
    + Improved parsing of indented raw HTML blocks (#1841).
      Previously we inadvertently interpreted indented HTML as
      code blocks.  This was a regression.  We now seek to determine the
      indentation level of the contents of an HTML block, and (optionally)
      skip that much indentation.  As a side effect, indentation may be
      stripped off of raw HTML blocks, if `markdown_in_html_blocks` is
      used. This is better than having things interpreted as indented
      code blocks.
    + Fixed smart quotes after emphasis (#2228).  E.g. in `*foo*'s 'foo'`.
    + Warn for notes defined but not used (#1718).
    + Use `anyLineNewline` (Alexander Krotov).
    + Interpret YAML metadata as Inlines when possible (#3755).  If
      the metadata field is all on one line, we try to interpret it as
      Inlines, and only try parsing as Blocks if that fails.  If it
      extends over one line (including possibly the `|` or `>` character
      signaling an indented block), then we parse as Blocks.  This was
      motivated by some German users finding that `date: '22. Juin 2017'`
      got parsed as an ordered list.
    + Fixed spurious parsing as citation as reference def (#3840).
      We now disallow reference keys starting with `@` if the
      `citations` extension is enabled.
    + Parse `-@roe` as suppress-author citation (pandoc-citeproc#237).
      Previously only `[-@roe]` (with brackets) was recognized as
      suppress-author, and `-@roe` was treated the same as `@roe`.
    + Fixed parsing of fenced code after list when there is no intervening
      blank line (#3733).
    + Allow raw latex commands starting with `\start` (#3558).  Previously
      these weren't allowed because they were interpreted as starting
      ConTeXt environments, even without a corresponding `\stop`...
    + Added `inlines`, `inlines1`.
    + Require nonempty alt text for `implicit_figures` (#2844).
      A figure with an empty caption doesn't make sense.
    + Removed texmath macro material; now all this is handled
      in the LaTeX reader functions.
    + Fixed bug with indented code following raw LaTeX (#3947).

  * LaTeX reader:

    + Rewrote LaTeX reader with proper tokenization (#1390,
      #2118, #3236, #3779, #934, #982).  This rewrite is primarily
      motivated by the need to get macros working properly.  A side benefit
      is that the reader is significantly faster.  We now tokenize the
      input text, then parse the token stream.  Macros modify the token
      stream, so they should now be effective in any context, including
      math. Thus, we no longer need the clunky macro processing
      capacities of texmath.
    + Parse `\,` to `\8198` (six-per-em space) (Henri Werth).
    + Allow `\newcommand\foo{blah}` without braces.
    + Support `\lstinputlisting` (#2116).
    + Issue warnings when skipping unknown latex commands (#3392).
    + Include contents of `\parbox`.
    + Allow `\hspace` and `\vspace` to count as raw block or inline.
      Previously we would refuse to parse anything as raw inline if
      it was in the `blockCommands` list.  Now we allow exceptions
      if they're listed under ignoreInlines in inlineCommands.
      This should make it easier e.g. to include an `\hspace`
      between two side-by-side raw LaTeX tables.
    + Don't drop contents of `\hypertarget`.
    + Handle spaces before `\cite` arguments.
    + Allow newpage, clearpage, pagebreak in inline contexts as well as
      block contexts (#3494).
    + Treat `{{xxx}}` the same as `{xxx}` (#2115).
    + Use `pMacroDefinition` in macro (for more direct parsing).
      Note that this means that `macro` will now parse one
      macro at a time, rather than parsing a whole group together.
    + Fixed failures on `\ref{}`, `\label{}` with `+raw_tex`.  Now these
      commands are parsed as raw if `+raw_tex`; otherwise, their argument
      is parsed as a bracketed string.
    + Don't crash on empty `enumerate` environment (#3707).
    + Handle escaped `&` inside table cell (#3708).
    + Handle block structure inside table cells (#3709).  `minipage` is no
      longer required.
    + Handle some width specifiers on table columns (#3709).  Currently
      we only handle the form `0.9\linewidth`.  Anything else would have
      to be converted to a percentage, using some kind arbitrary assumptions
      about line widths.
    + Make sure `\write18` is parsed as raw LaTeX.  The change is in the
      LaTeX reader's treatment of raw commands, but it also affects the
      Markdown reader.
    + Fixed regression with starred environment names (#3803).
    + Handle optional args in raw `\titleformat` (#3804).
    + Improved heuristic for raw block/inline.  An unknown command at the
      beginning of the line that could be either block or inline is
      treated as block if we have a sequence of block commands followed by
      a newline or a `\startXXX` command (which might start a raw ConTeXt
      environment).
    + Don't remove macro definitions from the output, even if
      `Ext_latex_macros` is set, so that macros will be applied.
      Since they're only applied to math in Markdown, removing the macros
      can have bad effects.  Even for math macros, keeping them should be
      harmless.
    + Removed `macro`.  It is no longer necessary, since the
      `rawLaTeXBlock` parser will parse macro definitions.  This also avoids
      the need for a separate `latexMacro` parser in the Markdown reader.
    + Use `label` instead of `data-label` for label in caption (#3639).
    + Fixed space after `\figurename` etc.
    + Resolve references to section numbers.
    + Fix `\let\a=0` case, with single character token.
    + Allow `@` as a letter in control sequences.  `@` is commonly used
      in macros using `\makeatletter`.  Ideally we'd make the tokenizer
      sensitive to `\makeatletter` and `\makeatother`, but until then this
      seems a good change.
    + Track header numbers and correlate with labels.
    + Allow `]` inside group in option brackets (#3857).
    + lstinline with braces can be used (verb cannot be used with braces)
      (Marc Schreiber, #3535).
    + Fix keyval function: pandoc did not parse options in braces correctly
      (Marc Schreiber, #3642).
    + When parsing raw LaTeX commands, include trailing space (#1773).
      Otherwise things like `\noindent foo` break and turn into
      `\noindentfoo`.  Affects `-f latex+raw_tex` and `-f markdown` (and other
      formats that allow `raw_tex`).
    + Don't treat "..." as Quoted (#3958).  This caused quotes to be omitted in
      `\texttt` contexts.
    + Add tests for existing `\includegraphics` behaviour (Ben Firshman).
    + Allow space before `=` in bracketd options (Ben Firshman).
    + Be more forgiving in parsing command options.  This was needed, for
      example, to make some minted options work.
    + Strip off quotes in `\include` filenames.

  * Added `Text.Pandoc.CSV`, simple (unexported) CSV parser.

  * `Text.Pandoc.PDF`:

    + Got `--resource-path` working with PDF output (#852).
    + Fetch images when generating PDF via context (#3380).
      To do this, we create the temp directory as a subdirectory
      of the working directory. Since context mk IV by default looks
      for images in the parent directory, this works.
    + Use `report` instead of `warn`, make it sensitive to verbosity settings.
    + Use `fillMediaBag` and `extractMedia` to extract media to temp dir.
      This reduces code duplication.
    + `html2pdf`: use stdin instead of intermediate HTML file
    + Removed useless `TEXINPUTS` stuff for `context2pdf`.  mkiv context
      doesn't use `TEXINPUTS`.

  * `Text.Pandoc.Pretty`:

    + Simplified definition of `realLength`.
    + Don't error for blocks of size < 1.  Instead, resize to 1 (see #1785).

  * `Text.Pandoc.MIME`:

    + Use `application/javascript` (not `application/x-javascript`).
    + Added `emf` to mimeTypes with type `application/x-msmetafile` (#1713).

  * `Text.Pandoc.ImageSize`:

    + Improve SVG image size code (Marc Schreiber, #3580).
    + Make `imageSize` recognize basic SVG dimensions (Mauro Bieg, #3462).

  * Use `Control.Monad.State.Strict` throughout.  This gives 20-30% speedup
    and reduction of memory usage in most of the writers.

  * Use `foldrWithKey` instead of deprecated `foldWithKey`.

  * `Text.Pandoc.SelfContained`:

    + Fixed problem with embedded fonts (#3629).
    + Refactored getData from `getDataURI` in `SelfContained`.
    + Don't use data URIs for script or style (#3423).  Instead, just use
      script or style tags with the content inside.  The old method with
      data URIs prevents certain optimizations outside pandoc.  Exception:
      data URIs are still used when a script contains `</script>` or a
      style contains `</`.
    + SelfContained: Handle URL inside material retrieved from a URL
      (#3629).  This can happen e.g. with an @import of a google web font.
      (What is imported is some CSS which contains an url reference
      to the font itself.) Also, allow unescaped pipe (|) in URL.
    + Load resources from `data-src` (needed for lazy loading in
      reveal.js slide shows).
    + Handle `data-background-image` attribute on section (#3979).

  * `Text.Pandoc.Parsing`:

    + Added `indentWith` (Alexander Krotov, #3687).
    + Added `stateCitations` to `ParserState`.
    + Removed `stateChapters` from `ParserState`.
    + In `ParserState`, make `stateNotes'` a Map, add `stateNoteRefs`.
    + Added `gobbleSpaces` and `gobbleAtMostSpaces`.
    + Adjusted type of `insertIncludedFile` so it can be used with token
      parser.
    + Replace old texmath macro stuff from Parsing.  Use Macro from
      Text.Pandoc.Readers.LaTeX.Types instead.
    + Export `insertIncludedFile`.
    + Added `HasLogMessages`, `logMessage`, `reportLogMessages` (#3447).
    + Replace partial with total function (Albert Krewinkel).
    + Introduce `HasIncludeFiles` type class (Albert Krewinkel).  The
      `insertIncludeFile` function is generalized to work with all parser
      states which are instances of that class.
    + Add `insertIncludedFilesF` which returns F blocks (Albert Krewinkel).
      The `insertIncludeFiles` function was generalized and renamed
      to `insertIncludedFiles'`; the specialized versions are based on that.
    + `many1Till`: Check for the end condition before parsing (Herwig
      Stuetz).  By not checking for the end condition before the first
      parse, the parser was applied too often, consuming too much of the
      input. This only affects `many1Till p end` where `p` matches on a
      prefix of `end`.
    + Provide `parseFromString` (#3690).  This is a version of
      `parseFromString` specialied to ParserState, which resets
      `stateLastStrPos` at the end.  This is almost always what we want.
      This fixes a bug where `_hi_` wasn't treated as emphasis in the
      following, because pandoc got confused about the position of the
      last word: `- [o] _hi_`.
    + Added `takeP`, `takeWhileP` for efficient parsing of `[Char]`.
    + Fix `blanklines` documentation (Alexander Krotov, #3843).
    + Give less misleading line information with `parseWithString`.
      Previously positions would be reported past the end of the chunk.
      We now reset the source position within the chunk and report
      positions "in chunk."
    + Add `anyLineNewline` (Alexander Krotov).
    + Provide shared F monad functions for Markdown and Org readers
      (Albert Krewinkel).  The `F` monads used for delayed evaluation
      of certain values in the Markdown and Org readers are based on a
      shared data type capturing the common pattern of both `F` types.
    + Add `returnF` (Alexander Krotov).
    + Avoid parsing `Notes:**` as a bare URI (#3570).  This avoids parsing
      bare URIs that start with a scheme + colon + `*`, `_`, or `]`.
    + Added `readerAbbreviations` to `ParserState`.  Markdown reader
      now consults this to determine what is an abbreviation.
    + Combine grid table parsers (Albert Krewinkel, #3638).  The grid table
      parsers for markdown and rst was combined into one single
      parser `gridTable`, slightly changing parsing behavior of both
      parsers: (1) The markdown parser now compactifies block content
      cell-wise: pure text blocks in cells are now treated as paragraphs
      only if the cell contains multiple paragraphs, and as plain blocks
      otherwise. Before, this was true only for single-column tables. (2)
      The rst parser now accepts newlines and multiple blocks in header
      cells.
    + Generalize tableWith, gridTableWith (Albert Krewinkel).
      The parsing functions `tableWith` and `gridTableWith` are generalized
      to work with more parsers. The parser state only has to be an
      instance of the `HasOptions` class instead of requiring a concrete
      type. Block parsers are required to return blocks wrapped into a
      monad, as this makes it possible to use parsers returning results
      wrapped in `Future`s.

  * `Text.Pandoc.Shared`:

    + Simplify `toRomanNumeral` using guards (Alexander Krotov, #3445)
    + `stringify`: handle Quoted better (#3958).  Previously we were losing
      the quotation marks in Quoted elements.

  * `Text.Pandoc.Writers.Shared`:

    + Export `metaToJSON'`, `addVariablesToJSON` (#3439).
      This allows us to add the variables AFTER using the metadata
      to generate a YAML header (in the Markdown writer).
    + Added `unsmartify` (previously in RST writer).
      Undo literal double curly quotes.  Previously we left these.
    + Generalize type of `metaToJSON` so it can take a Text.  Previously a
      String was needed as argument; now any ToJSON instance will do.
    + Added `gridTable` (previously in Markdown writer).
    + `gridTable`: Refactored to use widths in chars.
    + `gridTable`:  remove unnecessary extra space in cells.
    + Fixed `addVariablesToJSON`.  It was previously not allowing multiple
      values to become lists.
    + Pipe tables: impose minimum cell size (see #3526).


### Default template changes

  * HTML templates (including EPUB and HTML slide show templates):

    + Make default.html5 polyglot markup conformant (John Luke Bentley,
      #3473).  Polyglot markup is HTML5 that is also valid XHTML. See
      <https://www.w3.org/TR/html-polyglot>.  With this change, pandoc's
      html5 writer creates HTML that is both valid HTML5 and valid XHTML.
    + Regularized CSS in html/epub/html slide templates (#3485).
      All templates now include `code{white-space: pre-wrap}`
      and CSS for `q` if `--html-q-tags` is used.  Previously some templates
      had `pre` and others `pre-wrap`; the `q` styles were only sometimes
      included.
    + CSS for `.smallcaps`, (Mauro Bieg, #1592)
    + `default.revealjs`: make `history` default to true.
    + `default.revealjs`: use lazy loading (#2283).
    + `default.revealjs`: add `mathjax` variable and some conditional code
      to use the MathJaX plugin.
    + `default.slidy` uses `https` instead of `http` (ickc, #3848).
    + `default.dzslides`: Load Google Font using HTTPS by default
      (Yoan Blanc).

  * DocBook5 template: Use `lang` and `subtitle` variables (Jens Getreu,
    #3855).

  * LaTeX/Beamer template:

    + Combine LaTeX/Beamer templates (Andrew Dunning, #3878).
      `default.beamer` has been removed; beamer now uses the
      `default.latex` template.  Beamer-specific parts are conditional
      on the `beamer` variable set by the writer. Note that
      `pandoc -D beamer` will return this (combined) template.
    + Use `xcolor` for `colorlinks` option (Andrew Dunning, #3877).
      Beamer loads `xcolor` rather than `color`, and thus the
      `dvipsnames` option doesn't take effect. This also provides a wider
      range of colour selections with the `svgnames` option.
    + Use starred versions of `xcolor` names (Andrew Dunning).
      Prevents changes to documents defined using the `dvipsnames` list (e.g.
      `Blue` gives a different result with svgnames enabled).
    + Load `polyglossia` after header-includes (#3898).  It needs to be
      loaded as late as possible.
    + Use `unicode-math` (Vaclav Haisman).  Use `mathspec` with only
      XeLaTeX on request.
    + Don't load `fontspec` before `unicode-math` (over there).
      The `unicode-math` package loads `fontspec` so explicit loading of
      `fontspec` before `unicode-math` is not necessary.
    + Use `unicode-math` by default in default.latex template.  mathspec will
      be used in xelatex if the `mathspec` variable is set; otherwise
      unicode-math will be used (Václav Haisman).
    + Use `dvipsnames` options when `colorlinks` specified (otherwise
      we get an error for `maroon`) (Thomas Hodgson).
    + Added beamer `titlegraphic` and `logo` variables (Thomas Hodgson).
    + Fix typo in fix for notes in tables (#2378, zeeMonkeez).
    + Fix `hyperref` options clash (Andrew Dunning, #3847) Avoids an options
      clash when loading a package (e.g. `tufte-latex`) that uses
      `hyperref` settings different from those in the template.
    + Add `natbiboptions` variable (#3768).
    + Fix links inside captions in LaTeX output with links-as-notes
      (Václav Haisman, #3651).  Declare our redefined `\href` robust.
    + Load `parskip` before `hyperref` (Václav Haisman, #3654).
    + Allow setting Japanese fonts when using LuaLaTeX (Václav Haisman,
      #3873).  by using the `luatexja-fontspec` and `luatexja-preset`
      packages. Use existing `CJKmainfont` and `CJKoptions` template
      variables. Add `luatexjafontspecoptions` for `luatexja-fontspec`
      and `luatexjapresetoptions` for `luatexja-preset`.
    + Added `aspectratio` variable to beamer template (Václav Haisman,
      #3723).
    + Modified template.latex to fix XeLaTex being used with tables
      (lwolfsonkin, #3661).  Reordered `lang` variable handling to
      immediately before `bidi`.

  * ConTeXt template: Improved font handling: `simplefonts` is now
    obsolete in ConTeXt (Pablo Rodríguez).


### Documentation improvements

  * MANUAL.txt:

    + Add URL for Prince HTML > PDF engine (Ian, #3919).
    + Document that content above slide-level will be omitted in
      slide shows.  See #3460, #2265.
    + Explain `--webtex` SVG url (Mauro Bieg, #3471)
    + Small clarification in YAML metadata section.
    + Document that html4 is technically XHTML 1.0 transitional.
    + Remove refs to highlighting-kate (#3672).
    + Document ibooks specific epub metadata.
    + Clarify that mathml is used for ODT math.
    + Mention limitations of Literate Haskell Support (#3410,
      Joachim Breitner).
    + Add documentation of limitations of grid tables (Stephen
      McDowell, #3864).
    + Clarify that meta-json contains transformed values (Jakob Voß,
      #3491) Make clear that template variable `meta-json` does not
      contain plain text values or JSON output format but field values
      transformed to the selected output format.

  * COPYRIGHT:

    + Clarify that templates are dual-licensed.
    + Clarify that pandoc-types is BSD3 licensed.
    + List new files not written by jgm (Albert Krewinkel).
    + Update dates in copyright notices (Albert Krewinkel).  This follows
      the suggestions given by the FSF for GPL licensed software.
      <https://www.gnu.org/prep/maintain/html_node/Copyright-Notices.html>

  * INSTALL.md:

    + Improved instructions for tests with patterns.
    + Put RPM-based distros on separate point (Mauro Bieg, #3449)

  * CONTRIBUTING.md:

    + Fixed typos (Wandmalfarbe, #3479).
    + Add "ask on pandoc-discuss" (Mauro Bieg).

  * Add lua filter documentation in `doc/lua-filters.md`.  Note that the
    end of this document is autogenerated from `data/pandoc.lua`
    using `make doc/lua-filters.md`, which uses `tools/ldoc.ltp`
    (Albert Krewinkel).

  * Add `doc/filters.md`. This is the old scripting tutorial from
    the website.

  * Add `doc/using-the-pandoc-api.md` (#3289).  This gives an introduction
    to using pandoc as a Haskell library.


### Build infrastructure improvements

  * Removed `data/templates` submodule.  Templates are now a subtree
    in `data/templates`.  This removes the need to do `git submodule
    update`.

  * Renamed `tests` -> `test`.

  * Remove `https` flag.  Always build with HTTPS support.

  * Use `file-embed` instead of `hsb2hs` to embed data files when
    `embed_data_files` flag is set.  `file-embed` gives us better dependency
    tracking:  if a data file changes, ghc/stack/cabal know to recompile
    the Data module.  This also removes `hsb2hs` as a build dependency.

  * Add `custom-setup` stanza to pandoc, lowercase field names.

  * Add `static` Cabal flag.

  * Name change OSX -> MacOS.  Add a -MacOS suffix to mac package rather
    than -OSX.  Changed local names from osx to macos.

  * make_macos_package.sh - Use strip to reduce executable size.

  * Revised binary linux package.  Now a completely static executable
    is created, using Docker and alpine.  We create both a deb and a
    tarball.  The old `deb` directory has been replaced with a `linux`
    directory.  Running `make` in the `linux` directory should
    perform the build, putting the binary packages in `artifacts/`.

  * `linux/control.in`: add `Replaces:`, so existing pandoc-citeproc and
    pandoc-data packages will be uninstalled; this package provides
    both (#3822).  Add latex packages as 'suggested', update
    description.

  * Remove cpphs build requirement -- it is no longer needed.

  * Replaced `{deb,macos,windows}/stack.yaml` with `stack.pkg.yaml`.

  * Name change OSX -> macOS (ickc, #3869).

  * Fix casing of Linux, UNIX, and Windows (ickc).

  * `.travis.yml`:  create a source dist and do cabal build and test there.
    That way we catch errors due to files missing from the data
    section of pandoc.cabal.

  * Makefile:

    + Split `make haddock` from `make full`.
    + Add BRANCH variable for winpkg.
    + Add `lint` target.
    + Improve `make full`. Disable optimizations.
      Build everything, inc. trypandoc and benchmarks.  Use parallel build.
    + Allow `make test` to take `TESTARGS`.

  * Added new command tests (`Tests.Command`), using small text files
    in `test/command/`.  Any files added in this directory will be treated
    as shell tests (see smart.md for an example).  This makes it very easy
    to add regression tests etc.

  * Test fixes so we can find data files.  In old tests & command tests,
    we now set the environment variable `pandoc_datadir`.  In lua tests,
    we set the datadir explicitly.

  * Refactored `compareOutput` in docx writer test.

  * Consolidated some common functions in `Tests.Helper`.

  * Small change to unbalanced bracket test to speed up test suite.

  * Speed up Native writer quickcheck tests.

  * Use tasty for tests rather than test-framework.

  * Add simple Emacs mode to help with Pandoc templates editing.
    (Václav Haisman, #3889). `tools/pandoc-template-mode.el`


## pandoc 1.19.2.4 (2017-09-10)

  * Add dependencies on texmath and skylighting to the executable.
    This is needed for dependency version numbers to be available,
    with Cabal > 2.

## pandoc 1.19.2.3 (2017-09-09)

  * Add CPP to Setup.hs so it works with Cabal >= 2 and < 2.

## pandoc 1.19.2.2 (2017-09-08)

  * Fix build with GHC 8.2.1 (#3876, Peter Simons).  Setup.hs does not
    compile with Cabal 2.x, so we require an earlier version via
    setup-depends.  The following packages need newer versions with
    GHC 8.2.1 and had their constraints relaxed accordingly:
    executable-path, process, syb, and time.

## pandoc 1.19.2.1 (2017-01-31)

  * Require skylighting >= 0.1.1.4.
  * Adjust test output for skylighting version.
  * Relax upper bounds on blaze-html and blaze-markup.

## pandoc 1.19.2 (2017-01-29)

  * Use skylighting library instead of highlighting-kate for syntax
    highlighting. Skylighting is faster and more accurate (#3363).
    Later we'll be able to add features like warning messages, dynamic
    loading of xml syntax definitions, and dynamic loading of themes.

  * Added a new highlight style, `breezeDark`.

  * Text.Pandoc.Highlighting: Update list of `listings` languages (#3374).
    This allows more languages to be used when using the `--listings`
    option.

  * OpenDocument writer:

    + Small refactoring.  Removed separate 'parent' parameter in paraStyle.
    + Don't profilerate text styles unnecessarily (#3371).
      This change makes the writer create only as many temporary
      text styles as are absolutely necessary. It also consolidates
      adjacent nodes with the same style.

  * Org reader (Albert Krewinkel):

    + Allow short hand for single-line raw blocks (Albert Krewinkel,
      #3366).  Single-line raw blocks can be given via `#+FORMAT: raw line`,
      where `FORMAT` must be one of `latex`, `beamer`, `html`, or `texinfo`.
    + Accept org-ref citations followed by commas (Albert Krewinkel).
      Bugfix for an issue which, whenever the citation was immediately
      followed by a comma, prevented correct parsing of org-ref citations.
    + Ensure emphasis markup can be nested.  Nested emphasis markup (e.g.
      `/*strong and emphasized*/`) was interpreted incorrectly in that the
      inner markup was not recognized.
    + Remove pipe char irking the haddock coverage tool (Albert Krewinkel).

  * Docx reader: Empty header should be list of lists (Jesse Rosenthal).
    In the past, the docx reader wrote an empty header as an empty list. It
    should have the same width as a row (and be filled with empty cells).

  * MediaWiki reader:

    + Improved handling of display math (#3362).  Sometimes display math is
      indented with more than one colon.  Previously we handled these cases
      badly, generating definition lists and missing the math.
    + Fix quotation mark parsing (#3336, tgkokk).  Change MediaWiki reader's
      behavior when the smart option is parsed to match other readers'
      behavior.

  * Markdown reader:

    + Fixed `-f markdown_github-hard_line_breaks+escaped_line_breaks`
      (#3341).  Previously this did not properly enable escaped line breaks.
    + Disallow space between inline code and attributes (#3326, #3323,
      Mauro Bieg).

  * DocBook5 writer: make id attribute xml:id, fixes #3329 (#3330, Mauro Bieg).

  * Added some test cases for ODT reader (#3306, #3308, Hubert Plociniczak).

  * LaTeX writer: allow tables with empty cells to count as "plain."
    This addresses a problem of too-wide tables when empty cells
    are used.  Thanks to Joost Kremers for reporting the issue.

  * Org writer: prefix footnote numbers with `fn:` (Albert Krewinkel).
    Unprefixed numbers where used by older org-mode versions, but are no
    longer supported.

  * HTML writer: don't process pars with empty RawInline, (#1040, #3327,
    Mauro Bieg).

  * Markdown writer: Fix display math with `--webtex` (#3298).

  * Fix sample.lua so it properly handles raw blocks/inlines (#3358,
    bumper314).

  * Templates:

    + default.latex: Moved geometry after hyperref (Václav Haisman).
      Otherwise PDF sizes can be wrong in some circumstances.
    + Copied a few changes from default.latex to default.beamer
      (Wandmalfarbe).
    + default.latex, default.beamer: Changed position of `\VerbatimNotes`
      and `fancyvrb`.  This fixes hyperlinks on footnotes in documents
      that contain verbatim in notes (#3361).  (Note: the beamer template
      was updated to match the LaTeX template, but at this point verbatim
      in notes seems not to work in beamer.)
    + default.latex: Allow passing `microtypeoptions` to microtype
      (Václav Haisman).
    + default.latex: Add hyphen option to url package.
    + default.docbook5: Fix namespace declarations (Mauro Bieg).

  * Moved `make_osx_package.sh` to `osx/` directory.

  * Travis continuous integration:

    + Fix false positives with dist build.
    + Speed improvements (Kolen Cheung, #3304, #3357).

  * MANUAL.txt:

    + Clarify that blank space is needed around footnotes (#3352).
    + Fixed typo (#3351, Alexey Rogechev).
    + Note that `--wrap=auto` does not work in HTML output.
    + Default `--columns` width is 72, not 80.
    + Fixed broken links (#3316, Kolen Cheung).
    + Document usage of `@*` in nocite section (#3333, John Muccigrosso).

  * INSTALL.md:

    + Indent code so it's properly formatted (#3335, Bheesham Persaud).
    + Added instructions for extracting binary from OSX, Windows packages.

  * CONTRIBUTING.md: Describe labels currently used in issue tracker
    (Albert Krewinkel).  The labels have changed over time, the list of
    labels is updated to reflect the current set of labels used in the
    issue tracker.

  * Rearrange and extend badges in README (Albert Krewinkel, #3354)

  * Bumped version bounds for dependencies.


## pandoc 1.19.1 (2016-12-10)

  * Set `PANDOC_VERSION` environment variable for filters (#2640).
    This allows filters to check the pandoc version that produced
    the JSON they are receiving.

  * Docx reader: Ensure one-row tables don't have header (#3285,
    Jesse Rosenthal).  Tables in MS Word are set by default to have
    special first-row formatting, which pandoc uses to determine whether
    or not they have a header. This means that one-row tables will, by
    default, have only a header -- which we imagine is not what people
    want. This change ensures that a one-row table is not understood to
    be a header only.  Note that this means that it is impossible to
    produce a header-only table from docx, even though it is legal
    pandoc. But we believe that in nearly all cases, it will be an
    accidental (and unwelcome) result

  * HTML reader:

    + Fixed some bad regressions in HTML table parser (#3280).
      This regression leads to the introduction of empty rows
      in some circumstances.
    + Understand `style=width:` as well as `width` in `col` (#3286).

  * RST reader:

    + Print warnings when keys, substitition, notes not found.
      Previously the parsers failed and we got raw text.  Now we get a
      link with an empty URL, or empty inlines in the case of a note or
      substitution.

    + Fix hyperlink aliases (#3283).

  * Man writer: Ensure that periods are escaped at beginning of line
    (#3270).

  * LaTeX writer: Fix unnumbered headers when used with `--top-level`
    (#3272, Albert Krewinkel). Fix interaction of top-level
    divisions `part` or `chapter` with unnumbered headers when
    emitting LaTeX. Headers are ensured to be written using
    stared commands (like `\subsection*{}`).

  * LaTeX template: use comma not semicolon to separate keywords for
    `pdfkeywords`.  Thanks to Wandmalfarbe.

  * Markdown writer: Fixed incorrect word wrapping (#3277).
    Previously pandoc would sometimes wrap lines too early due to
    this bug.

  * Text.Pandoc.Pretty:  Added `afterBreak` [API change].  This makes it
    possible to insert escape codes for content that needs escaping at the
    beginning of a line.

  * Removed old MathMLInHTML.js from 2004, which should no longer
    be needed for MathML with modern browsers.

  * Fixed tests with dynamic linking (#2709).

  * Makefile: Use stack instead of cabal for targets.  This is just
    a convenience for developers.

  * Fixed bash completion of filenames with space (#2749).

  * MANUAL: improved documentation on how to create a custom
    `reference.docx`.

  * Fix minor spelling typos in the manual (#3273, Anthony Geoghegan)

## pandoc 1.19 (2016-12-01)

  * Changed resolution of filter paths.

    + We now first treat the argument of `--filter` as a full (absolute
      or relative) path, looking for a program there. If it's found, we
      run it.
    + If not, and if it is a simple program name or a relative path, we
      try resolving it relative to `$DATADIR/filters`.
    + If this fails, then we treat it as a program name and look in the
      user's PATH.
    + Removed a hardcoded '/' that may have caused problems with
      Windows paths.

    Previously if you did `--filter foo` and you had `foo` in your path and
    also an executable `foo` in your working directory, the one in the path
    would be used. Now the one in the working directory is used.

    In addition, when you do `--filter foo/bar.hs`, pandoc will now find a
    filter `$DATADIR/filters/foo/bar.hs` -- assuming there isn't a
    `foo/bar.hs` relative to the working directory.

  * Allow `file://` URIs as arguments (#3196). Also improved default reader
    format detection. Previously with a URI ending in .md or .markdown,
    pandoc would assume HTML input. Now it treats these as markdown.

  * Allow to overwrite top-level division type heuristics (#3258,
    Albert Krewinkel). Pandoc uses heuristics to determine the most
    reasonable top-level division type when emitting LaTeX or
    Docbook markup. It is now possible to overwrite this implicitly set
    top-level division via the `top-level-division` command line parameter.

  * Text.Pandoc.Options \[API changes\]:

    + Removed `writerStandalone` field in `WriterOptions`, made
      `writerTemplate` a `Maybe` value. Previously setting
      `writerStandalone = True` did nothing unless a template was provided
      in writerTemplate. Now a fragment will be generated if
      `writerTemplate` is `Nothing`; otherwise, the specified template
      will be used and standalone output generated.
    + `Division` has been renamed `TopLevelDivision` (#3197). The
      `Section`, `Chapter`, and `Part` constructors were renamed to
      `TopLevelSection`, `TopLevelChapter`, and
      `TopLevelPart`, respectively. An additional `TopLevelDefault`
      constructor was added, which is now also the new default value of
      the `writerTopLevelDivision` field in `WriterOptions`.

  * Improved error if they give wrong arg to `--top-level-division`.

  * Use new module from texmath to lookup MS font codepoints in Docx reader.
    Removed unexported module Text.Pandoc.Readers.Docx.Fonts. Its code now
    lives in texmath (0.9).

  * DocBook reader: Fixed xref lookup (#3243). It previously only worked
    when the qnames lacked the docbook namespace URI.

  * HTML reader:

    + Improved table parsing (#3027). We now check explicitly for non-1
      rowspan or colspan attributes, and fail when we encounter them.
      Previously we checked that each row had the same number of cells,
      but that could be true even with rowspans/colspans. And there are
      cases where it isn't true in tables that we can handle fine -- e.g.
      when a tr element is empty. So now we just pad rows with empty cells
      when needed.
    + Treat `<math>` as MathML by default unless something else is
      explicitly specified in xmlns. Provided it parses as MathML,
      of course. Also fixed default which should be to inline math if no
      display attribute is used.
    + Only treat "a" element as link if it has href (#3226). Otherwise
      treat as span.

  * Docx reader (Jesse Rosenthal):

    + Add a placeholder value for CHART. We wrap `[CHART]` in a
      `<span class="chart">`. Note that it maps to inlines because, in
      docx, anything in a drawing tag can be part of a larger paragraph.
    + Be more specific in parsing images We not only want `w:drawing`,
      because that could also include charts. Now we specify
      `w:drawing/pic:pic`. This shouldn't change behavior at all, but it's
      a first step toward allowing other sorts of drawing data as well.
    + Abstract out function to avoid code repetition.
    + Update tests for img title and alt (#3204).
    + Handle Alt text and titles in images. We use the "description" field
      as alt text and the "title" field as title. These can be accessed
      through the "Format Picture" dialog in Word.
    + Docx reader utils: handle empty namespace in `elemName`. Previously,
      if given an empty namespace `(elemName ns "" "foo")` `elemName`
      would output a QName with a `Just ""` namespace. This is never what
      we want. Now we output a `Nothing`. If someone *does* want a
      `Just ""` in the namespace, they can enter the QName
      value explicitly.

  * ODT reader/writer:

    + Inline code when text has a special style (Hubert Plociniczak). When
      a piece of text has a text `Source_Text` then we assume that this is
      a piece of the document that represents a code that needs to
      be inlined. Adapted the writer to also reflect that change.
      Previously it was just writing a 'preformatted' text using a
      non-distinguishable font style. Code blocks are still not recognized
      by the ODT reader. That's a separate issue.
    + Infer table's caption from the paragraph (#3224,
      Hubert Plociniczak). ODT's reader always put empty captions for the
      parsed tables. This commit

        1.  checks paragraphs that follow the table definition
        2.  treats specially a paragraph with a style named 'Table'
        3.  does some postprocessing of the paragraphs that combines tables
            followed immediately by captions

      The ODT writer used the `TableCaption` style for the caption
      paragraph.  This commit follows the OpenOffice approach which allows
      for appending captions to table but uses a built-in style named
      `Table` instead of `TableCaption`. Users of a custom `reference.odt`
      should change the style's name from `TableCaption` to `Table`.

  * ODT reader: Infer tables' header props from rows (#3199,
    Hubert Plociniczak). ODT reader simply provided an empty header list
    which meant that the contents of the whole table, even if not empty, was
    simply ignored. While we still do not infer headers we at least have to
    provide default properties of columns.

  * Markdown reader:

    +   Allow reference link labels starting with `@...` if `citations`
        extension disabled (#3209). Example: in

            \[link text\]\[@a\]

        `link text` isn't hyperlinked because `[@a]` is parsed as
        a citation. Previously this happened whether or not the `citations`
        extension was enabled. Now it happens only if the `citations`
        extension is enabled.
    +   Allow alignments to be specified in Markdown grid tables. For
        example,

            +-------+---------------+--------------------+
            | Right    | Left                 | Centered |
            +=========:+:=================+:=============:+
            | Bananas | $1.34         | built-in wrapper |
            +-------+---------------+--------------------+

    +   Allow Small Caps elements to be created using bracketed spans (as
        they already can be using HTML-syntax spans) (#3191, Kolen Cheung).

  * LaTeX reader:

    + Don't treat `\vspace` and `\hspace` as block commands (#3256).
      Fixed an error which came up, for example, with `\vspace` inside
      a caption. (Captions expect inlines.)
    + Improved table handling. We can now parse all of the tables emitted
      by pandoc in our tests. The only thing we don't get yet are
      alignments and column widths in more complex tables. See #2669.
    + Limited support for minipage.
    + Allow for `[]`s inside LaTeX optional args. Fixes cases like:
    + Handle BVerbatim from fancyvrb (#3203).
    + Handle hungarumlaut (#3201).
    + Allow beamer-style `<...>` options in raw LaTeX (also in Markdown)
      (#3184). This allows use of things like `\only<2,3>{my content}` in
      Markdown that is going to be converted to beamer.

  * Use pre-wrap for code in dzslides template (Nicolas Porcel). Otherwise
    overly long code will appear on every slide.

  * Org reader (Albert Krewinkel):

    +   Respect column width settings (#3246). Table column properties can
        optionally specify a column's width with which it is displayed in
        the buffer. Some exporters, notably the ODT exporter in org-mode
        v9.0, use these values to calculate relative column widths. The org
        reader now implements the same behavior. Note that the org-mode
        LaTeX and HTML exporters in Emacs don't support this feature yet,
        which should be kept in mind by users who use the column
        widths parameters.
    +   Allow HTML attribs on non-figure images (#3222). Images which are
        the only element in a paragraph can still be given HTML attributes,
        even if the image does not have a caption and is hence not a figure.
        The following will add set the `width` attribute of the image to
        `50%`:

            +ATTR\_HTML: :width 50%
            =======================

            \[\[file:image.jpg\]\]

    +   Support `ATTR_HTML` for special blocks (#3182). Special
        blocks (i.e. blocks with unrecognized names) can be prefixed with an
        `ATTR_HTML` block attribute. The attributes defined in that
        meta-directive are added to the `Div` which is used to represent the
        special block.
    +   Support the `todo` export option. The `todo` export option allows to
        toggle the inclusion of TODO keywords in the output. Setting this to
        `nil` causes TODO keywords to be dropped from headlines. The default
        is to include the keywords.
    +   Add support for todo-markers. Headlines can have optional
        todo-markers which can be controlled via the `#+TODO`, `#+SEQ_TODO`,
        or `#+TYP_TODO` meta directive. Multiple such directives can be
        given, each adding a new set of recognized todo-markers. If no
        custom todo-markers are defined, the default `TODO` and `DONE`
        markers are used. Todo-markers are conceptually separate from
        headline text and are hence excluded when autogenerating
        headline IDs. The markers are rendered as spans and labelled with
        two classes: One class is the markers name, the other signals the
        todo-state of the marker (either `todo` or `done`).

  * LaTeX writer:

    + Use `\autocites*` when "suppress-author" citation used.
    + Ensure that simple tables have simple cells (#2666). If cells
      contain more than a single Plain or Para, then we need to set
      nonzero widths and put contents into minipages.
    + Remove invalid inlines in sections (#3218, Hubert Plociniczak).

  * Markdown writer:

    + Fix calculation of column widths for aligned multiline tables
      (#1911, Björn Peemöller). This also fixes excessive CPU and memory
      usage for tables when `--columns` is set in such a way that cells
      must be very tiny. Now cells are guaranteed to be big enough so that
      single words don't need to line break, even if this pushes the line
      length above the column width.
    + Use bracketed form for native spans when `bracketed_spans`
      enabled (#3229).
    + Fixed inconsistent spacing issue (#3232). Previously a tight bullet
      sublist got rendered with a blank line after, while a tight ordered
      sublist did not. Now we don't get the blank line in either case.
    + Fix escaping of spaces in super/subscript (#3225). Previously two
      backslashes were inserted, which gave a literal backslash.
    + Adjust widths in Markdown grid tables so that they match
      on round-trip.

  * Docx writer:

    + Give full detail when there are errors converting tex math.
    + Handle title text in images (Jesse Rosenthal). We already handled
      alt text. This just puts the image "title" into the docx
      "title" attr.
    + Fixed XML markup for empty cells (#3238). Previously the Compact
      style wasn't being applied properly to empty cells.

  * HTML writer:

    + Updated `renderHtml` import from blaze-html.

  * Text.Pandoc.Pretty:

    + Fixed some bugs that caused blank lines in tables (#3251). The bugs
      caused spurious blank lines in grid tables when we had things like
      `blankline $$ blankline`.
    + Add exported function `minOffet` \[API change\] (Björn Peemöller).
    + Added error message for illegal call to `block` (Björn Peemöller).

  * Text.Pandoc.Shared:

    + Put `warn` in MonadIO.
    + `fetchItem`: Better handling of protocol-relative URL (#2635). If
      URL starts with `//` and there is no "base URL" (as there would be
      if a URL were used on the command line), then default to http:.

  * Export Text.Pandoc.getDefaultExtensions \[API change\] (#3178).

  * In --version, trap error in `getAppUserDataDirectory` (#3241). This
    fixes a crash with `pandoc --version` on unusual systems with no real
    user (e.g. SQL Server 2016).

  * Added weigh-pandoc for memory usage diagnostics (#3169).

  * Use correct mime types for woff and woff2 (#3228).

  * Remove make\_travis\_yml.hs (#3235, Kolen Cheung).

  * changelog: Moved an item that was misplaced in the 1.17.2 section to the
    1.18 section where it belongs.

  * CONTRIBUTING.md: minor change in wording and punctuation (#3252,
    Kolen Cheung).

  * Further revisions to manual for `--version` changes (#3244).



## pandoc 1.18 (2016-10-26)

  * Added `--list-input-formats`, `--list-output-formats`,
    `--list-extensions`, `--list-highlight-languages`, and
    `--list-highlight-styles` (#3173).  Removed list of highlighting
    languages from `--version` output.  Removed list of input and output
    formats from default `--help` output.

  * Added `--reference-location=block|section|document` option
    (Jesse Rosenthal).  This determines whether Markdown link references
    and footnotes are placed at the end of the document, the end of the
    section, or the end of the top-level block.

  * Added `--top-level-division=section|chapter|part` (Albert Krewinkel).
    This determines what a level-1 header corresponds to in LaTeX,
    ConTeXt, DocBook, and TEI output.  The default is `section`.
    The `--chapters` option has been deprecated in favor of
    `--top-level-division=chapter`.

  * Added `LineBlock` constructor for `Block` (Albert Krewinkel).  This
    is now used in parsing RST and Markdown line blocks, DocBook
    `linegroup`/`line` combinations, and Org-mode `VERSE` blocks.
    Previously `Para` blocks with hard linebreaks were used.  `LineBlock`s
    are handled specially in the following output formats: AsciiDoc
    (as `[verse]` blocks), ConTeXt (`\startlines`/`\endlines`),
    HTML (`div` with a style), Markdown (line blocks if `line_blocks`
    is enabled), Org-mode (`VERSE` blocks), RST (line blocks). In
    other output formats, a paragraph with hard linebreaks is emitted.

  * Allow binary formats to be written to stdout (but not to tty) (#2677).
    Only works on posix, since we use the unix library to check whether
    output is to tty.  On Windows, pandoc works as before and always requires
    an output file parameter for binary formats.

  * Changed JSON output format (Jesse Rosenthal).  Previously we used
    generically generated JSON, but this was subject to change depending
    on the version of aeson pandoc was compiled with.  To ensure stability,
    we switched to using manually written ToJSON and FromJSON
    instances, and encoding the API version.  **Note:**  pandoc filter
    libraries will need to be revised to handle the format change.
    Here is a summary of the essential changes:

    + The toplevel JSON format is now `{"pandoc-api-version" :
      [MAJ, MIN, REV], "meta" : META, "blocks": BLOCKS}`
      instead of `[{"unMeta": META}, [BLOCKS]]`.
      Decoding fails if the major and minor version numbers don't
      match.
    + Leaf nodes no longer have an empty array for their "c" value.
      Thus, for example, a `Space` is encoded as `{"t":"Space"}`
      rather than `{"t":"Space","c":[]}` as before.

  * Removed `tests/Tests/Arbitrary.hs` and added a `Text.Pandoc.Arbitrary`
    module to pandoc-types (Jesse Rosenthal).  This makes it easier
    to use QuickCheck with pandoc types outside of pandoc itself.

  * Add `bracketed_spans` Markdown extension, enabled by default
    in pandoc `markdown`.  This allows you to create a native span
    using this syntax:  `[Here is my span]{#id .class key="val"}`.

  * Added `angle_brackets_escapable` Markdown extension (#2846).
    This is needed because github flavored Markdown has a slightly
    different set of escapable symbols than original Markdown;
    it includes angle brackets.

  * Export `Text.Pandoc.Error` in `Text.Pandoc` [API change].

  * Print highlighting-kate version in `--version`.

  * `Text.Pandoc.Options`:

    + `Extension` has new constructors `Ext_brackted_spans` and
      `Ext_angle_brackets_escapable` [API change].
    + Added `ReferenceLocation` type [API change] (Jesse Rosenthal).
    + Added `writerReferenceLocation` field to `WriterOptions` (Jesse
      Rosenthal).

  * `--filter`:  we now check `$DATADIR/filters` for filters before
    looking in the path (#3127, Jesse Rosenthal, thanks to Jakob
    Voß for the idea).  Filters placed in this directory need not
    be executable; if the extension is `.hs`, `.php`, `.pl`, `.js`,
    or `.rb`, pandoc will run the right interpreter.

  * For `--webtex`, replace deprecated Google Chart API by CodeCogs as
    default (Kolen Cheung).

  * Removed `raw_tex` extension from `markdown_mmd` defaults (Kolen Cheung).

  * Execute .js filters with node (Jakob Voß).

  * Textile reader:

    + Support `bc..` extended code blocks (#3037).  Also, remove trailing
      newline in code blocks (consistently with Markdown reader).
    + Improve table parsing.  We now handle cell and row attributes, mostly
      by skipping them.  However, alignments are now handled properly.
      Since in pandoc alignment is per-column, not per-cell, we
      try to devine column alignments from cell alignments.
      Table captions are also now parsed, and textile indicators
      for thead and tfoot no longer cause parse failure.  (However,
      a row designated as tfoot will just be a regular row in pandoc.)
    + Improve definition list parsing.  We now allow multiple terms
      (which we concatenate with linebreaks).  An exponential parsing
      bug (#3020) is also fixed.
    + Disallow empty URL in explicit link (#3036).

  * RST reader:

    + Use Div instead of BlockQuote for admonitions (#3031).
      The Div has class `admonition` and (if relevant) one of the
      following:  `attention`, `caution`, `danger`, `error`, `hint`,
      `important`, `note`, `tip`, `warning`.  **Note:** This will change
      the rendering of some RST documents!  The word ("Warning", "Attention",
      etc.) is no longer added; that must be done with CSS or a filter.
    + A Div is now used for `sidebar` as well.
    + Skip whitespace before note (Jesse Rosenthal, #3163).  RST requires a
      space before a footnote marker. We discard those spaces so that footnotes
      will be adjacent to the text that comes before it. This is in line with
      what rst2latex does.
    + Allow empty lines when parsing line blocks (Albert Krewinkel).

  * Markdown reader:

    + Allow empty lines when parsing line blocks (Albert Krewinkel).
    + Allow attributes on autolinks (#3183, Daniele D'Orazio).

  * LaTeX reader:

    + More robust parsing of unknown environments (#3026).
      We no longer fail on things like `^` inside options for tikz.
    + Be more forgiving of non-standard characters, e.g. `^` outside of math.
      Some custom environments give these a meaning, so we should try not to
      fall over when we encounter them.
    + Drop duplicate `*` in bibtexKeyChars (Albert Krewinkel)

  * MediaWiki reader:

    + Fix for unquoted attribute values in mediawiki tables (#3053).
      Previously an unquoted attribute value in a table row
      could cause parsing problems.
    + Improved treatment of verbatim constructions (#3055).
      Previously these yielded strings of alternating Code and Space
      elements; we now incorporate the spaces into the Code.  Emphasis
      etc. is still possible inside these.
    + Properly interpret XML tags in pre environments (#3042).  They are meant
      to be interpreted as literal text.

  * EPUB reader:  don't add root path to data: URIs (#3150).
    Thanks to @lep for the bug report and patch.

  * Org reader (Albert Krewinkel):

    + Preserve indentation of verse lines (#3064).  Leading spaces in verse
      lines are converted to non-breaking spaces, so indentation is preserved.
    + Ensure image sources are proper links.  Image sources as those in plain
      images, image links, or figures, must be proper URIs or relative file
      paths to be recognized as images.  This restriction is now enforced
      for all image sources.  This also fixes the reader's usage of uncleaned
      image sources, leading to `file:` prefixes not being deleted from
      figure images.  Thanks to @bsag for noticing this bug.
    + Trim verse lines properly (Albert Krewinkel).
    + Extract meta parsing code to module.  Parsing of meta-data is well
      separable from other block parsing tasks.  Moving into new module to
      get small files and clearly arranged code.
    + Read markup only for special meta keys.  Most meta-keys should be read
      as normal string values, only a few are interpreted as marked-up text.
    + Allow multiple, comma-separated authors.  Multiple authors can be
      specified in the `#+AUTHOR` meta line if they are given as a
      comma-separated list.
    + Give precedence to later meta lines.  The last meta-line of any given
      type is the significant line.  Previously the value of the first line
      was kept, even if more lines of the same type were encountered.
    + Read LaTeX_header as header-includes.  LaTeX-specific header commands
      can be defined in `#+LaTeX_header` lines.  They are parsed as
      format-specific inlines to ensure that they will only show up in LaTeX
      output.
    + Set documentclass meta from LaTeX_class.
    + Set classoption meta from LaTeX_class_options.
    + Read HTML_head as header-includes.  HTML-specific head content can be
      defined in `#+HTML_head` lines.  They are parsed as format-specific
      inlines to ensure that they will only show up in HTML output.
    + Respect `author` export option.  The `author` option controls whether
      the author should be included in the final markup.  Setting
      `#+OPTIONS: author:nil` will drop the author from the final meta-data
      output.
    + Respect `email` export option.  The `email` option controls whether the
      email meta-field should be included in the final markup. Setting
      `#+OPTIONS: email:nil` will drop the email field from the final
      meta-data output.
    + Respect `creator` export option.  The `creator` option controls whether
      the creator meta-field should be included in the final markup.  Setting
      `#+OPTIONS: creator:nil` will drop the creator field from the final
      meta-data output.  Org-mode recognizes the special value `comment` for
      this field, causing the creator to be included in a comment.  This is
      difficult to translate to Pandoc internals and is hence interpreted the
      same as other truish values (i.e. the meta field is kept if it's
      present).
    + Respect unnumbered header property (#3095).  Sections the `unnumbered`
      property should, as the name implies, be excluded from the automatic
      numbering of section provided by some output formats.  The Pandoc
      convention for this is to add an "unnumbered" class to the header.  The
      reader treats properties as key-value pairs per default, so a special
      case is added to translate the above property to a class instead.
    + Allow figure with empty caption (Albert Krewinkel, #3161).
      A `#+CAPTION` attribute before an image is enough to turn an image into
      a figure. This wasn't the case because the `parseFromString` function,
      which processes the caption value, would fail on empty values. Adding
      a newline character to the caption value fixes this.

  * Docx reader:

    + Use XML convenience functions (Jesse Rosenthal).
      The functions `isElem` and `elemName` (defined in Docx/Util.hs) make
      the code a lot cleaner than the original XML.Light functions, but they
      had been used inconsistently. This puts them in wherever applicable.
    + Handle anchor spans with content in headers.  Previously, we would only
      be able to figure out internal links to a header in a docx if the
      anchor span was empty. We change that to read the inlines out of the
      first anchor span in a header.
    + Let headers use existing id.  Previously we always generated an id for
      headers (since they wouldn't bring one from Docx). Now we let it use an
      existing one if possible. This should allow us to recurs through anchor
      spans.
    + Use all anchor spans for header ids.  Previously we only used the first
      anchor span to affect header ids. This allows us to use all the anchor
      spans in a header, whether they're nested or not (#3088).
    + Test for nested anchor spans in header.  This ensures that anchor spans
      in header with content (or with other anchor spans inside) will resolve
      to links to a header id properly.

  * ODT reader (Hubert Plociniczak)

    + Include list's starting value.  Previously the starting value of
      the lists' items has been hardcoded to 1. In reality ODT's list
      style definition can provide a new starting value in one of its
      attributes.
    + Infer caption from the text following the image.
      Frame can contain other frames with the text boxes.
    + Add `fig:` to title for Image with a caption (as expected
      by pandoc's writers).
    + Basic support for images in ODT documents.
    + Don't duplicate text for anchors (#3143).  When creating an anchor
      element we were adding its representation as well as the original
      content, leading to text duplication.

  * DocBook writer:

    + Include an anchor element when a div or span has an id (#3102).
      Note that DocBook does not have a class attribute, but at least this
      provides an anchor for internal links.

  * LaTeX writer:

    + Don't use * for unnumbered paragraph, subparagraph.  The starred
      variants don't exist.  This helps with part of #3058...it gets rid of
      the spurious `*`s.  But we still have numbers on the 4th and 5th level
      headers.
    + Properly escape backticks in verbatim (#3121, Jesse Rosenthal).
      Otherwise they can cause unintended ligatures like `` ?` ``.
    + Handle NARRAOW NO-BREAK SPACE into LaTeX (Vaclav Zeman) as `\,`.
    + Don't include `[htbp]` placement for figures (#3103, Václav Haisman).
      This allows figure placement defaults to be changed by the user
      in the template.

  * TEI writer: remove heuristic to detect book template (Albert Krewinkel).
    TEI doesn't have `<book>` elements but only generic `<divN>` division
    elements. Checking the template for a trailing `</book>` is nonsensical.

  * MediaWiki writer:  transform filename with underscores in images (#3052).
    `foo bar.jpg` becomes `foo_bar.jpg`. This was already done
    for internal links, but it also needs to happen for images.

  * ICML writer: replace partial function (!!) in table handling (#3175,
    Mauro Bieg).

  * Man writer: allow section numbers that are not a single digit (#3089).

  * AsciiDoc writer: avoid unnecessary use of "unconstrained" emphasis
    (#3068).  In AsciiDoc, you must use a special form of emphasis
    (double `__`) for intraword emphasis.  Pandoc was previously using
    this more than necessary.

  * EPUB writer:  use stringify instead of plain writer for metadata
    (#3066).  This means that underscores won't be used for emphasis,
    or CAPS for bold.  The metadata fields will just have unadorned
    text.

  * Docx Writer:

    + Implement user-defined styles (Jesse Rosenthal).  Divs and Spans
      with a `custom-style` key in the attributes will apply the corresponding
      key to the contained blocks or inlines.
    + Add ReaderT env to the docx writer (Jesse Rosenthal).
    + Clean up and streamline RTL behavior (Jesse Rosenthal, #3140).
      You can set `dir: rtl` in YAML metadata, or use `-M dir=rtl`
      on the command line.  For finer-grained control, you can set
      the `dir` attribute in Div or Span elements.

  * Org writer (Albert Krewinkel):

    + Remove blank line after figure caption.  Org-mode only treats an image
      as a figure if it is directly preceded by a caption.
    + Ensure blank line after figure.  An Org-mode figure should be surrounded
      by blank lines.  The figure would be recognized regardless, but images
      in the following line would unintentionally be treated as figures as
      well.
    + Ensure link targets are paths or URLs.  Org-mode treats links as
      document internal searches unless the link target looks like a URL or
      file path, either relative or absolute.  This change ensures that this
      is always the case.
    + Translate language identifiers.  Pandoc and Org-mode use different
      programming language identifiers.  An additional translation between
      those identifiers is added to avoid unexpected behavior.  This fixes a
      problem where language specific source code would sometimes be output
      as example code.
    + Drop space before footnote markers (Albert Krewinkel, #3162).
      The writer no longer adds an extra space before footnote markers.

  * Markdown writer:

    + Don't emit HTML for tables unless `raw_html` extension is set (#3154).
      Emit `[TABLE]` if no suitable table formats are enabled and raw HTML
      is disabled.
    + Check for the `raw_html` extension before emitting a raw HTML block.
    + Abstract out note/ref function (Jesse Rosenthal).
    + Add ReaderT monad for environment variables (Jesse Rosenthal).

  * HTML, EPUB, slidy, revealjs templates: Use `<p>` instead of `<h1>` for
    subtitle, author, date (#3119).  Note that, as a result of this change,
    authors may need to update CSS.

  * revealjs template:  Added `notes-server` option
    (jgm/pandoc-templates#212, Yoan Blanc).

  * Beamer template:

    + Restore whitespace between paragraphs. This was
      a regression in the last release (jgm/pandoc-templates#207).
    + Added `themeoptions` variable (Carsten Gips).
    + Added `beamerarticle` variable.  This causes the `beamerarticle`
      package to be loaded in beamer, to produce an article from beamer
      slides.  (Carsten Gips)
    + Added support for `fontfamilies` structured variable
      (Artem Klevtsov).
    + Added hypersetup options (Jake Zimmerman).

  * LaTeX template:

    + Added dummy definition for `\institute`.
      This isn't a standard command, and we want to avoid a crash when
      `institute` is used with the default template.
    + Define default figure placement (Václav Haisman), since pandoc
      no longer includes `[htbp]` for figures.  Users with custom templates
      will want to add this. See #3103.
    + Use footnote package to fix notes in tables (jgm/pandoc-templates#208,
      Václav Haisman).

  * Moved template compiling/rendering code to a separate library.
    `doctemplates`.  This allows the pandoc templating system to be
    used independently.

  * Text.Pandoc.Error: Fix out of index error in `handleError`
    (Matthew Pickering).  The fix is to not try to show the exact line when
    it would cause an out-of-bounds error as a result of included files.

  * Text.Pandoc.Shared: Add `linesToBlock` function (Albert Krewinkel).

  * Text.Pandoc.Parsing.emailAddress: tighten up parsing of email
    addresses.  Technically `**@user` is a valid email address, but if we
    allow things like this, we get bad results in markdown flavors
    that autolink raw email addresses (see #2940).  So we exclude a few
    valid email addresses in order to avoid these more common bad cases.

  * Text.Pandoc.PDF:  Don't crash with nonexistent image (#3100).  Instead,
    emit the alt text, emphasized.  This accords with what the ODT writer
    currently does.  The user will still get a warning about a nonexistent
    image.

  * Fix example in API documentation (#3176, Thomas Weißschuh).

  * Tell where to get tarball in INSTALL (#3062).

  * Rename README to MANUAL.txt and add GitHub-friendly README.md
    (Albert Krewinkel, Kolen Cheung).

  * Replace COPYING with Markdown version COPYING.md from GNU (Kolen Cheung).

  * MANUAL.txt:

    + Put note on structured vars in separate paragraph (#2148, Albert
      Krewinkel).  Make it clearer that structured author variables require a
      custom template
    + Note that `--katex` works best with `html5` (#3077).
    + Fix the LaTeX and EPUB links in manual (Morton Fox).
    + Document `biblio-title` variable.

  * Improve spacing of footnotes in `--help` output (Waldir Pimenta).

  * Update KaTeX to v0.6.0 (Kolen Cheung).

  * Allow latest dependencies.

  * Use texmath 0.8.6.6 (#3040).

  * Allow http-client 0.4.30, which is the version in stackage lts.
    Previously we required 0.5.
    Remove CPP conditionals for earlier versions.

  * Remove support for GHC < 7.8 (Jesse Rosenthal).

    + Remove Compat.Monoid.
    + Remove an inline monad compatibility macro.
    + Remove Text.Pandoc.Compat.Except.
    + Remove directory compat.
    + Change constraint on mtl.
    + Remove unnecessary CPP condition in UTF8.
    + Bump base lower bound to 4.7.
    + Remove 7.6 build from .travis.yaml.
    + Bump supported ghc version in CONTRIBUTING.md.
    + Add note about GHC version support to INSTALL.
    + Remove GHC 7.6 from list of tested versions (Albert Krewinkel).
    + Remove TagSoup compat.
    + Add EOL note to time compat module.  Because time 1.4 is a boot library
      for GHC 7.8, we will support the compatibility module as long as we
      support 7.8. But we should be clear about when we will no longer need
      it.
    + Remove blaze-html CPP conditional.
    + Remove unnecessary CPP in custom Prelude.

## pandoc 1.17.2 (2016-07-17)

  * Added Zim Wiki writer, template and tests. `zimwiki` is now
    a valid output format. (Alex Ivkin)

  * Changed email-obfuscation default to no obfuscation (#2988).
    + `writerEmailObfuscation` in `defaultWriterOptions` is now
      `NoObfuscation`.
    + the default for the command-line `--email-obfuscation` option is
      now `none`.

  * Docbook writer: Declare xlink namespace in Docbook5 output (Ivo Clarysse).

  * Org writer:

    + Support arbitrary raw inlines (Albert Krewinkel).
      Org mode allows arbitrary raw inlines ("export snippets" in Emacs
      parlance) to be included as `@@format:raw foreign format text@@`.
    + Improve Div handling (Albert Krewinkel).  Div blocks handling is
      changed to make the output look more like idiomatic org mode:
        - Div-wrapped content is output as-is if the div's attribute is the
          null attribute.
        - Div containers with an id but neither classes nor key-value pairs
          are unwrapped and the id is added as an anchor.
        - Divs with classes associated with greater block elements are
          wrapped in a `#+BEGIN`...`#+END` block.
        - The old behavior for Divs with more complex attributes is kept.

  * HTML writer:  Better support for raw LaTeX environments (#2758).
    Previously we just passed all raw TeX through when MathJax
    was used for HTML math.  This passed through too much.
    With this patch, only raw LaTeX environments that MathJax
    can handle get passed through.
    This patch also causes raw LaTeX environments to be treated
    as math, when possible, with MathML and WebTeX output.

  * Markdown writer:  use raw HTML for simple, pipe tables with linebreaks
    (#2993).  Markdown line breaks involve a newline, and simple and pipe
    tables can't contain one.

  * Make --webtex work with the Markdown writer (#1177).
    This is a convenient option for people using
    websites whose Markdown flavors don't provide for math.

  * Docx writer:

    + Set paragraph to FirstPara after display math (Jesse Rosenthal).
      We treat display math like block quotes, and apply FirstParagraph style
      to paragraphs that follow them. These can be styled as the user
      wishes. (But, when the user is using indentation, this allows for
      paragraphs to continue after display math without indentation.)
    + Use actual creation time as doc prop (Jesse Rosenthal).
      Previously, we had used the user-supplied date, if available, for Word's
      document creation metadata. This could lead to weird results, as in
      cases where the user post-dates a document (so the modification might be
      prior to the creation). Here we use the actual computer time to set the
      document creation.

  * LaTeX writer:

    + Don't URI-escape image source (#2825).  Usually this is a local file,
      and replacing spaces with `%20` ruins things.
    + Allow 'standout' as a beamer frame option (#3007).
      `## Slide title {.standout}`.

  * RST reader: Fixed links with no explicit link text.  The link
    `` `<foo>`_ `` should have `foo` as both its link text and its URL.
    See RST spec at <http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#embedded-uris-and-aliases>
    Closes Debian #828167 -- reported by Christian Heller.

  * Textile reader:

    + Fixed attributes (#2984).  Attributes can't be followed by
      a space.  So, `_(class)emph_` but `_(noclass) emph_`.
    + Fixed exponential parsing bug (#3020).
    + Fix overly aggressive interpretation as images (#2998).
      Spaces are not allowed in the image URL in textile.

  * LaTeX reader:

    + Fix `\cite` so it is a NormalCitation not AuthorInText.
    + Strip off double quotes around image source if present (#2825).
      Avoids interpreting these as part of the literal filename.

  * Org reader:

    + Add semicolon to list of special chars (Albert Krewinkel)
      Semicolons are used as special characters in citations syntax.  This
      ensures the correct parsing of Pandoc-style citations: `[prefix; @key;
      suffix]`.  Previously, parsing would have failed unless there was a space
      or other special character as the last <prefix> character.
    + Add support for "Berkeley-style" cites (Albert Krewinkel, #1978).
      A specification for an official Org-mode citation syntax was drafted by
      Richard Lawrence and enhanced with the help of others on the orgmode
      mailing list.  Basic support for this citation style is added to the
      reader.
    + Support arbitrary raw inlines (Albert Krewinkel).
      Org mode allows arbitrary raw inlines ("export snippets" in Emacs
      parlance) to be included as `@@format:raw foreign format text@@`.
    + Remove partial functions (Albert Krewinkel, #2991).
      Partial functions like `head` lead to avoidable errors and should be
      avoided.  They are replaced with total functions.
    + Support figure labels (Albert Krewinkel, #2496, #2999).
      Figure labels given as `#+LABEL: thelabel` are used as the ID of the
      respective image.  This allows e.g. the LaTeX to add proper `\label`
      markup.
    + Improve tag and properties type safety (Albert Krewinkel).
      Specific newtype definitions are used to replace stringly typing of tags
      and properties.  Type safety is increased while readability is improved.
    + Parse as headlines, convert to blocks (Albert Krewinkel).
      Emacs org-mode is based on outline-mode, which treats documents as trees
      with headlines are nodes.  The reader is refactored to parse into a
      similar tree structure.  This simplifies transformations acting on
      document (sub-)trees.
    * Refactor comment tree handling (Albert Krewinkel).
      Comment trees were handled after parsing, as pattern matching on lists
      is easier than matching on sequences.  The new method of reading
      documents as trees allows for more elegant subtree removal.
    * Support archived trees export options (Albert Krewinkel).
      Handling of archived trees can be modified using the `arch` option.
      Archived trees are either dropped, exported completely, or collapsed to
      include just the header when the `arch` option is nil, non-nil, or
      `headline`, respectively.
    * Put export setting parser into module (Albert Krewinkel).
      Export option parsing is distinct enough from general block parsing to
      justify putting it into a separate module.
    * Support headline levels export setting (Albert Krewinkel).
      The depths of headlines can be modified using the `H` option.  Deeper
      headlines will be converted to lists.
    * Replace ugly code with view pattern (Albert Krewinkel).
      Some less-than-smart code required a pragma switching of overlapping
      pattern warnings in order to compile seamlessly.  Using view patterns
      makes the code easier to read and also doesn't require overlapping
      pattern checks to be disabled.
    * Fix parsing of verbatim inlines (Albert Krewinkel, #3016).
      Org rules for allowed characters before or after markup chars were not
      checked for verbatim text.  This resultet in wrong parsing outcomes of
      if the verbatim text contained e.g. space enclosed markup characters as
      part of the text (`=is_substr = True=`).  Forcing the parser to update
      the positions of allowed/forbidden markup border characters fixes this.

  * LaTeX template: fix for obscure hyperref/xelatex issue.
    Here's a minimal case:

        \documentclass[]{article}
        \usepackage{hyperref}
        \begin{document}
        \section{\%á}
        \end{document}

    Without this change, this fails on the second invocation of xelatex.
    This affects inputs this like `# %á` with pdf output via xelatex.

  * trypandoc:  call results 'html' instead of 'result'.
    This is for better compatibility with babelmark2.

  * Document MultiMarkdown as input/output format (Albert Krewinkel, #2973).
    MultiMarkdown was only mentioned as a supported Markdown dialect but not
    as a possible input or output format.  A brief mention is added
    everywhere the other supported markdown dialects are mentioned.

  * Document Org mode as a format containing raw HTML (Albert Krewinkel)
    Raw HTML is kept when the output format is Emacs Org mode.

  * Implement `RawInline` and `RawBlock` in sample lua custom writer (#2985).

  * Text.Pandoc.Shared:

    + Introduce blocksToInlines function (Jesse Rosenthal).
      This is a lossy function for converting `[Block] -> [Inline]`. Its main
      use, at the moment, is for docx comments, which can contain arbitrary
      blocks (except for footnotes), but which will be converted to spans.
      This is, at the moment, pretty useless for everything but the basic
      `Para` and `Plain` comments. It can be improved, but the docx reader
      should probably emit a warning if the comment contains more than this.
    + Add BlockQuote to blocksToInlines (Jesse Rosenthal).
    + Add further formats for `normalizeDate` (Jesse Rosenthal).
      We want to avoid illegal dates -- in particular years with greater than
      four digits. We attempt to parse series of digits first as `%Y%m%d`, then
      `%Y%m`, and finally `%Y`.
    + `normalizeDate` should reject illegal years (Jesse Rosenthal).
      We only allow years between 1601 and 9999, inclusive. The ISO 8601
      actually says that years are supposed to start with 1583, but MS Word
      only allows 1601-9999. This should stop corrupted word files if the date
      is out of that range, or is parsed incorrectly.
    + Improve year sanity check in normalizeDate (Jesse Rosenthal).
      Previously we parsed a list of dates, took the first one, and then
      tested its year range. That meant that if the first one failed, we
      returned nothing, regardless of what the others did. Now we test for
      sanity before running `msum` over the list of Maybe values. Anything
      failing the test will be Nothing, so will not be a candidate.

  * Docx reader:

    + Add simple comment functionality. (Jesse Rosenthal).
      This adds simple track-changes comment parsing to the docx reader. It is
      turned on with `--track-changes=all`. All comments are converted to
      inlines, which can list some information. In the future a warning will be
      added for comments with formatting that seems like it will be excessively
      denatured.  Note that comments can extend across blocks. For that reason
      there are two spans: `comment-start` and `comment-end`.  `comment-start`
      will contain the comment. `comment-end` will always be empty.  The two
      will be associated by a numeric id.
    + Enable warnings in top-level reader (Jesse Rosenthal).
      Previously we had only allowed for warnings in the parser. Now we allow
      for them in the `Docx.hs` as well. The warnings are simply concatenated.
    + Add warning for advanced comment formatting. (Jesse Rosenthal).
      We can't guarantee we'll convert every comment correctly, though we'll
      do the best we can. This warns if the comment includes something other
      than Para or Plain.
    + Add tests for warnings. (Jesse Rosenthal).
    + Add tests for comments (Jesse Rosenthal).
      We test for comments, using all track-changes options. Note that we
      should only output comments if `--track-changes=all`. We also test for
      emitting warnings if there is complicated formatting.

  * README: update to include track-changes comments. (Jesse Rosenthal)

  * Improved Windows installer - don't ignore properties set on command-line.
    See #2708.  Needs testing to see if this resolves the issue.
    Thanks to @nkalvi.

  * Process markdown extensions on command line in L->R order (#2995).
    Previously they were processed, very unintuitively, in R->L
    order, so that `markdown-tex_math_dollars+tex_math_dollars`
    had `tex_math_dollars` disabled.

  * Added `secnumdepth` variable to LaTeX template (#2920).

  * Include table of contents in README.html in Windows package.

  * Writers: treat SoftBreak as space for stripping (Jesse Rosenthal)
    In Writers.Shared, we strip leading and trailing spaces for display
    math. Since SoftBreak's are treated as spaces, we should strip those
    too.

  * beamer, latex templates:  pass biblatexoptions directly in package load.
    This allows runtime optinos to be used.  Fixes jgm/pandoc-citeproc#201

  * CPP workaround for deprecation of `parseUrl` in http-client.

  * Removed some redundant class constraints.

  * make_oxs_package.sh - use OSX env variable.

  * Added `winpkg` target to Makefile.  This downloads the windows package
    from appveyor and signs it using the key.

  * Document Org mode as a format containing raw TeX (Albert Krewinkel).
    Raw TeX is kept verbatim when the output format is Emacs Org mode.

  * Support math with haddock-library >= 1.4.

  * Removed `-rtsopts` from library stanza.  It has no effect, and Hackage
    wouldn't accept the package.

  * Update library dependency versions.

## pandoc 1.17.1 (2016-06-04)

  * New output format: `docbook5` (Ivo Clarysse).

  * `Text.Pandoc.Options`: Add `writerDocBook5` to `WriterOptions`
    (API change).

  * Org writer:

    + Add :PROPERTIES: drawer support (Albert Krewinkel, #1962).
      This allows header attributes to be added to org documents in the form
      of `:PROPERTIES:` drawers.  All available attributes are stored as
      key/value pairs.  This reflects the way the org reader handles
      `:PROPERTIES:` blocks.
    + Add drawer capability (Carlos Sosa).  For the implementation of the
      Drawer element in the Org Writer, we make use of a generic Block
      container with attributes.  The presence of a `drawer` class defines
      that the `Div` constructor is a drawer. The first class defines the
      drawer name to use.  The key-value list in the attributes defines
      the keys to add inside the Drawer.  Lastly, the list of Block elements
      contains miscellaneous blocks elements to add inside of the Drawer.
    + Use `CUSTOM_ID` in properties (Albert Krewinkel).  The `ID` property is
      reserved for internal use by Org-mode and should not be used.
      The `CUSTOM_ID` property is to be used instead, it is converted to the
      `ID` property for certain export format.

  * LaTeX writer:

    + Ignore `--incremental` unless output format is beamer (#2843).
    + Fix polyglossia to babel env mapping (Mauro Bieg, #2728).
      Allow for optional argument in square brackets.
    + Recognize `la-x-classic` as Classical Latin (Andrew Dunning).
      This allows one to access the hyphenation patterns in CTAN's
      hyph-utf8.
    + Add missing languages from hyph-utf8 (Andrew Dunning).
    + Improve use of `\strut` with `\minipage` inside tables
      (Jose Luis Duran).  This improves spacing in multiline
      tables.
    + Use `{}` around options containing special chars (#2892).
    + Avoid lazy `foldl`.
    + Don't escape underscore in labels (#2921).  Previously they were
      escaped as `ux5f`.
    + brazilian -> brazil for polyglossia (#2953).

  * HTML writer: Ensure mathjax link is added when math appears in footnote
    (#2881).  Previously if a document only had math in a footnote, the
    MathJax link would not be added.

  * EPUB writer: set `navpage` variable on nav page.
    This allows templates to treat it differently.

  * DocBook writer:

    + Use docbook5 if `writerDocbook5` is set (Ivo Clarysse).
    + Properly handle `ulink`/`link` (Ivo Clarysse).

  * EPUB reader:

    + Unescape URIs in spine (#2924).
    + EPUB reader:  normalise link id (Mauro Bieg).

  * Docx Reader:

    + Parse `moveTo` and `moveFrom` (Jesse Rosenthal).
      `moveTo` and `moveFrom` are track-changes tags that are used when a
      block of text is moved in the document. We now recognize these tags and
      treat them the same as `insert` and `delete`, respectively. So,
      `--track-changes=accept` will show the moved version, while
      `--track-changes=reject` will show the original version.
    + Tests for track-changes moving (Jesse Rosenthal).

  * ODT, EPUB, Docx readers: throw `PandocError` on unzip failure
    (Jesse Rosenthal) Previously, `readDocx`, `readEPUB`, and `readOdt`
    would error out if zip-archive failed. We change the archive extraction
    step from `toArchive` to `toArchiveOrFail`, which returns an Either value.

  * Markdown, HTML readers:  be more forgiving about unescaped `&` in
    HTML (#2410).  We are now more forgiving about parsing invalid HTML with
    unescaped `&` as raw HTML.  (Previously any unescaped `&`
    would cause pandoc not to recognize the string as raw HTML.)

  * Markdown reader:

    + Fix pandoc title blocks with lines ending in 2 spaces (#2799).
    + Added `-s` to markdown-reader-more test.

  * HTML reader: fixed bug in `pClose`.  This caused exponential parsing
    behavior in documnets with unclosed tags in `dl`, `dd`, `dt`.

  * MediaWiki reader: Allow spaces before `!` in MediaWiki table header
    (roblabla).

  * RST reader: Support `:class:` option for code block in RST reader
    (Sidharth Kapur).

  * Org reader (all Albert Krewinkel, except where noted otherwise):

    + Stop padding short table rows.
      Emacs Org-mode doesn't add any padding to table rows.  The first
      row (header or first body row) is used to determine the column count,
      no other magic is performed.
    + Refactor rows-to-table conversion.  This refactors
      the codes conversing a list table lines to an org table ADT.
      The old code was simplified and is now slightly less ugly.
    + Fix handling of empty table cells, rows (Albert Krewinkel, #2616).
      This fixes Org mode parsing of some corner cases regarding empty cells
      and rows.  Empty cells weren't parsed correctly, e.g. `|||` should be
      two empty cells, but would be parsed as a single cell containing a pipe
      character.  Empty rows where parsed as alignment rows and dropped from
      the output.
    + Fix spacing after LaTeX-style symbols.
      The org-reader was dropping space after unescaped LaTeX-style symbol
      commands: `\ForAll \Auml` resulted in `∀Ä` but should give `∀ Ä`
      instead.  This seems to be because the LaTeX-reader treats the
      command-terminating space as part of the command.  Dropping the trailing
      space from the symbol-command fixes this issue.
    + Print empty table rows.  Empty table rows should not
      be dropped from the output, so row-height is always set to be at least 1.
    + Move parser state into separate module.
      The org reader code has become large and confusing.  Extracting smaller
      parts into submodules should help to clean things up.
    + Add support for sub/superscript export options.
      Org-mode allows to specify export settings via `#+OPTIONS` lines.
      Disabling simple sub- and superscripts is one of these export options,
      this options is now supported.
    + Support special strings export option Parsing of special strings
      (like `...` as ellipsis or `--` as en dash) can be toggled using the `-`
      option.
    + Support emphasized text export option.  Parsing of emphasized text can
      be toggled using the `*` option.  This influences parsing of text marked
      as emphasized, strong, strikeout, and underline.  Parsing of inline math,
      code, and verbatim text is not affected by this option.
    + Support smart quotes export option.  Reading of smart quotes can be
      toggled using the `'` option.
    + Parse but ignore export options. All known export options are parsed
      but ignored.
    + Refactor block attribute handling.  A parser state attribute was used
      to keep track of block attributes defined in meta-lines.  Global state
      is undesirable, so block attributes are no longer saved as part of the
      parser state.  Old functions and the respective part of the parser state
      are removed.
    + Use custom `anyLine`.  Additional state changes need to be made after
      a newline is parsed, otherwise markup may not be recognized correctly.
      This fixes a bug where markup after certain block-types would not be
      recognized.
    + Add support for `ATTR_HTML` attributes (#1906).
      Arbitrary key-value pairs can be added to some block types using a
      `#+ATTR_HTML` line before the block.  Emacs Org-mode only includes these
      when exporting to HTML, but since we cannot make this distinction here,
      the attributes are always added.  The functionality is now supported
      for figures.
    + Add `:PROPERTIES:` drawer support (#1877).
      Headers can have optional `:PROPERTIES:` drawers associated with them.
      These drawers contain key/value pairs like the header's `id`.  The
      reader adds all listed pairs to the header's attributes; `id` and
      `class` attributes are handled specially to match the way `Attr` are
      defined.  This also changes behavior of how drawers of unknown type
      are handled.  Instead of including all unknown drawers, those are not
      read/exported, thereby matching current Emacs behavior.
    + Use `CUSTOM_ID` in properties.  See above on Org writer changes.
    + Respect drawer export setting.  The `d` export option can be used
      to control which drawers are exported and which are discarded.
      Basic support for this option is added here.
    + Ignore leading space in org code blocks (Emanuel Evans, #2862).
      Also fix up tab handling for leading whitespace in code blocks.
    + Support new syntax for export blocks.  Org-mode version 9
      uses a new syntax for export blocks.  Instead of `#+BEGIN_<FORMAT>`,
      where `<FORMAT>` is the format of the block's content, the new
      format uses `#+BEGIN_export <FORMAT>` instead.  Both types are
      supported.
    + Refactor `BEGIN...END` block parsing.
    + Fix handling of whitespace in blocks, allowing content to be indented
      less then the block header.
    + Support org-ref style citations.  The *org-ref* package is an
      org-mode extension commonly used to manage citations in org
      documents.  Basic support for the `cite:citeKey` and
      `[[cite:citeKey][prefix text::suffix text]]` syntax is added.
    + Split code into separate modules, making for cleaner code and
      better decoupling.

  * Added `docbook5` template.

  * `--mathjax` improvements:

    + Use new CommonHTML output for MathJax (updated default MathJax URL,
      #2858).
    + Change default mathjax setup to use `TeX-AMS_CHTML` configuration.
      This is designed for cases where the input is always TeX and maximal
      conformity with TeX is desired.  It seems to be smaller and load faster
      than what we used before.  See #2858.
    + Load the full MathJax config to maximize loading speed (KolenCheung).

  * Bumped upper version bounds to allow use of latest packages
    and compilation with ghc 8.

  * Require texmath 0.8.6.2.  Closes several texmath-related bugs (#2775,
    #2310, #2310, #2824).  This fixes behavior of roots, e.g.
    `\sqrt[3]{x}`, and issues with sub/superscript positioning
    and matrix column alignment in docx.

  * README:

    + Clarified documentation of `implicit_header_references` (#2904).
    + Improved documentation of `--columns` option.

  * Added appveyor setup, with artefacts (Jan Schulz).

  * stack.yaml versions: Use proper flags used for texmath, pandoc-citeproc.

  * LaTeX template: support for custom font families (vladipus).
    Needed for correct polyglossia operation with Cyrillic fonts and perhaps
    can find some other usages.  Example usage in YAML metadata:

          fontfamilies:
          - name: \cyrillicfont
            font: Liberation Serif
          - name: \cyrillicfonttt
            options: Scale=MatchLowercase
            font: Liberation

  * Create unsigned msi as build artifact in appveyor build.

  * On travis, test with ghc 8.0.1; drop testing for ghc 7.4.1.



## pandoc 1.17.0.3 (2016-03-24)

  * LaTeX writer: Fixed position of label in figures (#2813).
    Previously the label wasn't in the right place, and `\ref`
    wouldn't work properly.
  * Added .tei test files to pandoc.cabal so they'll be included
    in tarball (#2811).
  * Updated copyright dates.

## pandoc 1.17.0.2 (2016-03-23)

  * Fixed serious regression in `htmlInBalanced`, which caused
    newlines to be omitted in some raw HTML blocks in Markdown
    (#2804).

## pandoc 1.17.0.1 (2016-03-21)

  * File scope is no longer used when there are no input files (i.e.,
    when input comes from stdin).  Previously file scope was triggered
    when the `json` reader was specified and input came from `stdin`,
    and this caused no output to be produced.  (Fix due to Jesse Rosenthal;
    thanks to Fedor Sheremetyev for calling the bug to our attention.)
  * Improved documentation of templates (#2797).

## pandoc 1.17 (2016-03-20)

  * Added `--file-scope` option (Jesse Rosenthal).
    By default pandoc operates on multiple files by first concatenating
    them (around extra line breaks) and then processing the joined file. So
    it only parses a multi-file document at the document scope. This has the
    benefit that footnotes and links can be in different files, but for
    some purposes it is useful to parse the individual files first
    and then combine their outputs (e.g. when the files use footnotes
    or links with the same labels).  The `--file-scope` option causes
    pandoc to parse the files first, and then combine the parsed output,
    instead of combining before parsing. `--file-scope` is selected
    automatically for binary input files (which cannot be concatenated)
    and for pandoc json.

  * Add TEI Writer (Chris Forster) and `tei` output format.

  * Added a general `ByteStringReader` with warnings, used by the docx
    reader (API change, Jesse Rosenthal).

  * Add `readDocxWithWarnings` (API change, Jesse Rosenthal).

  * Changed type of `Shared.uniqueIdent` argument from
    `[String]` to `Set String`.  This avoids performance problems in documents
    with many identically named headers (API change, #2671).

  * Removed `tex_math_single_backslash` from `markdown_github` options
    (#2707).

  * Make language extensions as well as full language names
    trigger syntax highlighting.  For example, `py` will now work as
    well as `python` (jgm/highlighting-kate#83).

  * Added `institute` variable to latex, beamer templates (Fraser
    Tweedale, Josef Svenningsson).

  * Docx reader (Jesse Rosenthal):

    + Handle alternate content.  Some word functions (especially graphics)
      give various choices for content so there can be backwards compatibility.
    + Don't turn numbered headers into lists.
    + Docx Reader: Add state to the parser, for warnings
    + Update feature checklist in source code.
    + Get rid of `Modifiable` typeclass.
    + Add tests for adjacent hyperlinks.
    + Add a "Link" modifier to `Reducible`. We want to make sure that
      links have their spaces removed, and are appropriately smushed
      together (#2689).

  * HTML reader:

    + Fixed behavior of base tag (#2777).
      If the base path does not end with slash, the last component
      will be replaced.  E.g. base = `http://example.com/foo`
      combines with `bar.html` to give `http://example.com/bar.html`.
      If the href begins with a slash, the whole path of the base
      is replaced.  E.g. base = `http://example.com/foo/` combines
      with `/bar.html` to give `http://example.com/bar.html`.
    + Rewrote `htmlInBalanced`.  This version avoids an exponential
      performance problem with `<script>` tags, and it should be faster
      in general (#2730).
    + Properly handle an empty cell in a simple table (#2718).
    + Handle multiple `<meta>` tags with same name.  Put them in a list
      in the metadata so they are all preserved, rather than (as before)
      throwing out all but one..

  * Markdown reader:

    + Improved pipe table parsing (#2765).
    + Allow `+` separators in pipe table cells.  We already allowed
      them in the header, but not in the body rows, for some reason.
      This gives compatibility with org-mode tables.
    + Don't cross line boundary parsing pipe table row.
      Previously an Emph element could be parsed across the newline
      at the end of the pipe table row.
    + Use `htmlInBalanced` for `rawVerbatimBlock`, for better
      performance (#2730).
    + Fixed bug with smart quotes around tex math.

  * LaTeX reader:

    + Handle interior `$` characters in math (#2743).  For example,
      `$$\hbox{$i$}$$`.
    + `inlineCommand` now gobbles an empty `{}` after any command (#2687).
      This gives better results when people write e.g. `\TeX{}` in Markdown.
    + Properly handle LaTeX "math" environment as inline math (#2171).

  * Textile reader: Support `>`, `<`, `=`, `<>` text alignment attributes.
    Closes #2674.

  * Org reader (Albert Krewinkel):

    + Prefix even empty figure names with "fig:" (#2643).  The
      convention used by pandoc for figures is to mark them by prefixing
      the name with `fig:`.  The org reader failed to do this if a figure
      had no name.
    + Refactor link-target processing (#2684).

  * ConTeXt writer: Fix whitespace at line beginning in line blocks (#2744).
    Thanks to @c-foster.

  * HTML writer: Don't include alignment attribute for default table columns.
    Previously these were given "left" alignment.  Better to leave off
    alignment attributes altogether (#2694).

  * Markdown writer: Use hyphens for YAML metadata block bottom line, for
    better compatibility with other Markdown flavors (Henrik Tramberend).

  * LaTeX writer:

    + Use image identifier to create a label and hypertarget for
      figures (Mauro Bieg).
    + Avoid double toprule in headerless table with caption (#2742).
    + Clean up options parser (Jesse Rosenthal).
    + Treat `memoir` template with `article` option as article, instead
      of treating all `memoir` templates as books.
    + Allow more flexible table alignment (Henrik Tramberend, #2665).
      New default is not to include `[c]` option (which is the default
      anyway if no positioning is specified).  Now LaTeX emplates can
      control the overall table alignment in a document by setting the
      longtable length variables `LTleft` and `LTright`.  For example,
      `\setlength\LTleft\parindent\setlength\LTright\fill`
      will create left-aligned tables that respect paragraph indentation.

  * Docx writer: Handle image alt text (#2754, Mauro Bieg).

  * Org writer - pass through RawInline with format "org".

  * DokuWiki writer: use `$$` for display math.

  * Custom writer: Pass attributes parameter to CaptionedImage (#2697).

  * Make protocol-relative URIs work again (#2737).

  * make_osx_package.sh:  Use env variable for developer id certs.

  * Raise `tagsoup` lower bound to 0.13.7 to fix entity-related
    problems (#2734).

  * Allow `zip-archive` 0.3.

  * Allow `aeson` 0.11.

## pandoc 1.16.0.2 (2016-01-12)

  * Depend on deepseq rather than deepseq-generics (fpco/stackage#1096).

  * Fixed regression in latex smart quote parsing (#2645).
    In cases where a match was not found for a quote, everything
    from the open quote to the end of the paragraph was being dropped.

## pandoc 1.16.0.1 (2016-01-10)

  * Fixed regression with `--latex-engine` (#2618). In 1.16
    `--latex-engine` raises an error if a full path is given.

  * Org reader: Fix function dropping subtrees tagged `:noexport`
    (Albert Krewinkel, #2628):

  * Markdown reader: renormalize table column widths if they exceed 100%
    (#2626).

  * Textile reader:  don't allow block HTML tags in inline contexts.
    The reader previously did allow this, following redcloth,
    which happily parses

        Html blocks can be <div>inlined</div> as well.

    as

        <p>Html blocks can be <div>inlined</div> as well.</p>

    This is invalid HTML.  The above sample now produces;

        <p>Html blocks can be</p>
        <div>
        <p>inlined</p>
        </div>
        <p>as well.</p>

  * Improved default template lookup for custom lua scripts (#2625).
    Previously, if you tried to do `pandoc -s -t /path/to/lua/script.lua`,
    pandoc would look for the template in
    `~/.pandoc/templates/default./path/to/lua/script.lua`.
    With this change it will look in the more reasonable
    `~/.pandoc/templates/default.script.lua`.  This makes it possible to
    store default templates for custom writers.

  * RST, Markdown writers: Fixed rendering of grid tables with blank rows
    (#2615).

  * LaTeX writer: restore old treatment of Span (#2624).  A Span is
    now rendered with surrounding `{}`, as it was before 1.16.

  * Entity handling fixes: improved handling of entities like
    `&lang;` that require a trailing semicolon.  Allow uppercase
    `x` in numerical hexadecimal character references, working
    around a tagsoup bug.

  * `stack.yaml` - use lts-4.0, but with older aeson to avoid excessive
    memory use on compile.  With aeson 0.10 we were getting an out of
    memory error on a 2GB Ubuntu 64-bit VM.

  * Improved deb package creation script. Made `DPKGVER` work.
    Renamed `COMMIT` to `TREE`.  You should now be able to do
    `TREE=1.16.0.1 DPKGVER=2 make deb`.


## pandoc 1.16 (2016-01-02)

  * Added `Attr` field to `Link` and `Image` (Mauro Bieg, #261, API change).

    + Added syntax for link and image attributes to pandoc's Markdown.
    + Updated readers and writers to use link and image attributes
      when appropriate.
    + Support image attributes in Docx, Textile, RST readers.

  * Renamed link attribute extensions.  The old `link_attributes` is
    now `mmd_link_attributes`, and `link_attributes` now enables the
    new pandoc-style link and image attributes (API change).
    Note: this change could break some existing workflows.

  * Implemented `SoftBreak` and new `--wrap` option (#1701, API change).
    Added threefold wrapping option.

    + Command line option: deprecated `--no-wrap`, added
      `--wrap=[auto|none|preserve]`
    + Added `WrapOption`, exported from `Text.Pandoc.Options`
    + Changed type of `writerWrapText` in `WriterOptions` from
      `Bool` to `WrapOption`.
    + Modified `Text.Pandoc.Shared` functions to allow `SoftBreak`.
    + Supported `SoftBreak` in readers and writers.

  * Text.Pandoc.Options:  Added `writerDpi` to `WriterOptions` (API
    change, Mauro Bieg).

  * Added `--dpi` command-line option (Mauro Bieg).

  * Rationalized behavior of `--no-tex-ligatures` and `--smart` (#2541).
    This change makes `--no-tex-ligatures` affect the LaTeX reader
    as well as the LaTeX and ConTeXt writers.  If it is used,
    the LaTeX reader will parse characters `` ` ``, `'`, and `-`
    literally, rather than parsing ligatures for quotation marks
    and dashes.  And the LaTeX writer will print unicode quotation
    mark and dash characters literally, rather than converting
    them to the standard ASCII ligatures.  Note that `--smart` has
    no effect on the LaTeX reader.  `--smart` is still the default
    for all input formats when LaTeX or ConTeXt is the output format,
    *unless* `--no-tex-ligatures` is used.

    Some examples to illustrate the logic:

    ```
    % echo "'hi'" | pandoc -t latex
    `hi'
    % echo "'hi'" | pandoc -t latex --no-tex-ligatures
    'hi'
    % echo "'hi'" | pandoc -t latex --no-tex-ligatures --smart
    ‘hi’
    % echo "'hi'" | pandoc -f latex --no-tex-ligatures
    <p>'hi'</p>
    % echo "'hi'" | pandoc -f latex
    <p>’hi’</p>
    ```

  * Removed deprecated options `--offline` and `--html5`.

  * Fixed language code for Czech (`cs` not `cz`) (#2597).

  * Implemented `east_asian_line_breaks` extension (#2586).
    In `Text.Pandoc.Options`, added `Ext_east_asian_line_breaks` constructor
    to `Extension` (API change).  This extension is like
    `ignore_line_breaks`, but smarter -- it only ignores line breaks
    between two East Asian wide characters.  This makes it better suited
    for writing with a mix of East Asian and non-East Asian scripts.

  * Added support for PDF creation via `wkhtmltopdf`.
    To use this: `pandoc -t html5 -o result.pdf` (and add `--mathjax`
    if you have math.) Margins can be set using the variables
    `margin-top`, `margin-bottom`, `margin-left`, `margin-right`.
    Other styling can be done through CSS.

  * Fixed cite key parsing regression (jgm/pandoc-citeproc#201).
    We were capturing final colons as in `[@foo: bar]`; the citation id
    was being parsed as `@foo:`.

  * ICML writer:

    + Fixed image syntax for local files (#2589).
    + Changed type of `writeICML` (Mauro Bieg).
      API change:  It is now `WriterOptions -> Pandoc -> IO String`.
      Also handle new image attributes.
    + Intersperse line breaks instead of appending them to
      every `ParagraphStyleRange` (Mauro Bieg, #2501).
    + Add `Cite` style to citations (Mauro Bieg).
    + Added figure handling (#2590, Mauro Bieg).
    + Better handling of math.  Instead of just printing the raw tex,
      we now try to fake it with unicode characters.

  * HTML writer: Include `example` class for example lists (#2524).

  * ODT/OpenDocument writer: improved image attributes (Mauro Bieg).

    + Support for percentage widths/heights
    + Use `Attr` instead of title to get dimensions from ODT walker
      to `writeOpenDocument`.

  * AsciiDoc writer:

    + Support anchors in spans and divs with id elements
      (jgm/pandoc-citeproc#143).
    + Fixed code blocks (#1861).

  * Haddock writer:  omit formatting inside links, which isn't supported
    by Haddock (#2515).

  * MediaWiki writer:  Fixed spacing issues in table cells.

    + Start cell on new line unless it's a single Para or Plain
      (#2606).
    + For single Para or Plain, insert a space after the `|` to
      avoid problems when the text begins with a character like
      `-` (#2604).

  * Beamer writer: mark frame as fragile when it contains verbatim (#1613).

  * LaTeX writer:

    + Add support for GAP highlighting using listings (Raniere Silva).
    + Consider `header-includes` content as well as templates
      when determining whether to use csquotes (Andreas Lööw).
    + Create defaults for geometry using `margin-left` etc.
      If `geometry` has no value, but `margin-left`, `margin-right`,
      `margin-top`, and/or `-margin-bottom` are given, a default value
      for `geometry` is created from these.  Note that these variables
      already affect PDF production via HTML5 with `wkhtmltopdf`.

  * ConTeXt writer: set default layout based on `margin-left`, etc.
    This sets up `\setuplayout` based on the variables `margin-left`,
    `margin-right`, `margin-bottom`, and `margin-top`, if no layout
    is given.

  * Docx writer:  better handling of PDF images.  Previously we tried
    to get the image size from the image even if an explicit size was
    specified.  Since we still can't get image size for PDFs, this made
    it impossible to use PDF images in docx.  Now we don't try to get
    the image size when a size is already explicitly specified.

  * Markdown writer:  use raw HTML for link/image attributes when
    the `link_attributes` extension is unset and `raw_html` is set (#2554).

  * MediaWiki reader: interpret markup inside `<tt>`, `<code>` (#2607).

  * LaTeX reader:

    + Improved smart quote parsing (#2555). This fixes redering of
      unmatched quotes.
    + Use curly quotes for unmatched ` (#2555).
    + Allow blank space between braced arguments of commands (#2592).

  * Markdown reader:

    + Improved pipe table relative widths.  Previously pipe table
      columns got relative widths (based on the header underscore lines)
      when the source of one of the rows was greater in width than the
      column width.  This gave bad results in some cases where much of
      the width of the row was due to nonprinting material (e.g. link
      URLs).  Now pandoc only looks at printable width (the width of a
      plain string version of the source), which should give better results.
      Thanks to John Muccigrosso for bringing up the issue.
    + Fixed parsing bug with macros.  Previously macro definitions in
      indented code blocks were being parsed as macro definitions, not code.

  * Textile reader:  skip over attribute in image source (#2515).
    We don't have a place yet for styles or sizes on images, but
    we can skip the attributes rather than incorrectly taking them
    to be part of the filename.

  * Docx reader: Handle dummy list items (Jesse Rosenthal).
    These come up when people create a list item and then delete the
    bullet. It doesn't refer to any real list item, and we used to ignore
    it.

  * CommonMark reader/writer rewritten to use latest `cmark`.

  * Fixed Emoji character definitions (#2523).  There were many bugs in the
    definitions.

  * `Text.Pandoc.CSS`:

    + Added `pickStylesToKVs` function to extract multiple properties at
      once (API change, Mauro Bieg).
    + Parse CSS that doesn't contain the optional semicolon (Mauro Bieg).

  * `trypandoc`: sort drop-down lists.

  * Beamer template:

    + Made `\euro` conditional on presence of character.
      for xelatex and lualatex, as it is for pdflatex (Andrew Dunning).
    + Moved `header-includes` before setting of title (Thomas Hodgson),
      to match the LaTeX template (jgm/pandoc-templates#168).
    + Added `section-titles` variable (defaults to true)
      to enable/suppress section title pages in beamer
      slide shows (Thomas Hodgson).
    + Moved beamer themes after fonts, so that themes can
      change fonts.  (Previously the fonts set were being
      clobbered by lmodern.sty.) (Thomas Hodgson).

  * Beamer/LaTeX template changes (Thomas Hodgson):
    + Added `thanks` variable
    + Use `parskip.sty` when `indent` isn't set (fall back to using
      `setlength` as before if `parskip.sty` isn't available).
    + Use `biblio-style` with biblatex.
    + Added `biblatexoptions` variable.

  * LaTeX template changes:

    + Added `paper` after `$papersize$` variable in latex template.
      Thus you can say `papersize: a4` and the latex will contain
      `a4paper`.  This change may break some existing workflows; if
      you currently specify `a4paper`, you'll get `a4paperpaper` which
      is meaningless.  However, the change seems worth it, as it will
      make the `papersize` variable work uniformly across ConTeXt, LaTeX,
      and html->pdf via wkhtmltopdf.
    + Only pass options to color package if `colorlinks` is set
      (Andrew Dunning).
    + Make definition of `\euro` conditional in xelatex/lualatex,
      as it is already for pdflatex (Andrew Dunning).
    + Removed setting of `subject` in PDF metadata.
      This used to be set to the subtitle, but really the subtitle
      need not give the subject.  Also, `subtitle` can contain formatting,
      so we'd need, at least, a plain text version for this.
    + Moved `header-includes` before setting of `\title`, `\author`,
      etc.  This allows these macros to be redefined.
    + Use `\subtitle` command for `subtitle`, instead of tacking it
      on to the title as before.  We give a no-op fallback definition if it
      is not defined.  This change should produce much better results
      in classes that support `\subtitle`.  With the default article
      class, which does not define `\subtitle`, subtitles will no
      longer be printed unless the user defines `\subtitle` and
      redefines `\maketitle`.
    + Moved redefinitions of `\paragraph` and `\subparagraph` to
      before header-includes.

  * Context template:

    + Use `simplefonts` for font loading (Paolo Rodríguez).  This is
      needed for things to work on ConTeXt stable from TeXLive 2015.
    + Revert use of `\setuphead` in title block (Andrew Dunning,
      Rik Kabel).

  * Update LaTeX/ConTeXt link colour usage (Andrew Dunning).

  * Fixed man template so disabling hyphenation actually works.
    The command needs to come after .TH.

  * Added 'navigation' variable to beamer template (#2543).
    Valid values are `empty` (the default), `horizontal`, `vertical`,
    and `frame`.  Note that this changes the default behavior from
    `horizontal` to `empty`.  Closes #2543.

  * Added `toc` to HTML slide format templates (Andrew Dunning),
    so that `--toc` creates a contents slide.

  * Added `stack.full.yaml` to build `pandoc-citeproc` as well.

  * Allow pipe tables with no body rows (#2556).
    Previously this raised a runtime error.

  * Shared: Improved `fetchItem` so that `C:/Blah/Blah.jpg` isn't treated
    as URL.  The Haskell URI parsing routines will accept "C:" as a
    scheme, so we rule that out manually.  This helps with
    `--self-contained` and absolute Windows paths.

  * Define a `meta-json` variable for all writers (#2019).  This contains
    a JSON version of all the metadata, in the format selected for the
    writer.  So, for example, to get just the YAML metadata, you can run
    pandoc with the following custom template: `$meta-json$`.  The intent
    is to make it easier for static site generators and other tools to get
    at the metadata.

  * Document limitations of --self-contained (#2553).

  * Improved Citations section of README (#2551).  Added information
    about `link-citations` and a link to the pandoc-citeproc man page.

  * `ImageSize`: use `safeRead` instead of `readMaybe`, which isn't
    in base < 4.6.

  * Allow .adoc file extension for AsciiDoc (Andrew Dunning).

  * Improved implicit pandoc-citeproc inclusion.
    The filter pandoc-citeproc is automatically used when
    `--bibliography` is specified on the command line, unless
    `--natbib` or `--biblatex` is used.  However, previously this
    only worked if `--bibliography` was spelled out in full, and not
    if `--biblio` was used.

  * reveal.js: Interpret pauses correctly for all headers (#2530).
    Previously, when using headers below the slide level, pauses are left
    uninterpreted into pauses. In my opinion, unexpected behavior but
    intentional looking at the code.

  * Remove redundant `center` variable for reveal.js (Andrew Dunning).

  * Parsing: Add `extractIdClass`, modified type of `KeyTable` (Mauro
    Bieg, API change).

  * ImageSize:  Added functions for converting between image dimensions
    (Mauro Bieg).

  * Use lts-3.18 in stack.yaml.  This avoids Windows build
    issues with the HTTP library.

  * Bump version bounds for dependencies.

## pandoc 1.15.2.1 (2015-11-16)

  * Added two missing test files, and `stack.yaml`, to
    `extra-source-files` so they're included in the source tarball.

  * reveal.js template: Fixed parallaxBackground options.
    `parallaxBackgroundHorizontal` and `parallaxBackgroundVertical`
    need integer values, not strings.  (Vaughn Iverson)

## pandoc 1.15.2 (2015-11-15)

  * `pandoc my.md -t context -o my.pdf` will now create a PDF using
    ConTeXt rather than LaTeX (#2463).

  * Fixed omitted `url(...)` in CSS data-uri with `--self-contained` (#2489).

  * Added `emoji` Markdown extension, enabled by default in `markdown_github`
    (#2523).  Added `Ext_emoji` to `Extension` in `Text.Pandoc.Options`
    (API change).

  * `Text.Pandoc.Readers.HTML.parseTags`: Fixed over-eager raw HTML inline
    parsing (#2469).  Tightened up the inline HTML parser so it disallows
    TagWarnings.

  * Derive `Generic` instances for the types in `Text.Pandoc.Options`.

  * Org reader:

    + Fix paragraph/list interaction (Albert Krewinkel, #2464).
      Paragraphs can be followed by lists, even if there is no blank line
      between the two blocks.  However, this should only be true if the
      paragraph is not within a list, were the preceding block should be
      parsed as a plain instead of paragraph (to allow for compact lists).
      Thanks to @rgaiacs for bringing this up.
    + Allow toggling header args (Albert Krewinkel, #2269).
      Org-mode allows to skip the argument of a code block header argument if
      it's toggling a value.  Argument-less headers are now recognized,
      avoiding weird parsing errors.
    + Fix markup parsing in headers (Albert Krewinkel, #2504).
      Markup as the very first item in a header wasn't recognized.  This was
      caused by an incorrect parser state: positions at which inline markup
      can start need to be marked explicitly by changing the parser state.
      This wasn't done for headers.  The proper function to update the state
      is now called at the beginning of the header parser, fixing this issue.
    + Fix emphasis rules for smart parsing (Albert Krewinkel, #2513).
      Smart quotes, ellipses, and dashes should behave like normal quotes,
      single dashes, and dots with respect to text markup parsing.
    + Require whitespace around definition list markers (#2518).
      This rule was not checked before, resulting in bugs with footnotes
      and some link types.

  * Markdown reader:

    + Pipe tables with long lines now get relative cell widths (#2471).
      If a pipe table contains a line longer than the column width (as set by
      `--columns` or 80 by default), relative widths are computed based on the
      widths of the separator lines relative to the column width.  This should
      solve persistent problems with long pipe tables in LaTeX/PDF output, and
      give more flexibility for determining relative column widths in other
      formats, too.  For narrower pipe tables, column widths of 0 are used,
      telling pandoc not to specify widths explicitly in output formats that
      permit this.
    + Improved parser for `mmd_title_block`.  We now allow blank metadata
      fields.  These were explicitly disallowed before.
    + Citation keys can now contain `://`, so URLs and DOIs can be used
      as citation keys (jgm/pandoc-citeproc#166).

  * Beamer template:  fix incompatibility of section slides with natbib.
    Natbib (and presumably biblatex) bibliography commands create
    their own section.  Since these are in frame environments,
    we have an incompatibility with the `\AtBeginSection` macro
    which creates a special frame when a new section occurs.
    (We can't have a frame inside another frame.) This change disables
    `\AtBeginSection` inside bibliography slides.  Thinks to Yihui Xie for
    bringing the problem to my attention.  This supersedes #145.  See
    discussion there.

  * Textile reader:  don't do smart punctuation unless explicitly asked
    (#2480).  Note that although smart punctuation is part of the textile
    spec, it's not always wanted when converting from textile
    to, say, Markdown.  So it seems better to make this an option.

  * LaTeX reader: Handle `comment` environment (Arata Mizuki).
    The `comment` environment is handled in a similar way to the
    `verbatim` environment, except that its content is discarded.

  * Docx reader:  Follow relationships correctly in foot/endnotes (#2258,
    Jesse Rosenthal).  This fixes a problem with links in notes.

  * LaTeX and ConTeXt writers: support `lang` attribute on divs and spans
    (mb21).  For LaTeX, also collect `lang` and `dir` attributes on spans and
    divs to set the `lang`, `otherlangs` and `dir` variables if they aren’t set
    already.  See #895.

  * LaTeX writer:

    + Use proper command for `\textarabic` (mb21).
    + Added `de-CH-1901`, fixed `el-polyton` in `toPloyglossia` (Nick Bart).
    + Use `\hypertarget` and `\hyperlink` for links.  This works correctly
      to link to Div or Span elements.  We now don't bother defining `\label`
      for Div or Span elements.  Closes jgm/pandoc-citeproc#174.
    + Avoid footnotes in list of figures (#1506).
    + Properly handle footnotes in captions (#1506).
    + Add `\protect` to `\hyperlink` (#2490).  Thanks to Hadrien Mary.
    + Set `colorlinks` if `linkcolor`, `urlcolor`, `citecolor`, or
      `toccolor` is set (#2508).

  * Textile writer: support start number in ordered lists (#2465).

  * OpenDocument writer:  Allow customization of opendocument
    automatic styles.  Automatic styles can now be inserted in the
    template, which now provides the enclosing `<office:automatic-styles>`
    tags (#2520).

  * Docx writer:  insert space between footnote reference and note (#2527).
    This matches Word's default behavior.

  * EPUB writer:  don't download linked media when `data-external` attribute
    set (#2473).  By default pandoc downloads all linked media and includes it
    in the EPUB container.  This can be disabled by setting `data-external` on
    the tags linking to media that should not be downloaded.  Example:

        <audio controls="1">
         <source src="http://www.sixbarsjail.it/tmp/bach_toccata.mp3"
         type="audio/mpeg"></source>
        </audio>

  * HTML writer:  use width on whole table if col widths sum to < 100%.
    Otherwise some browsers display the table with the columns
    separated far apart.

  * AsciiDoc template:  Fix `author` and `date`; add `keywords`,
    `abstract` (Andrew Dunning).

  * HTML-based templates (Andrew Dunning):

    + Use en dash instead of hyphen between title prefix and title.
    + Add `keywords` to metadata.
    + Add `lang`, `dir`, `quotes` where missing.
    + Always make author and date display conditional.
    + Updated dzslides template from source.

  * Man template: make "generated by" comment conditional.

  * LaTeX, Beamer templates:

    + Add `babel-otherlangs` for language divs/spans; `babel-newcommands`,
      filled by commands that make babel understand the polyglossia-style
      language directives (mb21, #137).
    + Improved formatting of conditionals; `$for$` is always provided to allow
      multiple options (Andrew Dunning, #141).
    + Use `Ligatures=TeX` rather than `Mapping=tex-text` with `fontspec`
      to improve support for LuaTeX (Andrew Dunning, #135).
    + Revise `hyperref` usage (Andrew Dunning, #139, #141):
        - use same options for all LaTeX engines;
        - add `subtitle` and `keywords` to PDF metadata;
        - do not override `hyperref` link coloring without user input, effectively making
          the `hidelinks` option the default (removed as a separate variable);
        - link colors can be enabled (using a slightly darker version of the old
          defaults) using a new `colorlinks` variable, automatically used by
          the LaTeX writer when custom colors are specified;
        - `pdfborder={0 0 0}` is automatically set by `hyperref` with
          `colorlinks`, and is only applied if `colorlinks` is disabled.

  * ConTeXt template (Andrew Dunning):

    + New variables for controlling styles: `linkstyle`, `linkcolor`,
      `linkcontrastcolor`, `layout`, `pagenumbering`, `whitespace`, `indenting`,
      `interlinespace`, `headertext`, `footertext`, `mainfont`, `sansfont`,
      `monofont`, `mathfont`, `fontsize`.
    + Default template no longer supports MkII.
    + Improve writing of title block (suppressing numbering of first page).
    + Add `title` `subtitle`, `author`, `date`, `keywords` to PDF metadata.
    + Support `subtitle`, `abstract`.
    + Support list of figures (`lof`), list of tables (`lot`).
    + Disable link styling by default.
    + Define styles for all section types.
    + Enable microtype.
    + Improved formatting of conditionals.

  * Beamer template:  added code to prevent slide breaks inside paragraphs
    (#2422, thanks to Nick Bart).  This will matter, in practice, only when
    `allowframebreaks` is used.  It is especially helpful for bibliography
    slides.

  * OpenDocument template:  Add `<office:automatic-styles>` tag around
    automatic styles.  The writer now longer provides this (see #2520).

  * Restored Text.Pandoc.Compat.Monoid.

  * Do not export (<>) from custom Prelude.  The Prelude now matches
    base 4.8 Prelude's API.

  * Don't use custom prelude with ghc 7.10.  Use the custom prelude
    only for earlier versions.  This change makes `stack ghci` and
    `cabal repl` work (#2503), at least with ghc 7.10.

  * Changed § to % in operators from Odt.Arrows.Utils (#2457).
    This prevents problems building haddocks with "C" locale.

  * Change default for old-locale flag to False.

  * Use stack in deb, osx, and Windows package generators.

  * Added Vagrantfile for building deb in vm.
    This should help in automating binary package creation.  'make package'
    will make the package.  'make package COMMIT=blah' will make the package
    from commit blah.

  * README:

    + Consistent capitalization for pandoc and Markdown.
    + Fixed `auto_identifiers` examples (Benoit Schweblin).
    + Improved documentation of template variables (Andrew Dunning).

## pandoc 1.15.1 (2015-10-15)

  * `pandocVersion` is now defined in `Text.Pandoc.Shared`
    and reexported from `Text.Pandoc` (Alex Vong).  This allows
    writers to access it.  (Alex Vong) (API change)

  * For `markdown_mmd`, add: `implicit_figures`, `superscripts`,
    `subscripts` (#2401).

  * Added `odt` as input format (MarLinn).  Added new module
    `Text.Pandoc.Reader.ODT` (API change). Fully implemented features:
    Paragraphs, Headers, Basic styling, Unordered lists, Ordered lists,
    External Links, Internal Links, Footnotes, Endnotes, Blockquotes.
    Partly implemented features: Citations, Tables.

  * Markdown Reader:

    + Add basic tests for each header style (Ophir Lifshitz).
    + Add implicit header ref tests for headers with spaces (Ophir Lifshitz).
    + Skip spaces in headers (Ophir Lifshitz).
    + Handle 'id' and 'class' in parsing key/value attributes (#2396).
      `# Header {id="myid" class="foo bar"}`
      is now equivalent to `# Header {#myid .foo .bar}`.
    + Use '=' instead of '#' for atx-style headers in markdown+lhs.
      (Kristof Bastiaensen)
    + Pipe tables: allow indented columns.  Previously the left-hand column
      could not start with 4 or more spaces indent.  This was inconvenient
      for right-aligned left columns.  Note that the first (header column)
      must still have 3 or fewer spaces indentation, or the table will be
      treated as an indented code  block.
    + Fix regression:  allow HTML comments containing `--`.
      Technically this isn't allowed in an HTML comment, but
      we've always allowed it, and so do most other implementations.
      It is handy if e.g. you want to put command line arguments
      in HTML comments.

  * LaTeX reader:

    + Don't eat excess whitespace after macros with only optional
      arguments (#2446).
    + Support longtable (#2411).
    + Implement `\Cite` (#2335).
    + Support abstract environment.  The abstract populates an
      `abstract` metadata field.
    + Properly handle booktabs lines.  Lines aren't part of the
      pandoc table model, so we just ignore them (#2307).

  * HTML reader:

    + Handle type attribute on ol, e.g. `<ol type="i">` (#2313).
    + Updated for new automatic header attributes.
    + Add auto identifiers if not present on headers.  This makes
      TOC linking work properly.
    + Detect `font-variant` with `pickStyleAttrProps` (Ophir Lifshitz).
    + Test `<ol>` type, class, and inline list-style(-type) CSS
      (Ophir Lifshitz).
    + Better handling of "section" elements (#2438).  Previously
      `<section>` tags were just parsed as raw HTML blocks.  With
      this change, section elements are parsed as Div elements with
      the class "section".

  * MediaWiki reader:  handle unquoted table attributes (#2355).

  * DocBook reader:

    + Added proper support for DocBook `xref` elements (Frerich Raabe).
      Added `dbContent` field to reader state, so we can lookup
      cross refs.
    + Handle `informalexample` (#2319).

  * Docx Reader:

    + Create special punctuation test (Ophir Lifshitz).
    + Parse soft, no-break hyphen elements (Ophir Lifshitz).
    + Updated headers test (Ophir Lifshitz). Replaced `styles.xml`
      in `headers.docx` with pandoc's current `styles.xml`, which
      contains styles for Heading 1 through 6. Added Heading 4
      through 7 to the test document. Note that Heading 7 is not
      parsed as a Heading because there is no Heading 7 style.

  * RST reader:  better handling of indirect roles.
    Previously the parser failed on this kind of case

        .. role:: indirect(code)

        .. role:: py(indirect)
           :language: python

        :py:`hi`

    Now it correctly recognizes `:py:` as a code role.

  * Org reader:

    + Add auto identifiers if not present on headers
      (#2354, Juliusz Gonera).
    + Allow verse blocks to contain empty lines (#2402,
      Albert Krewinkel).

  * EPUB reader:  stop mangling external URLs (#2284).

  * RST writer:

    + Don't insert `\ ` when complex expression in matched pairs.
      E.g. `` [:sup:`3`] `` is okay; you don't need `` [:sup:`3`\ ] ``.
    + Ensure that `\ ` is inserted when needed before Cite and Span
      elements that begin with a "complex" element (jgm/pandoc-citeproc#157).
    + Normalize headers only in "standalone" mode (#2394).

  * Haddock writer: escape `*` and `^` (G. Bataille).

  * Markdown writer:

    + In TOC, add links to headers (#829).
    + Use unicode super/subscripts for digits in plain output
      (when the `superscripts` and `subscripts` extensions are
      not enabled).

  * Docx writer:

    + Moved invalid character stripping to `formattedString`.
      This avoids an inefficient generic traversal (#2356).
    + Use user data directory for `reference.docx` archive.
      This allows the test suite to work without installing pandoc first.
      It also brings the docx writer in line with the odt writer.
    + Tests:  docx writer tests now use `../data` for data directory.
      This allows tests to be run without installing first.
    + Tests: Use real jpg (not empty) for docx tests to avoid warning.

  * LaTeX writer:

    + Fixed detection of 'chapters' from template.
      If a documentclass isn't specified in metadata, but the
      template has a hardwired bookish documentclass, act as if
      `--chapters` was used.  This was the default in earlier
      versions, but it has been broken for a little while.
    + Correctly recognize book documentclass in metadata (#2395).
    + Set language-related variables automatically, depending
      on the value of the `lang` field, which is now always
      assumed to be in BCP47 format (mb21, #1614, #2437).
    + Add `\protect` to `\hyperdef` in inline context.  This way we
      don't get an error when this is used as a moveable argument (#2136).
    + Support all frame attributes in Beamer.
    + Percent-encode more special characters in URLs (#1640, #2377).
      The special characters are '<','>','|','"','{','}','[',']','^', '`'.

  * HTML writer:

    + Update KaTeX JS and CSS versions (Emily Eisenberg).
    + For dzslides, add `role="note"` for speaker notes (#1693).
    + Percent-encode more special characters in URLs (#1640, #2377).
      The special characters are '<','>','|','"','{','}','[',']','^', '`'.
    + Render Div with class `section` as `<section>` in HTML5.

  * EPUB writer:

    + In TOC, replace literal `<br/>` with space (#2105).
    + With `--webtex`, include image file rather than `data:` URI (#2363).

  * Native writer: format Div properly, with blocks separated.

  * Support bidirectional text output with XeLaTeX, ConTeXt and HTML
    (#2191, mb21).

  * Reference Docx:

    + Add missing Header 6 style (steel blue) (Ophir Lifshitz).
    + Correct `outlineLvl` for Header styles (Ophir Lifshitz).

  * Templates

    + Beamer:  Add `innertheme`, `outertheme` variables
      (Guilhem Bonnefille, #121). Add space after colon in figure caption.
      Integrate recent font and language updates from LaTeX template;
      allow use of `mainfont` variable for changing the slide text
      in XeTeX and LuaTeX (Andrew Dunning, #131).
    + LaTeX:  Add `mainfontoptions`, `sansfontoptions`,
      `monofontoptions`, `mathfontoptions`, `fontfamilyoptions`
      (Andrew Dunning, #122).  Support handling of bidirectional
      text (mb21, #120). Improve reliability of superscripts/subscripts
      under XeTeX and prevent letters and numbers from appearing on a
      different baseline by removing use of the `realscripts` package
      (via `xltxtra`).  To restore use of OpenType characters for these
      features under XeTeX or LuaTeX, add `\usepackage{realscripts}` to
      `header-includes` (Andrew Dunning, #130).  Remove redundant
      reference to `xunicode` (Andrew Dunning, #130).  Add `fontenc`,
      `indent`, `subparagraph` variables (Andrew Dunning).
      Allow use of `hidelinks` variable for `hyperref` package (Hugo Roy,
      #113).  Prevent package clash with `tufte-latex` and other classes that
      include `hyperref` or `color` (Xavier Olive, #115).
    + ConTeXt:  Support handling of bidirectional text (mb21, #120).
    + LaTeX and ConTeXt: Use more specific language variables.
      Instead of directly using `lang`, we now use `babel-lang` and
      `polyglossia-lang` and `context-lang`.  These variables are set by
      the writers to the necessary values, based on the `lang` variable
      (which now always takes a value in BCP47 format). (mb21, #114, #129).
    + HTML:  Support handling of bidirectional text (mb21, #120).
      Move HTML5 shiv after CSS and fix URL (Andrew Dunning).
      Add dir attribute in html5 (Andrew Dunning).
    + reveal.js: Add `controls`, `progress` variables (Grégoire Pineau, #127).
      Add `width`, `height` variables (Anrew Dunning).  Update template
      from 3.1 source (Andrew Dunning).  All configuration options are now
      available as variables, but are only be included if set (reveal.js
      uses defaults otherwise).
    + man: Added comment stating that the page is autogenerated by pandoc,
      giving version.  Added `adjusting` and `hyphenate` variables
      (Alex Vong, #123).

  * epub.css: added selectors for nested emphasis (Pablo Rodriguez).

  * MediaBag:  ensure that `/` is always used as path separator.

  * `sample.lua`: define `CaptionedImage`, add newline at end (#2393).

  * Added `--bash-completion` option.  This generates a bash completion
    script.  To use: `eval "$(pandoc --bash-completion)"`.

  * Text.Pandoc.Error: Define Typeable and Exception instances
    for PandocError (#2386).

  * Text.Pandoc.Parsing: `toKey`: strip off outer brackets.
    This makes keys with extra space at the beginning and end
    work:  e.g.

        [foo]: bar

        [ foo ]

    will now be a link to bar (it wasn't before).

  * Text.Pandoc: disable `auto_identifiers` for epub.
    The epub writer inserts its own auto identifiers;
    this is more complex due to splitting into "chapter" files.

  * Renamed Text.Pandoc.Compat.Locale -> Text.Pandoc.Compat.Time.
    It now reexports Data.Time.

  * Use custom Prelude to avoid compiler warnings.

    + The (non-exported) prelude is in prelude/Prelude.hs.
    + It exports Monoid and Applicative, like base 4.8 prelude,
      but works with older base versions.
    + It exports (<>) for mappend.
    + It hides 'catch' on older base versions.

  * Added a `stack.ymal` and stack install instructions to INSTALL.

  * Clarified what is "out of scope" in README and CONTRIBUTING.md.

  * Added note to CONTRIBUTING.md about ghc versions and travis.

  * Clarify docs on block quotes.  The space after `>` is optional (#2346).

  * Removed obsolete reference to default.csl (#2372).

  * List all styles in manual for `--reference-docx` (Chris Black)

  * Don't capitalize header links in man page.

  * Added section on repl to CONTRIBUTING.md.

  * README:  Added space after backslash in image example (#2329).

  * Document details of citation locator terms (Nick Bart).

  * Fixed some internal links in README (#2309).

  * Improve CSL documentation, variables documentations,
    links, and cross-references in README. (Andrew Dunning)

  * Fix build failure with `--flags=-https` (Sergei Trofimovich).

  * Use `newManager` instead of `withManager` in recent `http-client`.
    This avoids a deprecation warning.

  * Allow building with latest versions of http-types,
    HUnit, criterion, syb, aeson.

  * Updated benchmark program for new criterion API.

  * Setup.hs: rewrite so as not to use process, directory, filepath.
    Using anything outside base is dangerous, since older
    versions of ghc may link against two different versions.

  * Added appveyor (Windows continuous integration) builds.

  * New `.travis.yml`.  Autgenerated using `make_travis_yml.hs`.
    This script has been modified in a few ways, e.g. to add `GHCOPTS`.
    `make .travis.yml` regenerates it based on the tested-with
    field of the cabal file.

## pandoc 1.15.0.6 (2015-07-15)

  * `--self-contained`: Fixed overaggressive CSS minimization
    (#2301, 2286). Previously `--self-contained` wiped out all
    spaces in CSS, including semantically significant spaces.
    This was a regression from 1.14.x.

  * Markdown reader: don't allow bare URI links or autolinks in link
    label (#2300).  Added test cases.

  * `Text.Pandoc.Parsing`, `uri`: Improved bare autolink detection (#2299).
    Previously we disallowed `-` at the end of an autolink,
    and disallowed the combination `=-`.  This commit liberalizes the
    rules for allowing punctuation in a bare URI, and adds test cases.
    One potential drawback is that you can no longer put a bare
    URI in em dashes like this:
    `this uri---http://example.com---is an example.`
    But in this respect we now match github's treatment of bare URIs.

  * HTML writer:  support speaker notes in dzslides.
    With this change `<div class="notes">` and also `<div class="notes"
    role="note">` will be output if `-t dzslides` is used. So we can
    have speaker notes in dzslides too.  Thanks to maybegeek.

  * Updated dzslides template.

  * Improved documentation of options to print system default files (#2298).
    `--print-default-data-file` and `--print-default-template`.

  * DokuWiki writer: use `$..$` for Math instead of `<math>..</math>`
    (Tiziano Müller).  MathJax seems currently to be the only maintained
    math rendering extension for DokuWiki.

  * `Text.Pandoc.Shared`: Changed `hierarchicalize` so it treats references
    div as top-level header (#2294).  This fixes a bug with `--section-divs`,
    where the final references section added by pandoc-citeproc, enclosed in
    its own div, got nested in the div for the section previous to it.

  * Allow vector 0.11.

  * Require cmark > 0.4.

## pandoc 1.15.0.5 (2015-07-10)

  * HTML writer: Fixed email javascript obfuscation with `mailto:`
    URLs (#2280).  This fixes a potential security issue.  Because
    single quotes weren't being escaped in the link portion, a
    specially crafted email address could allow javascript code injection.

  * RST reader:  allow inline formatting in definition list field
    names (Lars-Dominik Braun).

  * PDF:  Make sure `--latex-engine-opt` goes before the filename
    on the command line.  LaTeX needs the argument to come after
    the options (#1779).

  * CommonMark writer: fixed tags used for super/subscript.

  * ConTeXt template:  activate hanging indent for definition lists
    (mb21).

  * Make cabal require `hsb2hs` >= 0.3.1 if `embed_data_files` specified.
    This is done by adding `hookedPrograms` in `Setup.hs`, which allows us
    to include `hsb2hs` in Build-Tools in cabal.

  * Improved Windows installer (thanks to nkalvi).

    + When per-machine installation is chosen, the system path
      is updated instead of the user's.
    + An appropriate default is used for per-machine installation
      directory.
    + Admin privileges are no longer required for a per-user install

  * Travis:  unpack sdist for build to catch packaging bugs.

  * Improved documentation on where user templates go (#2272).

## pandoc 1.15.0.4 (2015-07-03)

  * Added pandoc.1 man page to the repository.  It is no longer
    built as part of the cabal build process. (This proved too
    fragile.)  pandoc.1 can be regenerated (`make man/pandoc.1`)
    when `README` is changed.

  * Copying of the man page now respects `--destdir` (#2262).

  * Improved error messages for filters.  User is now informed if
    the filter requires an interpreter that isn't found in the path,
    or if the filter returns an error status.

## pandoc 1.15.0.3 (2015-07-02)

  * Ensure target directory is created when installing man page.

## pandoc 1.15.0.2 (2015-07-02)

  * Added files needed for building man page to Extra-Source-Files.

## pandoc 1.15.0.1 (2015-07-01)

  * Man page is now built and installed as part of the cabal build
    process. Removed Makefile target for man page.

## pandoc 1.15 (2015-07-01)

  * Man page changes:

    + Removed `--man1`, `--man5` options (breaking change).
    + Removed `Text.Pandoc.ManPages` module (breaking API change).
    + Makefile target for `man/man1/pandoc.1`.  This uses pandoc to
      create the man page from README using a custom template and filters.
    + Added `man/` directory with template and filters needed to build
      man page.
    + We no longer have two man pages: `pandoc.1` and `pandoc_markdown.5`.
      Now there is just pandoc.1, which has all the content from README.
      This change was needed because of the extensive cross-references
      between parts of the README.
    + Removed old `data/pandoc.1.template` and
      `data/pandoc_markdown.5.template`.

  * OpenDocument writer: Do not add a carriage return after a hard
    line break (Michael Chladek).

  * ConTeXt writer:

    + use `\goto` for internal links.
    + Added a `%` at end for `\reference` to avoid spurious space.

  * Ignore sandbox on 'make quick'

## pandoc 1.14.1 (2015-06-30)

  * Added `--man1` and `--man5` options to pandoc, allowing
    pandoc to generate its own man pages. Man pages are no longer
    automatically generated in the build process (the process for
    this was too complex and prone to failure, #2190). The
    `make-pandoc-man-pages` executable has been removed. The
    `man/` directory has been removed, and man page templates
    have been moved to `data/`. NOTE TO PACKAGERS: You will no
    longer find pandoc's man pages in `man/`, but you can
    generate them using `pandoc --man1 > pandoc.1` and `pandoc
    --man5 > pandoc_markdown.5`.

  * Added new unexported module:  `Text.Pandoc.ManPages`.

  * `README` now acts like a data file (even though it isn't in
    `data/`).  So, for example, `pandoc --print-default-data-file README`
    will produce the README.)  This change was required for the `--man1`
    and `--man5` options, since the man pages are produced from the
    README, but it may be useful for other purposes as well.

  * Allow `reference.docx` and `reference.odt` to be used with
    `--print-default-data-file` and to shadow defaults if placed in
    the user data directory.  Note that as of 1.14, we no longer
    include these files as data files; instead, we include their
    components.  This change causes pandoc to behave as if it has
    these data files; they are constructed on demand when needed
    using `getDefaultReferenceDocx` and `getDefaultReferenceODT`.

  * Fixed regression in CSS parsing with `--self-contained` (#2224).
    Pandoc 1.14.0.x used css-text to parse the CSS, but its parser
    silently drops big sections of CSS.  This commit replaces the
    use of css-text with a small but principled CSS preprocessor,
    which removes whitespace and comments and replaces `url()` with
    base 64 data when possible.

  * Use `https://` instead of `//` for MathJax and KaTeX CDN URLs (#1920).
    This will allow math to work when pages are being viewed locally.

  * `Text.Pandoc.Options`:  Export `plainExtensions`.
    These are the extensions used in `plain` output.

  * LaTeX reader: Don't parse `_` and `^` as sub/superscript outside of
    math mode; treat them as regular inline text.  Normally these will
    cause an error in LaTeX, but there are contexts (e.g. `alltt`
    environments) where they are allowed.

  * HTML reader:  allow `<body>` to close `<head>`.

  * DocBook reader: support `mediaobject`s and `figures` (#2184, mb21).

  * RST reader: Fix reference names with special characters
    (Lars-Dominik Braun).

  * Textile writer:  escape `+` and `-` as entities (#2225).

  * DokuWiki writer: Use proper `<code>` tags for code blocks (#2213).

  * Plain writer:  don't use symbols for super/subscript (#2237).
    Simplified code by using `plainExtensions`.

  * InDesign writer: Properly escape URLs containing more than one
    colon character (gohai).

  * Docx writer: Make sure we use dist version of `reference.docx`
    (and not the user's version) for certain settings.  Taking some
    settings values from a user-supplied reference.docx can lead to
    corruption.  This fixes a regression from the last release (#2249).

  * `Text.Pandoc.Shared`: exports `getDefaultReferenceDocx` and
    `getDefaultReferenceODT` (API change).  These functions have been
    removed from the Docx and ODT writers.

  * LaTeX template (Xavier Olive):
    + Added `CJKmainfont` and `CJKoptions` variables.
    + Allow dvipsnames (e.g. `MidnightBlue`) for colors (Xavier Olive).

  * Epub templates:  use `author.role`, not `author.type`.

  * Bump cmark version to >= 0.3.4.

  * Improved Windows installer (#2205, thanks to nkalvi).
    Users can now select a per-user or systemwide install, and can set
    the installation path.  At the end of installation, the install location
    is given.  The install location is also now given in the list of
    installed programs in Control Panel.  Cleaner WiX syntax is used for
    setting the path.

  * Added `download_stats` target to Makefile.

## pandoc 1.14.0.4 (2015-06-02)

  * Added missing commonmark template.

  * Improved try pandoc (moved button, show raw command).

## pandoc 1.14.0.3 (2015-06-01)

  * Allow compilation with syb 0.5.*.

  * Custom writer:  fixed some compiler warnings for ghc < 7.10.

## pandoc 1.14.0.2 (2015-05-31)

  * Allow building with hslua 0.4.

## pandoc 1.14.0.1 (2015-05-28)

  * Fixed problem with building of `reference.docx` and `reference.odt`
    when the `embed_data_files` flag is used.  Instead of having a phase
    of the build where `reference.docx` and `reference.odt` are created
    from their constituent data files, we now construct these archives
    from their constituents when a `docx` or `odt` is built.  The
    constituent files have been moved from `extra-source-files` to
    `data-files`, and `reference.docx` and `reference.odt` have been
    removed. Users can create their own `reference.docx` or
    `reference.odt` by using pandoc to create a simple `docx` or `odt`.
    `make-reference-files.hs` has been removed, simplifying the build
    process (#2187)

  * Don't include generated man pages in extra-source-files (#2189).

  * Bumped upper bound for aeson.

  * ConTeXt writer:  create internal link anchors for Div elements with
    identifiers.  (This is needed for linked citations to work.)

## pandoc 1.14 (2015-05-27)

### New features

  * Added `commonmark` as input and output format.

  * Added `--verbose` flag for debugging output in PDF production (#1840,
    #1653).

  * Allow wildcards in `--epub-embed-font` arguments (#1939).

  * Added `--latex-engine-opt` option (#969, #1779, Sumit Sahrawat).

  * Added `shortcut_reference_links` extension (Konstantin Zudov, #1977).
    This is enabled by default for those markdown flavors that
    support reading shortcut reference links, namely: `markdown`,
    `markdown_strict`, `markdown_github`, `markdown_php`.
    If the extension is enabled, the reader parses shortcut reference
    links like `[foo]`, and the writer creates such links unless doing
    so would cause problems.  Users of markdown flavors that support
    shortcut reference links should not notice a difference in reading
    markdown, but the markdown pandoc produces may differ.
    If shortcut links are not desired, the extension can be disabled
    in the normal way.

### Behavior changes

  * `--toc` is now supported for `docx` output (#458, Nikolay Yakimov).
    A "dirty" TOC is created at the beginning of document.
    It can be regenerated after the document has been opened.

  * An implicit `--filter pandoc-citeproc` is now triggered only when the
    `--bibliography` option is used, and not when the `bibliography`
    field in metadata is specified (#1849).

  * Markdown reader:

    + Reference links with `implicit_header_references` are no longer
      case-sensitive (#1606).
    + Definition lists no longer require indentation for first line (#2087).
      Previously the body of the definition (after the `:` or `~` marker)
      needed to be in column 4.  This commit relaxes that requirement,
      to better match the behavior of PHP Markdown Extra.  So, now
      this is a valid definition list:

            foo
            : bar
    + Resolve a potentially ambiguity with table captions:

            foo

              : bar

              -----
              table
              -----

      Is "bar" a definition, or the caption for the table?  We'll count
      it as a caption for the table.
    + Disallow headerless pipe tables (#1996), to conform to GFM and PHP
      Markdown Extra.  Note:  If you have been using headerless pipe tables,
      this change may cause existing tables to break.
    + Allow pipe tables with header but no body (#2017).
    + Allow a digit as first character of a citation key (Matthias Troffaes).
      See https://github.com/jgm/pandoc-citeproc/issues/97

  * LaTeX reader:

    + Don't limit includes to `.tex` extension (#1882).
      If the extension is not `.tex`, it must be given explicitly in
      the `\input` or `\include`.

  * Docx reader:

    + Allow numbering in the style file.  This allows inherited styles
      with numbering (lists) (Jesse Rosenthal).

  * Org reader:

    + Support smart punctuation (Craig Bosma).
    + Drop trees with a :noexport: tag (Albert Krewinkel). Trees having a
      `:noexport:` tag set are not exported.  This mirrors org-mode.
    + Put header tags into empty spans (Albert Krewinkel, #2160).
      Org mode allows headers to be tagged: `* Headline  :TAG1:TAG2`.
      Instead of being interpreted as part of the headline, the tags are now
      put into the attributes of empty spans.  Spans without textual content
      won't be visible by default, but they are detectable by filters.  They
      can also be styled using CSS when written as HTML.
    + Generalize code block result parsing (Albert Krewinkel).
      Previously, only code blocks were recognized as result blocks;
      now, any kind of block can be the result.

  * Append newline to the LineBreak in Dokuwiki, HTML, EPUB,
    LaTeX, MediaWiki, OpenDocument, Texinfo writers (#1924, Tim Lin).

  * HTML writer:

    + Add "inline" or "display" class to math spans (#1914).
      This allows inline and display math to be styled differently.
    + Include raw latex blocks if `--mathjax` specified (#1938).
    + Require highlighting-kate >= 0.5.14 (#1903).
      This ensures that all code blocks will be wrapped in a `div`
      with class `sourceCode`.  Also, the default highlighting CSS
      now adds `div.sourceCode { x-overflow: auto; }`, which means
      that code blocks (even with line numbers) will acquire a scroll
      bar on screens too small to display them (e.g. mobile phones).
      See also jgm/highlighting-kate#65.

  * LaTeX writer:

    + Use a declaration for tight lists (Jose Luis Duran, Joseph
      Harriott). Previously, pandoc hard-coded some commands to make
      tight lists in LaTeX.  Now we use a custom command instead,
      allowing the styling to be changed in a macro in the header.
      (Note:  existing templates may need to be modified to include
      the definition of this macro.  See the current template.)
    + Beamer output: if the header introducing a slide has the
      class `fragile`, add the `[fragile]` option to the slide (#2119).

  * MediaWiki writer:

    + Use `File:` instead of the deprecated `Image:` for images and
      other media files (Greg Rundlett).

  * DocBook writer:

    + Render a `Div (id,_,_) [Para _]` element as a `para` element
      with an `id` attribute.  This makes links to citations work in
      DocBook with pandoc-citeproc.

  * RST writer:

    + Normalize headings to sequential levels (Nikolay Yakimov).
      This is pretty much required by docutils.
    + Treat headings in block quotes, etc as rubrics (Nikolay Yakimov).
    + Better handling of raw latex inline (#1961).  We use
      `` :raw-latex:`...` `` and add a definition for this role to
      the template.

  * EPUB writer:

    + Remove `linear=no` from cover `itemref` (#1609).
    + Don't use `sup` element for epub footnotes (#1995).
      Instead, just use an a element with class `footnoteRef`.
      This allows more styling options, and provides better results
      in some readers (e.g. iBooks, where anything inside the a
      tag breaks popup footnotes).
    + Take TOC title from `toc-title` metadata field.

  * Docx writer:

    + Implemented `FirstParagraph` style (Jesse Rosenthal).
      Following the ODT writer, we add the `FirstParagraph` style to the
      first text paragraph following an image, blockquote, table, heading,
      or beginning of document.  This allows it to be styled differently.
      The default is for it to be the same as `Normal`.
    + Added `BodyText` style (Jesse Rosenthal).
      We apply a `BodyText` style to all unstyled paragraphs. This is,
      essentially, the same as `Normal`, except that since not everything
      inherits from `BodyText` (the metadata won't, for example, or
      the headers or footnote numbers), we can change the text in the body
      without having to make exceptions for everything.  If we do want to
      change *everything*, we can still do it through `Normal`.
    + Altered `Blockquote` style slightly (Jesse Rosenthal).
      Since `BlockQuote` derives from `BodyText`, we just want to specify
      by default that it won't indent, regardless of what `BodyText` does.
      Note that this will not produce any visible difference in the default
      configuration.
    + Take TOC title from `toc-title` metadata field (Nikolay Yakimov).
    + Added a style to figure images (Nikolay Yakimov).
      Figures with empty captions use style `Figure`.
      Figures with nonempty captions use style `Figure with Caption`, which
      is based on `Figure`, and additionally has `keepNext` set.

  * ODT writer:

    + Added figure captions (Nikolay Yakimov). The following styles are
      used for figures:
      `Figure` -- for figure with empty caption),
      `FigureWithCaption` (based on `Figure`) -- for figure with caption,
      `FigureCaption` (based on `Caption`) -- for figure captions.
      Also, `TableCaption` (based on `Caption`) is used for table captions.

### API changes

  * New `Text.Pandoc.Error` module with `PandocError` type
    (Matthew Pickering).

  * All readers now return `Either PandocError Pandoc` instead of `Pandoc`
    (Matthew Pickering).  This allows better handling of errors.

  * Added `Text.Pandoc.Writers.CommonMark`, exporting `writeCommonMark`.

  * Added `Text.Pandoc.Readers.CommonMark`, exporting `readCommonMark`.

  * Derive `Data` and `Typeable` instances for `MediaBag`, `Extension`,
    `ReaderOptions`, `EPUBVersion`, `CiteMethod`, `ObfuscationMethod`,
    `HTMLSlideVariant`, `TrackChanges`, `WriterOptions` (Shabbaz
    Youssefi).

  * New `Ext_shortcut_reference_links` constructor for `Extension`
    (Konstantin Zudov).

###  Bug fixes

  * Markdown reader:

    + Allow smart `'` after inline math (#1909, Nikolay Yakimov).
    + Check for tex macros after indented code (#1973).
    + Rewrote `charsInBalancedBrackets` for efficiency.
    + Make sure a closing `</div>` doesn't get included in a
      definition list item (#2127).
    + Don't parse bracketed text as citation if it might be a link,
      image, or footnote (Nikolay Yakimov).
    + Require space after key in mmd title block (#2026, Nikolay
      Yakimov).  Require space after key-value delimiter colon in mmd title
      block.
    + Require nonempty value in mmd title block (Nikolay Yakimov).
    + Disable all metadata block extensions when parsing
      metadata field values (#2026, Nikolay Yakimov).  Otherwise we
      could get a mmd title block inside YAML metadata, for example.

  * HTML reader:

    + Improve self-closing tag detection in `htmlInBalanced` (#2146).
    + Handle tables with `<th>` in body rows (#1859, mb21).
    + Fixed `htmlTag` (#1820).  If the tag parses as a comment, we check
      to see if the input starts with `<!--`. If not, it's bogus comment
      mode and we fail `htmlTag`.
    + Handle `base` tag; if it has an `href` value, this is added to
      all relative URLs in links and images.

  * DocBook reader:

    + Look inside "info" elements for section titles (#1931).

  * Docx reader:

    + Parse images in deprecated vml format (Jesse Rosenthal).
    + Allow sub/superscript verbatims (Jesse Rosenthal).
      Verbatim usually shuts off all other run styles, but we don't want it
      to shut off sub/superscript.

  * LaTeX reader:

    + Handle `tabular*` environment (#1850).
      Note that the table width is not actually parsed or taken into
      account, but pandoc no longer chokes on it.
    + Ignore options in `\lstinline` rather than raising error (#1997).
    + Add some test cases for simple tables (Mathias Schenner).
    + Handle valign argument in tables (Mathias Schenner) (currently
      we just ignore this).
    + Allow non-empty colsep in tables (Mathias Schenner).
      The `tabular` environment allows non-empty column separators
      with the "@{...}" syntax. Previously, pandoc would fail to
      parse tables if a non-empty colsep was present. With this
      commit, these separators are still ignored, but the table gets
      parsed. A test case is included.
    + Recognize `\newpage` as a block command.
    + Allow block content in `\title{}` (#2001).
    + Check for block-level newcommand aliases in blockCommand (Nikolay
      Yakimov).
    + Guard against paragraph starting with inline macro (Nikolay Yakimov).
    + Properly gobble spaces after `\\` (#2007).

  * Textile reader:

    +  Handle newlines in table cells, and empty cells (#1919).

  * Org reader:

    + Allow image links with non-image targets (Hans-Peter Deifel).
      This matches behavior of Org-Mode for links like
      `[[http://example.com][https://www.haskell.org/static/img/logo.png]]`.

  * Docbook writer:

    + Don't print empty id attributes (thanks to Steve Horne).

  * HTML writer:

    + Fixed list-style-type for numbered example lists.
      Should be "decimal," not "example" (#1902).
    + Do not omit missing `alt` attribute on `img` tag (#1131,
      Konstantin Zudov).
    + Allow multiple colgroups in table (#2122).
    + In revealjs, ensure that lists in speaker notes don't add "fragment"
      classes, which can cause additional keypresses to be needed to
      advance a slide (#1394).

  * LaTeX writer:

    + Don't escape `$` in URL (#1913).
    + Don't use listings in headers (Matthew Pickering, #1963).
    + Recognize book documentclass if set in metadata (#1971).
      This sets `--chapters` implicitly if the documentclass in metadata
      is a book documentclass.  Previously this was done only if a book
      documentclass was set in a variable.
    + Add a `\label` in `\hyperdef` for Div, Span (or links don't work).
    + Make `mainlang` work when `lang` is in metadata (#2174).

  * Texinfo writer:

    + Fix wrapping by using breakable spaces (Tim Lin).

  * RST writer:

    + Fixed toc depth in RST writer.  Previously the depth was being
      rendered as a floating point number with a decimal point.

  * Markdown writer:

    + Improved escaping (#2086).  `<` should not be escaped as `\<`, for
      compatibility with original Markdown.  We now escape `<` and `>`
      with entities.  Also, we now backslash-escape square brackets.
    + Avoid introducing spurious list items through wrapping (#1946).
    + Don't emit span tags if plain or raw HTML disabled.

  * MediaWiki writer:

    + Convert spaces to underscores in wikilink URL (#1982), like MediaWiki.

  * AsciiDoc writer:

    + Insert some needed blank lines (#1860).
    + Avoid wrapping after list marker (#1858).

  * EPUB writer:

    + Properly handle internal links to IDs in spans, divs (#1884).
    + Use plain writer for metadata dc: fields (#2121).
      This gives better results when we have, e.g. multiple paragraphs.
      Note that tags aren't allowed in these fields.
    + Properly handle image links without an extension (#1855).
    + Improved chapter splitting and internal link rewriting (#1887,
      #2162, #2163).  This will ensure that internal links work and
      that the references section produced by pandoc-citeproc is
      in its own chapter.
    + Fixed handling of svg images (#2183).

  * ICML writer:

    + Better handling of raw blocks and inlines (#1951).
      Previously these were always escaped and printed verbatim.
      Now they are ignored unless the format is `icml`, in which
      case they are passed through unescaped.
    + Fixed image URIs in ICML output (gohai).

  * Custom writer:

    + Raise error if loadstring returns an error status.
    + Raise `PandocLuaException` instead of using 'error'.
      Eventually we'll change the return type so that no exception
      is involved, but at least this can be trapped.
    + Use UTF-8 aware bytestring conversion.
    + Set foreign encoding to UTF-8 (Nikolay Yakimov, #2101, #1634).
      Also factored out ByteString, since it's only used as an intermediate
      representation.

  * Docx writer:

    + Copy hyphenation settings from reference.docx (Nikolay Yakimov).
    + Filter out illegal XML characters (#1992, Matthew Pickering).
    + Added `noProof` to docx syntax highlighting `SourceCode` style.
    + Added footnotes id -1 and 0 (Jesse Rosenthal).
      Word uses, by default, footnotes with id -1 and 0 for separators. If a
      user modifies `reference.docx`, they will end up with a `settings.xml`
      file that references these footnotes, but no such footnotes in the
      document. This will produce a corruption error. Here we add these to the
      document and `settings.xml` file, so future modifications won't break
      the file.
    + Handle lists correctly inside table cells (Jesse Rosenthal).
      Previously we didn't transform lists inside table cells.
    + Set firstRow information in tables (Nikolay Yakimov).
    + Don't replace `SourceCode` style in `reference.docx` if it is defined
      there (Nikolay Yakimov, #1872).  If `--no-highlight` specified, remove
      any `SourceCode` and `*Tok` styles in `reference.docx`.
    + Attempt to match international style names (#1607, Nikolay Yakimov).
    + Set these styles as custom (Nikolay Yakimov): `Author`, `Abstract`,
      `Compact`, `Image Caption`, `Table Caption`, `Definition Term`,
      `Definition`, `First Paragraph`.
    + Rename these styles to correspond with Word `Normal.dotm` (Nikolay
      Yakimov): `Block Quote -> Block Text`, `Link -> Hyperlink`,
      `Footnote Ref -> Footnote Reference`.
    + Added `Caption` style (Nikolay Yakimov).
    + Changed these styles' inheritance (Nikolay Yakimov):
      `Image Caption <- Caption`, `Table Caption <- Caption`.
    + Remove `SourceCode` style from `reference.docx` (#1872).
      This is added automatically by the docx writer.
    + Added toc heading style to `reference.docx` (Nikolay Yakimov).

  * `Text.Pandoc.PDF`

    + Don't suggest "Try xelatex" if xelatex already in use (mb21, #1832).
    + More comprehensible errors on image conversion (#2067).
      EPS can't be supported without shelling out to something like
      ImageMagick, but at least we can avoid mysterious error messages.

  * `Text.Pandoc.Shared`:

    + Make safeRead safe (#1801, Matthew Pickering).
    + Added `mapLeft`, `hush` (Matthew Pickering).

  * `Text.Pandoc.Pretty`:

    + Remove partial function (Matthew Pickering).

  * `Text.Pandoc.SelfContained`:

    + Add `;charset=utf-8` to script mime type if missing (#1842).
    + Improved building of data URIs (#1940).  Now base64 is used except
      for `text/*` mime types.
    + `cssURLs` no longer tries to fetch fragment URLs (#2121).
    + Properly handle data URIs in css urls (#2129).
      Use a proper CSS parser (adds dependency on `text-css`).

  * `Text.Pandoc.UTF8`:

    + Better handling of bare CRs in input files (#2132).
      Previously we just stripped them out; now we convert
      other line ending styles to LF line endings.

  * `Text.Pandoc.ImageSize`:

    + Fixed some exif header parsing bugs (#1834).
    + Make imageSize return an Either, not a Maybe (#1834).
      Use `runGetOrFail` (with `binary >= 0.7`) to return `Left` on
      parse failure (rather than `error`).
    + Improved warnings when image size can't be determined.
    + Removed error landmines (Matthew Pickering).

  * Added woff2 to MIME types (Alfred Wechselberger).

  * pandoc:  When a binary input format is used, warn that file
    arguments past the first one are being ignored (Matthew Pickering).

### Template changes

  * LaTeX template:

    + Degrade gracefully if `\paragraph` not defined.
    + Include `grffile` together with `graphicx` (#2074).
      This properly handles filenames containing spaces and dots.
    + Redefine `\paragraph`, `\subparagraph`...  to behave more
      like section headers (#1658).
    + Import hyperref before polyglossia to avoid an error with xelatex,
      "please load package hyperref before bidi package" (Nick Bart).
    + Added `toccolor` variable to control link color in toc (Kaixhin).

  * LaTeX, Beamer templates:

    + Provide `\tightlist`, which is now used by the LaTeX writer.
    + Use polyglossia in beamer (#85).
    + Use `bibliography` instead of `biblio-files`
      (#1661).  Also use `\addbibresource` instead of `\bibliography` for
      biblatex.
    + Added `setotherlanguages` in polyglossia. This uses an `otherlang`
      variable that is derived from a comma-separated list in `lang`;
      the last language is `mainlang` and the others are `otherlang`.

  * EPUB templates:

    + Use `div`, not `p`, for "rights" on title page.
    + Added header-includes, include-before, include-after (#1987).

  * OpenDocument template:

    + Use `text:p` instead of `text:h` for title.
      Using `text:h` causes problems with numbering.  Closes #2059.
      Thanks to @nkalvi for diagnosing this.

  * reveal.js template:

    + Link to non-minified css, js.  The minified versions no longer
      ship with the library.
    + Correctly include style CSS (#1949).
    + New configurable options options: `center`, `maxScale`, `slideNuber`
      (Dmitry Smirnov, pandoc-templates#89).
    + Moved custom CSS after theme.  This allows custom CSS to modify
      themes, instead of being replaced by them.
    + Allow `center` to be set to false.

### Under the hood improvements

  * Removed pre-built `reference.docx` and `reference.odt` (Nikolay
    Yakimov).  Instead the repository now includes the component text files,
    and the zipped binaries are built from these using a helper
    program, `make-reference-files`.  This should make maintenance of
    these components easier going forward.

  * `Text.Pandoc.Parsing`:

    + Added new `<+?>` combinator (Nikolay Yakimov).
    + Added `stateHeaderKeys` to `ParserState`.

  * `make_deb.sh` fixes:

    + Detect architecture.
    + Add Installed-Size to debian package control file (#1900).
    + Use `fakeroot` to get permissions right.
    + Use `mkdir` and `cp` instead of `install`.
    + Set permissions of directories to 755.
    + Install in `/usr` rather than `/usr/local`.
    + Compress man pages.
    + Combine copyright files for `pandoc`, `pandoc-citeproc`.

  * Added `Text.Pandoc.Compat.Locale` and `old-locale` flag
    to assist with transition to `time` 1.5.

  * Updated CONTRIBUTING.md with information about issue tags (Matthew
    Pickering).

  * Updated travis installs to the new sudo-less syntax (Tim Lin).

  * Updated dependency version bounds.

  * EPUB tests:  don't use `joinPath`, which varies across platforms.
    Instead, use a forward-slash to join paths, regardless of the
    platform. This matches the way `MediaBag` now works.

  * Clarify JSON input and output in usage message (Caleb McDaniel).

  * Improved INSTALL instructions.

  * Always build man pages.  Removed make-pandoc-man-pages flag.

  * Makefile:  removed man target, now that we generate man pages by default.

  * README:

    + Fixed typos (J. Lewis Muir).
    + Added documentation on backtick_code_blocks (#2135, Nikolay Yakimov).
    + Added note on in-field markup in biblio databases (Nick Bart).
    + Fixed misleading example of raw HTML block.
    + Various minor formatting and consistency fixes for the program
      options (Andreas Lööw).
    + Made definition lists for options all "loose" for consistency.
    + Added YAML biblio format to table, and note on `pandoc-citeproc`'s
      `--bib2json` and `--bib2yaml` options (Nick Bart).
    + Removed obsolete reference to `mods2yaml` (Nick Bart).
    + Added section on syntax highlighting.
    + Documented `toccolor` variable.

## pandoc 1.13.2.1 (2015-04-15)

  * Updated to build with ghc 7.10.1.

  * Bumped package upper bounds for filepath, blaze-html, blaze-markup.

## pandoc 1.13.2 (2014-12-20)

  * TWiki Reader: add new new twiki reader (API chaneg, Alexander Sulfrian).

  * Markdown reader:

    + Better handling of paragraph in div (#1591).
      Previously text that ended a div would be parsed as Plain
      unless there was a blank line before the closing div tag.
    + Don't treat a citation as a reference link label (#1763).
    + Fix autolinks with following punctuation (#1811).
      The price of this is that autolinked bare URIs can no longer
      contain `>` characters, but this is not a big issue.
    + Fix `Ext_lists_without_preceding_blankline` bug (#1636, Artyom).
    + Allow `startnum` to work without `fancy_lists`. Formerly
      `pandoc -f markdown-fancy_lists+startnum` did not work properly.

  * RST reader (all Daniel Bergey):

    + Parse quoted literal blocks (#65).  RST quoted literal blocks are
      the same as indented literal blocks (which pandoc already supports)
      except that the quote character is preserved in each line.
    + Parse RST class directives. The class directive accepts one or more
      class names, and creates a Div value with those classes.  If the
      directive has an indented body, the body is parsed as the children of
      the Div.  If not, the first block following the directive is made a
      child of the Div. This differs from the behavior of rst2xml, which
      does not create a Div element.  Instead, the specified classes are
      applied to each child of the directive.  However, most Pandoc Block
      constructors to not take an Attr argument, so we can't duplicate this
      behavior.
    + Warn about skipped directives.
    + Literal role now produces Code. Code role should have "code" class.
    + Improved support for custom roles

        - Added `sourceCode` to classes for `:code:` role, and anything
          inheriting from it.
        - Add the name of the custom role to classes if the Inline
          constructor supports Attr.
        - If the custom role directive does not specify a parent role,
          inherit from the `:span:` role.

      This differs somewhat from the `rst2xml.py` behavior.  If a custom
      role inherits from another custom role, Pandoc will attach both
      roles' names as classes.  `rst2xml.py` will only use the class of
      the directly invoked role (though in the case of inheriting from a
      `:code:` role with a `:language:` defined, it will also provide the
      inherited language as a class).
    + Warn about ignored fields in role directives.

  * LaTeX reader:

    + Parse label after caption into a span instead of
     inserting an additional paragraph of bracketed text (#1747).
    + Parse math environments as inline when possible (#1821).
    + Better handling of `\noindent` and `\greektext` (#1783).
    + Handle `\texorpdfstring` more gracefully.
    + Handle `\cref` and `\sep` (Wikiwide).
    + Support `\smartcite` and `\Smartcite` from biblatex.

  * HTML reader:

    + Retain display type of MathML output (#1719, Matthew Pickering).
    + Recognise `<br>` tags inside `<pre>` blocks (#1620, Matthew Pickering).
    + Make `embed` tag either block or inline (#1756).

  * DocBook reader:

    + Handle `keycombo`, `keycap` (#1815).
    + Get string content in inner tags for literal elements (#1816).
    + Handle `menuchoice` elements better, with a `>` between (#1817).
    + Include `id` on section headers (#1818).
    + Document/test "type" as implemented (Brian O'Sullivan).
    + Add support for calloutlist and callout (Brian O'Sullivan).
      We treat a calloutlist as a bulleted list. This works well in practice.
    + Add support for `classname` (Bryan O'Sullivan).

  * Docx reader:

    + Fix window path for image lookup (Jesse Rosenthal).
      Don't use os-sensitive "combine", since we always want the paths in our
      zip-archive to use forward-slashes.
    + Single-item headers in ordered lists are headers (Jesse Rosenthal).
      When users number their headers, Word understands that as a single item
      enumerated list. We make the assumption that such a list is, in fact,
      a header.
    + Rewrite rewriteLink to work with new headers (Jesse Rosenthal).
      There could be new top-level headers after making lists, so we have to
      rewrite links after that.
    + Use polyglot header list (Jesse Rosenthal).
      We're just keeping a list of header formats that different languages
      use as their default styles. At the moment, we have English, German,
      Danish, and French. We can continue to add to this.
      This is simpler than parsing the styles file, and perhaps less
      error-prone, since there seems to be some variations, even within a
      language, of how a style file will define headers.
    + Remove header class properly in other langs (Jesse Rosenthal).
      When we encounter one of the polyglot header styles, we want to remove
      that from the par styles after we convert to a header. To do that, we
      have to keep track of the style name, and remove it appropriately.
    + Account for external link URLs with anchors. Previously, if a URL
      had an anchor, the reader would incorrectly identify it as an
      internal link and return only the anchor as URL. (Caleb McDaniel)
    + Fix for Issue #1692 (i18n styles) (Nikolay Yakimov).

  * Org reader:

    + Added state changing blanklines (Jesse Rosenthal).
      This allows us to emphasize at the beginning of a new paragraph (or, in
      general, after blank lines).
    + Fixed bug with bulleted lists:

        - a
        - b
        * c

      was being parsed as a list, even though an unindented `*`
      should make a heading.  See
      <http://orgmode.org/manual/Plain-lists.html#fn-1>.
    + Org reader: absolute, relative paths in link (#1741, Albert
      Krewinkel). The org reader was too restrictive when parsing links;
      some relative links and links to files given as absolute paths
      were not recognized correctly.
    + Org reader:  allow empty links (jgm/gitit#471, Albert Krewinkel).
      This is important for use in gitit, which uses empty links
      for wikilinks.
    + Respect indent when parsing Org bullet lists (#1650, Timothy
      Humphries).  Fixes issue with top-level bullet list parsing.
    + Fix indent issue for definition lists (Timothy Humphries,
      see #1650, #1698, #1680).
    + Parse multi-inline terms correctly in definition list (#1649,
      Matthew Pickering).
    + Fix rules for emphasis recognition (Albert Krewinkel).
      Things like `/hello,/` or `/hi'/` were falsy recognized as emphasised
      strings.  This is wrong, as `,` and `'` are forbidden border chars and
      may not occur on the inner border of emphasized text.
    + Drop COMMENT document trees (Albert Krewinkel).
      Document trees under a header starting with the word `COMMENT` are
      comment trees and should not be exported.  Those trees are dropped
      silently (#1678).
    + Properly handle links to `file:target` (Albert Krewinkel).
      Org links like `[[file:target][title]]` were not handled correctly,
      parsing the link target verbatim.  The org reader is changed such that
      the leading `file:` is dropped from the link target (see #756, #1812).
    + Parse LaTeX-style MathML entities (#1657, Albert Krewinkel).
      Org supports special symbols which can be included using LaTeX syntax,
      but are actually MathML entities.  Examples for this are
      `\nbsp` (non-breaking space), `\Aacute` (the letter A with accent acute)
      or `\copy` (the copyright sign ©)

  * EPUB reader:

    + URI handling improvements. Now we outsource most of the work to
      `fetchItem'`. Also, do not include queries in file extensions (#1671).

  * LaTeX writer:

    + Use `\texorpdfstring` for section captions when needed (Vaclav Zeman).
    + Handle consecutive linebreaks (#1733).
    + Protect graphics in headers (Jesse Rosenthal).
      Graphics in `\section`/`\subsection` etc titles need to be `\protect`ed.
    + Put `~` before header in list item text (Jesse Rosenthal).
      Because of the built-in line skip, LaTeX can't handle a section header
      as the first element in a list item.
    + Avoid using reserved characters as `\lstinline` delimiters (#1595).
    + Better handling of display math in simple tables (#1754).
      We convert display math to inline math in simple tables,
      since LaTeX can't deal with display math in simple tables.
    + Escape spaces in code (#1694, Bjorn Buckwalter).

  * MediaWiki writer:

    + Fixed links with URL = text. Previously these were rendered as bare
      words, even if the URL was not an absolute URL (#1825).

  * ICML writer:

    + Don't force all citations into footnotes.

  * RTF writer:

    + Add blankline at end of output (#1732, Matthew Pickering).

  * RST writer:

    + Ensure blank line after figure.
    + Avoid excess whitespace after last list item (#1777).
    + Wrap line blocks with spaces before continuations (#1656).
    + Fixed double-rendering of footnotes in RST tables (#1769).

  * DokuWiki writer:

    + Better handling of block quotes. This change ensures that
      multiple paragraph blockquotes are rendered using native `>`
      rather than as HTML (#1738).
    + Fix external images (#1739). Preface relative links with ":",
      absolute URIs without. (Timothy Humphries)

  * HTML writer:

    + Use protocol-relative URL for mathjax.
    + Put newline btw img and caption paragraph.
    + MathML now outputted with tex annotation (#1635, Matthew Pickering).
    + Add support for KaTeX HTML math (#1626, Matthew Pickering).
      This adds `KaTeX` to `HTMLMathMethod` (API change).
    + Don't double render when `email-obfuscation=none` (#1625, Matthew
      Pickering).
    + Make header attributes work outside top level (#1711).
      Previously they only appeared on top level header elements.
      Now they work e.g. in blockquotes.

  * ODT writer:

    + Correctly handle images without extensions (#1729).
    + Strip querystring in ODT write (#1682, Todd Sifleet).

  * FB2 writer:

    + Add newline to output.

  * EPUB writer:

    + Don't add `sourceURL` to absolute URIs (#1669).
    + Don't use unsupported `opf:title-type` for epub2.
    + Include "landmarks" section in nav document for epub3 (#1757).
    + Removed playOrder from navpoint elements in ncx file (#1760).
      These aren't required, and they make manual modification of epubs
      difficult.
    + Extract title even from structured title.
    + Don't include nav node in spine unless `--toc` was requested.
      Previously we included it in the spine with `linear="no"`, leading
      to odd results in some readers (#1593).
    + Fixed absolute URI detection (#1672).
    + Correctly resolve relative URIs (#1671).
    + Use regular page template for `nav.xhtml`, including doctype (#1759).

  * Docx writer:

    + Put docx table captions above tables (#1641, Nikolay Yakimov).
    + Get the page width from the reference docx file, and use
      it to scale images that are too large to fit (Grégory Bataille).
    + Partial fix for #1607 (Nikolay Yakimov). International heading styles
      are inferred based on `<w:name val="heading #">` fallback, if there
      are no en-US "Heading#" styles
    + Look in user data dir for archive `reference.docx`.
    + Renumber header and footer relationships to avoid collisions (Jesse
      Rosenthal). We previously took the old relationship names of the
      headers and footer in secptr. That led to collisions. We now make
      a map of available names in the relationships file, and then rename
     in secptr.

  * ConTeXt writer:

    + Add function toLabel (Mark Szepieniec).
      This function can be used to sanitize reference labels so that
      they do not contain any of the illegal characters \#[]",{}%()|= .
      Currently only Links have their labels sanitized, because they
      are the only Elements that use passed labels.

  * `Text.Pandoc.Shared`:

    + Moved import of `toChunks` outside of CPP conditional (#1590).
    + Fix `inDirectory` to reset to the original directory in case
      an exception occurs (Freiric Barral).

  * Templates:

    + LaTeX template: load polyglossia before bibtex (jgm/pandoc-templates#70).
      Thanks to bluebirch.
    + LaTeX template: Added `\VerbatimFootnotes` if there is verbatim in notes
      (#1616).
    + LaTeX template:  Add shorthands=off to babel options (#1648).
    + EPUB, EPUB3 templates: Added `id="cover"` to body of cover page.
      This aids styling, making it possible for example to set 0 margins
      on the title page (#1758).
    + EPUB, EPUB3 templates:  Handle structured metadata on titlepage.
      Previously we just expected 'title', 'subtitle', 'author', 'date'.
      Now we still support those, but also support the format recommended
      for epub metadata in the pandoc README:

        ---
        title:
        - type: main
          text: My Book
        - type: subtitle
          text: An investigation of metadata
        creator:
        - role: author
          text: John Smith
        - role: editor
          text: Sarah Jones
        identifier:
        - scheme: DOI
          text: doi:10.234234.234/33
        publisher:  My Press
        rights:  (c) 2007 John Smith, CC BY-NC
        ...

  * `Text.Pandoc.Templates.getDefaultTemplate`:
    don't fail when called with "fb2" (#1660).

  * `Text.Pandoc.Parsing`:

    + Fixed `inlineMath` so it handles `\text{..}` containing `$`.
      For example: `$x = \text{the $n$th root of $y$}` (#1677).
    + Change `parseFromString` to fail if not all input is consumed.
      (Matthew Pickering)
    + Moved `addWarning` from Markdown reader to `Parsing`, so it can be
      used by more readers (API change, Daniel Bergey).

  * `Text.Pandoc.Pretty`:

    +  Improve performance of `realLength` (Matthew Pickering).
    +  Make CR + BLANKLINE = BLANKLINE. This fixes an extra blank line we
       were getting at the end of markdown fragments (as well as rst, org,
       etc.) (#1705).

  * `Text.Pandoc.MIME`:

    + Add mime type for WebVTT (Jason Ronallo).
    + Changed mime type for `otf` to `application/vnd.ms-opentype` (#1761).
      This is needed for epub3 validation.

  * `Text.Pandoc.MediaBag`:

    + Fix Windows specific path problems (#1597).

  * `Text.Pandoc.Shared`:

    + Make `collapseFilePath` OS-agnostic (Matthew Pickering).

  * Link the test suite using `-threaded`.
    This allows the test suite to be run using `+RTS -N`.

  * Added `network` dependency under `network-uri` flag in test section.

  * Give better error messages when someone tries to convert from
    pdf, doc, odt (#1683).

  * Added `track` to list of tags treated by `--self-contained` (#1664).


## pandoc 1.13.1 (2014-08-30)

  * Fixed `--self-contained` with Windows paths (#1558).
    Previously `C:\foo.js` was being wrongly interpreted as a URI.

  * HTML reader:  improved handling of tags that can be block or inline.
    Previously a section like this would be enclosed in a paragraph,
    with RawInline for the video tags (since video is a tag that can
    be either block or inline):

        <video controls="controls">
           <source src="../videos/test.mp4" type="video/mp4" />
           <source src="../videos/test.webm" type="video/webm" />
           <p>
              The videos can not be played back on your system.<br/>
              Try viewing on Youtube (requires Internet connection):
              <a href="http://youtu.be/etE5urBps_w">Relative Velocity on
        Youtube</a>.
           </p>
        </video>

    This change will cause the video and source tags to be parsed
    as RawBlock instead, giving better output.
    The general change is this:  when we're parsing a "plain" sequence
    of inlines, we don't parse anything that COULD be a block-level tag.

  * Docx reader:

    + Be sensitive to user styles.  Note that "Hyperlink" is
      "blacklisted," as we don't want the default underline styling to be
      inherited by all links by default (Jesse Rosenthal).
    + Read single paragraph in table cell as `Plain` (Jesse Rosenthal).
      This makes to docx reader's native output fit with the way the markdown
      reader understands its markdown output.

  * Txt2Tags reader:

    + Header is now parsed only if standalone flag is set (Matthew Pickering).
    + The header is now parsed as meta information. The first line is the
      `title`, the second is the `author` and third line is the `date`
      (Matthew Pickering).
    + Corrected formatting of `%%mtime` macro (Matthew Pickering).
    + Fixed crash when reading from stdin.

  * Textile writer:  Extended the range of cases where native textile
    tables will be used (as opposed to raw HTML):  we now handle any
    alignment type, but only for simple tables with no captions.

  * EPUB writer:  Don't use page-progression-direction in EPUB2, which
    doesn't support it.  Also, if page-progression-direction not specified
    in metadata, don't include the attribute even in EPUB3; not including it
    is the same as including it with the value "default", as we did before.
    (#1550)

  * Org writer: Accept example lines with indentation at the beginning
    (Calvin Beck).

  * DokuWiki writer:

    + Refactor to use Reader monad (Matthew Pickering).
    + Avoid using raw HTML in table cells; instead, use `\\`
      instead of newlines (Jesse Rosenthal).
    + Properly handle HTML table cell alignments, and use spacing
      to make the tables look prettier (#1566).

  * Docx writer:

    + Bibliography entries get `Bibliography` style (#1559).
    + Implement change tracking (Jesse Rosenthal).

  * LaTeX writer:

    + Fixed a bug that caused a table caption to repeat across all pages
      (Jose Luis Duran).
    + Improved vertical spacing in tables and made it customizable using
      standard lengths set by booktab.  See
      <https://groups.google.com/forum/#!msg/pandoc-discuss/qMu6_5lYy0o/ZAU7lzAIKw0J>
      (Jose Luis Duran).
    + Added `\strut` to fix spacing in multiline tables (Jose Luis Duran).
    + Use `\tabularnewline` instead of `\\` in table cells (Jose Luis Duran).
    + Made horizontal rules more flexible (Jose Luis Duran).

  * Text.Pandoc.MIME:

    + Added `MimeType` (type synonym for `String`) and `getMimeTypeDef`.
      Code cleanups (Artyom Kazak).

  * Templates:

    + LaTeX template: disable microtype protrusion for typewriter font (#1549,
      thanks lemzwerg).

  * Improved OSX build procedure.

  * Added `network-uri` flag, to deal with split of `network-uri` from
    `network`.

  * Fix build dependencies for the `trypandoc` flag, so that they are
    ignored if `trypandoc` flag is set to False (Gabor Pali).

  * Updated README to remove outdated claim that `--self-contained`
    looks in the user data directory for missing files.

## pandoc 1.13.0.1 (2014-08-17)

  * Docx writer:

    + Fixed regression which bungled list numbering (#1544), causing
      all lists to appear as basic ordered lists.
    + Include row width in table rows (Christoffer Ackelman, Viktor Kronvall).
      Added a property to all table rows where the sum of column widths
      is specified in pct (fraction of 5000).  This helps persuade Word
      to lay out the table with the widths we specify.

  * Fixed a bug in Windows 8 which caused pandoc not to find the
    `pandoc-citeproc` filter (#1542).

  * Docx reader: miscellaneous under-the-hood improvements (Jesse Rosenthal).
    Most significantly, the reader now uses Builder, leading to some
    performance improvements.

  * HTML reader:  Parse appropriately styled span as SmallCaps.

  * Markdown writer: don't escape `$`, `^`, `~` when `tex_math_dollars`,
    `superscript`, and `subscript` extensions, respectively, are
    deactivated (#1127).

  * Added `trypandoc` flag to build CGI executable used in the online
    demo.

  * Makefile:  Added 'quick', 'osxpkg' targets.

  * Updated README in templates to indicate templates license.
    The templates are dual-licensed, BSD3 and GPL2+.

## pandoc 1.13 (15 August 2014)

### New features

  * Added `docx` as an input format (Jesse Rosenthal).  The docx
    reader includes conversion of native Word equations to pandoc
    LaTeX `Math` elements.  Metadata is taken from paragraphs at the
    beginning of the document with styles `Author`, `Title`, `Subtitle`,
    `Date`, and `Abstract`.

  * Added `epub` as an input format (Matthew Pickering).  The epub
    reader includes conversion of MathML to pandoc LaTeX `Math`
    elements.

  * Added `t2t` (Txt2Tags) as an input format (Matthew Pickering).
    Txt2tags is a lightweight markup format described at
    <http://txt2tags.org/>.

  * Added `dokuwiki` as an output format (Clare Macrae).

  * Added `haddock` as an output format.

  * Added `--extract-media` option to extract media contained in a zip
    container (docx or epub) while adjusting image paths to point to the
    extracted images.

  * Added a new markdown extension, `compact_definition_lists`, that
    restores the syntax for definition lists of pandoc 1.12.x, allowing
    tight definition lists with no blank space between items, and
    disallowing lazy wrapping.  (See below under behavior changes.)

  * Added an extension `epub_html_exts` for parsing HTML in EPUBs.

  * Added extensions `native_spans` and `native_divs` to activate
    parsing of material in HTML span or div tags as Pandoc Span
    inlines or Div blocks.

  * `--trace` now works with the Markdown, HTML, Haddock, EPUB,
    Textile, and MediaWiki readers.  This is an option intended
    for debugging parsing problems; ordinary users should not need
    to use it.

### Behavior changes

  * Changed behavior of the `markdown_attribute` extension, to bring
    it in line with PHP markdown extra and multimarkdown.  Setting
    `markdown="1"` on an outer tag affects all contained tags,
    recursively, until it is reversed with `markdown="0"` (#1378).

  * Revised markdown definition list syntax (#1429).  Both the reader
    and writer are affected.  This change brings pandoc's definition list
    syntax into alignment with that used in PHP markdown extra and
    multimarkdown (with the exception that pandoc is more flexible about
    the definition markers, allowing tildes as well as colons).  Lazily
    wrapped definitions are now allowed.  Blank space is required
    between list items.  The space before a definition is used to determine
    whether it is a paragraph or a "plain" element.  **WARNING: This change
    may break existing documents!**  Either check your documents for
    definition lists without blank space between items, or use
    `markdown+compact_definition_lists` for the old behavior.

  * `.numberLines` now works in fenced code blocks even if no language
    is given (#1287, jgm/highlighting-kate#40).

  * Improvements to `--filter`:

    + Don't search PATH for a filter with an explicit path.
      This fixed a bug wherein `--filter ./caps.py` would run `caps.py` from
      the system path, even if there was a `caps.py` in the working directory.
    + Respect shebang if filter is executable (#1389).
    + Don't print misleading error message.
      Previously pandoc would say that a filter was not found,
      even in a case where the filter had a syntax error.

  * HTML reader:

    + Parse `div` and `span` elements even without `--parse-raw`,
      provided `native_divs` and `native_spans` extensions are set.
      Motivation:  these now generate native pandoc Div and Span
      elements, not raw HTML.
    + Parse EPUB-specific elements if the `epub_html_exts`
      extension is enabled.  These include `switch`, `footnote`,
      `rearnote`, `noteref`.

  * Org reader:

    + Support for inline LaTeX.  Inline LaTeX is now accepted and parsed by the
      org-mode reader.  Both math symbols (like `\tau`) and LaTeX commands (like
      `\cite{Coffee}`), can be used without any further escaping (Albert
      Krewinkel).

  * Textile reader and writer:

    + The `raw_tex` extension is no longer set by default.  You can
      enable it with `textile+raw_tex`.

  * DocBook reader:

    + Support `equation`, `informalequation`, `inlineequation` elements with
      `mml:math` content.  This is converted into LaTeX and put into a Pandoc
      Math inline.

  * Revised `plain` output, largely following the style of Project
    Gutenberg:

    + Emphasis is rendered with `_underscores_`, strong emphasis
      with ALL CAPS.
    + Headings are rendered differently, with space to set them off,
      not with setext style underlines. Level 1 headers are ALL CAPS.
    + Math is rendered using unicode when possible, but without the
      distracting emphasis markers around variables.
    + Footnotes use a regular `[n]` style.

  * Markdown writer:

    + Horizontal rules are now a line across the whole page.
    + Prettier pipe tables.  Columns are now aligned  (#1323).
    + Respect the `raw_html` extension.  `pandoc -t markdown-raw_html`
      no longer emits any raw HTML, including span and div tags
      generated by Span and Div elements.
    + Use span with style for `SmallCaps` (#1360).

  * HTML writer:

    + Autolinks now have class `uri`, and email autolinks have class
      `email`, so they can be styled.

  * Docx writer:

    + Document formatting is carried over from `reference.docx`.
      This includes margins, page size, page orientation, header,
      and footer, including images in headers and footers.
    + Include abstract (if present) with `Abstract` style (#1451).
    + Include subtitle (if present) with `Subtitle` style, rather
      than tacking it on to the title (#1451).

  * Org writer:

    + Write empty span elements with an id attribute as org anchors.
      For example `Span ("uid",[],[]) []` becomes `<<uid>>`.

  * LaTeX writer:

    + Put table captions above tables, to match the conventional
      standard.  (Previously they appeared below tables.)
    + Use `\(..\)` instead of `$..$` for inline math (#1464).
    + Use `\nolinkurl` in email autolinks.  This allows them to be styled
      using `\urlstyle{tt}`.  Thanks to Ulrike Fischer for the solution.
    + Use `\textquotesingle` for `'` in inline code.  Otherwise we get
      curly quotes in the PDF output (#1364).
    + Use `\footnote<.>{..}` for notes in beamer, so that footnotes
      do not appear before the overlays in which their markers appear
      (#1525).
    + Don't produce a `\label{..}` for a Div or Span element.  Do produce
      a `\hyperdef{..}` (#1519).

  * EPUB writer:

    + If the metadata includes `page-progression-direction` (which can be
      `ltr` or `rtl`, the `page-progression-direction` attribute will
      be set in the EPUB spine (#1455).

  * Custom lua writers:

    + Custom writers now work with `--template`.
    + Removed HTML header scaffolding from `sample.lua`.
    + Made citation information available in lua writers.

  * `--normalize` and `Text.Pandoc.Shared.normalize` now consolidate
    adjacent `RawBlock`s when possible.

### API changes

  * Added `Text.Pandoc.Readers.Docx`, exporting `readDocx` (Jesse Rosenthal).

  * Added `Text.Pandoc.Readers.EPUB`, exporting `readEPUB` (Matthew
    Pickering).

  * Added `Text.Pandoc.Readers.Txt2Tags`, exporting `readTxt2Tags` (Matthew
    Pickering).

  * Added `Text.Pandoc.Writers.DokuWiki`, exporting `writeDokuWiki`
    (Clare Macrae).

  * Added `Text.Pandoc.Writers.Haddock`, exporting `writeHaddock`.

  * Added `Text.Pandoc.MediaBag`, exporting `MediaBag`, `lookupMedia`,
    `insertMedia`, `mediaDirectory`, `extractMediaBag`.  The docx and epub
    readers return a pair of a `Pandoc` document and a `MediaBag` with
    the media resources they contain.  This can be extracted using
    `--extract-media`.  Writers that incorporate media (PDF, Docx,
    ODT, EPUB, RTF, or HTML formats with `--self-contained`) will look
    for resources in the `MediaBag` generated by the reader, in addition to
    the file system or web.

  * `Text.Pandoc.Readers.TexMath`: Removed deprecated `readTeXMath`.
    Renamed `readTeXMath'` to `texMathToInlines`.

  * `Text.Pandoc`: Added `Reader` data type (Matthew Pickering).
    `readers` now associates names of readers with `Reader`
     structures.  This allows inclusion of readers, like the docx
     reader, that take binary rather than textual input.

  * `Text.Pandoc.Shared`:

    + Added `capitalize` (Artyom Kazak), and replaced uses of
      `map toUpper` (which give bad results for many languages).
    + Added `collapseFilePath`, which removes intermediate `.` and
      `..` from a path (Matthew Pickering).
    + Added `fetchItem'`, which works like `fetchItem` but searches
      a `MediaBag` before looking on the net or file system.
    + Added `withTempDir`.
    + Added `removeFormatting`.
    + Added `extractSpaces` (from HTML reader) and generalized its type
      so that it can be used by the docx reader (Matthew Pickering).
    + Added `ordNub`.
    + Added `normalizeInlines`, `normalizeBlocks`.
    + `normalize` is now `Pandoc -> Pandoc` instead of
      `Data a :: a -> a`.  Some users may need to change their uses of
      `normalize` to the newly exported `normalizeInlines` or
      `normalizeBlocks`.

  * `Text.Pandoc.Options`:

    + Added `writerMediaBag` to `WriterOptions`.
    + Removed deprecated and no longer used `readerStrict` in
      `ReaderOptions`.  This is handled by `readerExtensions` now.
    + Added `Ext_compact_definition_lists`.
    + Added `Ext_epub_html_exts`.
    + Added `Ext_native_divs` and `Ext_native_spans`.
      This allows users to turn off the default pandoc behavior of
      parsing contents of div and span tags in markdown and HTML
      as native pandoc Div blocks and Span inlines.

  * `Text.Pandoc.Parsing`:

    + Generalized `readWith` to `readWithM` (Matthew Pickering).
    + Export `runParserT` and `Stream` (Matthew Pickering).
    + Added `HasQuoteContext` type class (Matthew Pickering).
    + Generalized types of `mathInline`, `smartPunctuation`, `quoted`,
      `singleQuoted`, `doubleQuoted`, `failIfInQuoteContext`,
      `applyMacros` (Matthew Pickering).
    + Added custom `token` (Matthew Pickering).
    + Added `stateInHtmlBlock` to `ParserState`.  This is used to keep
      track of the ending tag we're waiting for when we're parsing inside
      HTML block tags.
    + Added `stateMarkdownAttribute` to `ParserState`. This is used
      to keep track of whether the markdown attribute has been set in
      an enclosing tag.
    + Generalized type of `registerHeader`, using new type classes
      `HasReaderOptions`, `HasIdentifierList`, `HasHeaderMap` (Matthew
      Pickering).  These allow certain common functions to be reused
      even in parsers that use custom state (instead of `ParserState`),
      such as the MediaWiki reader.
    + Moved `inlineMath`, `displayMath` from Markdown reader to Parsing,
      and generalized their types (Matthew Pickering).

  * `Text.Pandoc.Pretty`:

    + Added `nestle`.
    + Added `blanklines`, which guarantees a certain number of blank lines
      (and no more).

### Bug fixes

  * Markdown reader:

    + Fixed parsing of indented code in list items.  Indented code
      at the beginning of a list item must be indented eight spaces
      from the margin (or edge of the container), or four spaces
      from the list marker, whichever is greater.
    + Fixed small bug in HTML parsing with `markdown_attribute`, which
      caused incorrect tag nesting for input like
      `<aside markdown="1">*hi*</aside>`.
    + Fixed regression with intraword underscores (#1121).
    + Improved parsing of inline links containing quote characters (#1534).
    + Slight rewrite of `enclosure`/`emphOrStrong` code.
    + Revamped raw HTML block parsing in markdown (#1330).
      We no longer include trailing spaces and newlines in the
      raw blocks.  We look for closing tags for elements (but without
      backtracking).  Each block-level tag is its own `RawBlock`;
      we no longer try to consolidate them (though `--normalize` will do so).
    + Combine consecutive latex environments.  This helps when you have
      two minipages which can't have blank lines between them (#690, #1196).
    + Support smallcaps through span.
      `<span style="font-variant:small-caps;">foo</span>` will be
      parsed as a `SmallCaps` inline, and will work in all output
      formats that support small caps (#1360).
    + Prevent spurious line breaks after list items (#1137).  When the
      `hard_line_breaks` option was specified, pandoc would formerly
      produce a spurious line break after a tight list item.
    + Fixed table parsing bug (#1333).
    + Handle `c++` and `objective-c` as language identifiers in
      github-style fenced blocks (#1318).
    + Inline math must have nonspace before final `$` (#1313).

  * LaTeX reader:

    + Handle comments at the end of tables.  This resolves the issue
      illustrated in <http://stackoverflow.com/questions/24009489>.
    + Correctly handle table rows with too few cells.  LaTeX seems to
      treat them as if they have empty cells at the end  (#241).
    + Handle leading/trailing spaces in `\emph` better.
      `\emph{ hi }` gets parsed as `[Space, Emph [Str "hi"], Space]`
      so that we don't get things like `* hi *` in markdown output.
      Also applies to `textbf` and some other constructions (#1146).
    + Don't assume preamble doesn't contain environments (#1338).
    + Allow (and discard) optional argument for `\caption` (James Aspnes).

  * HTML reader:

    + Fixed major parsing problem with HTML tables.  Table cells were
      being combined into one cell (#1341).
    + Fixed performance issue with malformed HTML tables.
      We let a `</table>` tag close an open `<tr>` or `<td>` (#1167).
    + Allow space between `<col>` and `</col>`.
    + Added `audio` and `source` in `eitherBlockOrInline`.
    + Moved `video`, `svg`, `progress`, `script`, `noscript`, `svg` from
      `blockTags` to `eitherBlockOrInline`.
    + `map` and `object` were mistakenly in both lists; they have been removed
      from `blockTags`.
    + Ignore `DOCTYPE` and `xml` declarations.

  * MediaWiki reader:

    + Don't parse backslash escapes inside `<source>` (#1445).
    + Tightened up template parsing.
      The opening `{{` must be followed by an alphanumeric or `:`.
      This prevents the exponential slowdown in #1033.
    + Support "Bild" for images.

  * DocBook reader:

    + Better handle elements inside code environments.  Pandoc's document
      model does not allow structure inside code blocks, but at least this way
      we preserve the text (#1449).
    + Support `<?asciidoc-br?>` (#1236).

  * Textile reader:

    + Fixed list parsing. Lists can now start without an intervening
      blank line (#1513).
    + HTML block-level tags that do not start a line are parsed as
      inline HTML and do not interrupt paragraphs (as in RedCloth).

  * Org reader:

    + Make tildes create inline code (#1345).  Also relabeled `code` and
      `verbatim` parsers to accord with the org-mode manual.
    + Respect `:exports` header argument in code blocks (Craig Bosma).
    + Fixed tight lists with sublists (#1437).

  * EPUB writer:

    + Avoid excess whitespace in `nav.xhtml`.  This should improve
      TOC view in iBooks (#1392).
    + Fixed regression on cover image.
      In 1.12.4 and 1.12.4.2, the cover image would not appear properly,
      because the metadata id was not correct.  Now we derive the id from the
      actual cover image filename, which we preserve rather than using
      "cover-image."
    + Keep newlines between block elements.  This allows
      easier diff-ability (#1424).
    + Use `stringify` instead of custom `plainify`.
    + Use `renderTags'` for all tag rendering.  This properly handles tags
      that should be self-closing.  Previously `<hr/>` would appear in EPUB
      output as `<hr></hr>` (#1420).
    + Better handle HTML media tags.
    + Handle multiple dates with OPF `event` attributes.  Note: in EPUB3 we
      can have only one dc:date, so only the first one is used.

  * LaTeX writer:

    + Correctly handle figures in notes.  Notes can't contain figures in
      LaTeX, so we fake it to avoid an error  (#1053).
    + Fixed strikeout + highlighted code (#1294).
      Previously strikeout highlighted code caused an error.

  * ConTeXt writer:

    + Improved detection of autolinks with URLs containing escapes.

  * RTF writer:

    + Improved image embedding: `fetchItem'` is now used to get the
      images, and calculated image sizes are indicated in the RTF.
    + Avoid extra paragraph tags in metadata (#1421).

  * HTML writer:

    + Deactivate "incremental" inside slide speaker notes (#1394).
    + Don't include empty items in the table of contents for
      slide shows.  (These would result from creating a slide
      using a horizontal rule.)

  * MediaWiki writer:

    + Minor renaming of `st` prefixed names.

  * AsciiDoc writer:

    + Double up emphasis and strong emphasis markers in intraword
      contexts, as required by asciidoc (#1441).

  * Markdown writer:

    + Avoid wrapping that might start a list, blockquote, or header (#1013).
    + Use Span instead of (hackish) `SmallCaps` in `plainify`.
    + Don't use braced attributes for fenced code (#1416).
      If `Ext_fenced_code_attributes` is not set, the first class
      attribute will be printed after the opening fence as a bare word.
    + Separate adjacent lists of the same kind with an HTML comment (#1458).

  * PDF writer:

    + Fixed treatment of data uris for images (#1062).

  * Docx writer:

    + Use Compact style for empty table cells (#1353).
      Otherwise we get overly tall lines when there are empty
      table cells and the other cells are compact.
    + Create overrides per-image for `media/` in reference docx.
      This should be somewhat more robust and cover more types of images.
    + Improved `entryFromArchive` to avoid an unneeded parse.
    + Section numbering carries over from reference.docx (#1305).
    + Simplified `abstractNumId` numbering.  Instead of sequential numbering,
      we assign numbers based on the list marker styles.

  * `Text.Pandoc.Options`:

    + Removed `Ext_fenced_code_attributes` from `markdown_github`
      extensions.

  * `Text.Pandoc.ImageSize`:

    + Use default instead of failing if image size not found
      in exif header (#1358).
    + ignore unknown exif header tag rather than crashing.
      Some images seem to have tag type of 256, which was causing
      a runtime error.

  * `Text.Pandoc.Shared`:

    + `fetchItem`:  unescape URI encoding before reading local file (#1427).
    + `fetchItem`:  strip a fragment like `?#iefix` from the extension before
      doing mime lookup, to improve mime type guessing.
    + Improved logic of `fetchItem`:  absolute URIs are fetched from the net;
      other things are treated as relative URIs if `sourceURL` is `Just _`,
      otherwise as file paths on the local file system.
    + `fetchItem` now properly handles links without a protocol (#1477).
    + `fetchItem` now escapes characters not allowed in URIs before trying
      to parse the URIs.
    + Fixed runtime error with `compactify'DL` on certain lists (#1452).

  * `pandoc.hs`: Don't strip path off of `writerSourceURL`: the path is
    needed to resolve relative URLs when we fetch resources (#750).

  * `Text.Pandoc.Parsing`

    + Simplified `dash` and `ellipsis` (#1419).
    + Removed `(>>~)` in favor of the equivalent `(<*)` (Matthew Pickering).
    + Generalized functions to use `ParsecT` (Matthew Pickering).
    + Added `isbn` and `pmid` to list of recognized schemes (Matthew
      Pickering).

### Template changes

  * Added haddock template.
  * EPUB3:  Added `type` attribute to `link` tags.  They are supposed to
    be "advisory" in HTML5, but kindlegen seems to require them.
  * EPUB3:  Put title page in section with `epub:type="titlepage"`.
  * LaTeX: Made `\subtitle` work properly (#1327).
  * LaTeX/Beamer: remove conditional around date (#1321).
  * LaTeX:  Added `lot` and `lof` variables, which can be set to
    get `\listoftables` and `\listoffigures` (#1407).  Note that
    these variables can be set at the command line with `-Vlot -Vlof`
    or in YAML metadata.

### Under the hood improvements

  * Rewrote normalize for efficiency (#1385).

  * Rewrote Haddock reader to use `haddock-library` (#1346).

    + This brings pandoc's rendering of haddock markup in line
      with the new haddock.
    + Fixed line breaks in `@` code blocks.
    + alex and happy are no longer build-depends.

  * Added `Text.Pandoc.Compat.Directory` to allow building against
    different versions of the `directory` library.

  + Added `Text.Pandoc.Compat.Except` to allow building against
    different versions of `mtl`.

  * Code cleanup in some writers, using Reader monad to avoid
    passing options parameter around (Matej Kollar).

  * Improved readability in `pandoc.hs`.

  * Miscellaneous code cleanups (Artyom Kazak).

  * Avoid `import Prelude hiding (catch)` (#1309, thanks to Michael
    Thompson).

  * Changed `http-conduit` flag to `https`.  Depend on `http-client`
    and `http-client-tls` instead of `http-conduit`.  (Note:  pandoc still
    depends on `conduit` via `yaml`.)

  * Require `highlighting-kate >= 0.5.8.5` (#1271, #1317, Debian #753299).
    This change to highlighting-kate means that PHP fragments no longer need
    to start with `<?php`.  It also fixes a serious bug causing failures with
    ocaml and fsharp.

  * Require latest `texmath`.  This fixes `\tilde{E}` and allows
    `\left` to be used with `]`, `)` etc. (#1319), among many other
    improvements.

  * Require latest `zip-archive`.  This has fixes for unicode path names.

  * Added tests for plain writer.

  * `Text.Pandoc.Templates`:

    + Fail informatively on template syntax errors.
      With the move from parsec to attoparsec, we lost good error
      reporting.  In fact, since we weren't testing for end of input,
      malformed templates would fail silently.  Here we revert back to
      Parsec for better error messages.
    + Use `ordNub` (#1022).

  * Benchmarks:

    + Made benchmarks compile again (Artyom Kazak).
    + Fixed so that the failure of one benchmark does not prevent others
      from running (Artyom Kazak).
    + Use `nfIO` instead of the `getLength` trick to force full evaluation.
    + Changed benchmark to use only the test suite, so that benchmarks
      run more quickly.

  * Windows build script:

    + Add `-windows` to file name.
    + Use one install command for pandoc, pandoc-citeproc.
    + Force install of pandoc-citeproc.

  * `make_osx_package`:  Call zip file `pandoc-VERSION-osx.zip`.
    The zip should not be named `SOMETHING.pkg.zip`, or OSX finder
    will extract it into a folder named `SOMETHING.pkg`, which it
    will interpret as a defective package (#1308).

  * `README`:

    + Made headers for all extensions so they have IDs and can be
      linked to (Beni Cherniavsky-Paskin).
    + Fixed typos (Phillip Alday).
    + Fixed documentation of attributes (#1315).
    + Clarified documentation on small caps (#1360).
    + Better documentation for `fenced_code_attributes` extension
      (Caleb McDaniel).
    + Documented fact that you can put YAML metadata in a separate file
      (#1412).


## pandoc 1.12.4.2 (2014-05-14)

  * Require highlighting-kate >= 0.5.8.  Fixes a performance regression.

  * Shared:  `addMetaValue` now behaves slightly differently:
    if both the new and old values are lists, it concatenates their
    contents to form a new list.

  * LaTeX reader:

    + Set `bibliography` in metadata from `\bibliography` or
      `\addbibresource` command.
    + Don't error on `%foo` with no trailing newline.

  * Org reader:

    + Support code block headers (`#+BEGIN_SRC ...`) (Albert Krewinkel).
    + Fix parsing of blank lines within blocks (Albert Krewinkel).
    + Support pandoc citation extension (Albert Krewinkel).  This can
      be turned off by specifying `org-citation` as the input format.

  * Markdown reader:

    + `citeKey` moved to `Text.Pandoc.Parsing` so it can be used by
      other readers (Albert Krewinkel).

  * `Text.Pandoc.Parsing`:

    + Added `citeKey` (see above).
    + Added `HasLastStrPosition` type class and `updateLastStrPos`
      and `notAfterString` functions.

  * Updated copyright notices (Albert Krewinkel).

  * Added default.icml to data files so it installs with the package.

  * OSX package:

    + The binary is now built with options to ensure that it can be
      used with OSX 10.6+.
    + Moved OSX package materials to osx directory.
    + Added OSX package uninstall script, included in the zip container
      (thanks to Daniel T. Staal).

## pandoc 1.12.4 (2014-05-07)

  * Made it possible to run filters that aren't executable
    (#1096). Pandoc first tries to find the executable (searching
    the path if path isn't given). If it fails, but the file
    exists and has a `.py`, `.pl`, `.rb`, `.hs`, or `.php`
    extension, pandoc runs the filter using the appropriate
    interpreter. This should make it easier to use filters on
    Windows, and make it more convenient for everyone.

  * Added Emacs org-mode reader (Albert Krewinkel).

  * Added InDesign ICML Writer (mb21).

  * MediaWiki reader:

    + Accept image links in more languages (Jaime Marquínez Ferrándiz).
    + Fixed bug in certain nested lists (#1213).  If a level 2 list was
      followed by a level 1 list, the first item of the level 1 list
      would be lost.
    + Handle table rows containing just an HTML comment (#1230).

  * LaTeX reader:

    + Give better location information on errors, pointing to line
      numbers within included files (#1274).
    + LaTeX reader:  Better handling of `table` environment (#1204).
      Positioning options no longer rendered verbatim.
    + Better handling of figure and table with caption (#1204).
    + Handle `@{}` and `p{length}` in tabular.  The length is not actually
      recorded, but at least we get a table (#1180).
    + Properly handle `\nocite`.  It now adds a `nocite` metadata
      field.  Citations there will appear in the bibliography but not
      in the text (unless you explicitly put a `$nocite$` variable
      in your template).

  * Markdown reader:

    + Ensure that whole numbers in YAML metadata are rendered without
      decimal points.  (This became necessary with changes to aeson
      and yaml libraries.  aeson >= 0.7 and yaml >= 0.8.8.2 are now required.)
    + Fixed regression on line breaks in strict mode (#1203).
    + Small efficiency improvements.
    + Improved parsing of nested `div`s.  Formerly a closing `div` tag
      would be missed if it came right after other block-level tags.
    + Avoid backtracking when closing `</div>` not found.
    + Fixed bug in reference link parsing in `markdown_mmd`.
    + Fixed a bug in list parsing (#1154).  When reading a raw list
      item, we now strip off up to 4 spaces.
    + Fixed parsing of empty reference link definitions (#1186).
    + Made one-column pipe tables work (#1218).

  * Textile reader:

    + Better support for attributes.  Instead of being ignored, attributes
      are now parsed and included in Span inlines.  The output will be a bit
      different from stock textile: e.g. for `*(foo)hi*`, we'll get
      `<em><span class="foo">hi</span></em>` instead of
      `<em class="foo">hi</em>`.  But at least the data is not lost.
    + Improved treatment of HTML spans (%) (#1115).
    + Improved link parsing.  In particular we now pick up on attributes.
      Since pandoc links can't have attributes, we enclose the whole link in
      a span if there are attributes (#1008).
    + Implemented correct parsing rules for inline markup (#1175, Matthew
      Pickering).
    + Use Builder (Matthew Pickering).

  * DocBook reader:

    + Better treatment of `formalpara`.  We now emit the title (if present)
      as a separate paragraph with boldface text (#1215).
    + Set metadata `author` not `authors`.
    + Added recognition of `authorgroup` and `releaseinfo` elements (#1214,
      Matthew Pickering).
    + Converted current meta information parsing in DocBook to a more
      extensible version which is aware of the more recent meta
      representation (Matthew Pickering).

  * HTML reader:

    + Require tagsoup 0.13.1, to fix a bug with parsing of script tags
      (#1248).
    + Treat processing instructions & declarations as block.  Previously
      these were treated as inline, and included in paragraph tags in HTML
      or DocBook output, which is generally not what is wanted (#1233).
    + Updated `closes` with rules from HTML5 spec.
    + Use Builder (Matthew Pickering, #1162).

  * RST reader:

    + Remove duplicate `http` in PEP links (Albert Krewinkel).
    + Make rst figures true figures (#1168, CasperVector)
    + Enhanced Pandoc's support for rST roles (Merijn Verstaaten).
      rST parser now supports: all built-in rST roles, new role definition,
      role inheritance, though with some limitations.
    + Use `author` rather than `authors` in metadata.
    + Better handling of directives.  We now correctly handle field
      lists that are indented more than three spaces.  We treat an
      `aafig` directive as a code block with attributes, so it can be
      processed in a filter (#1212).

  * LaTeX writer:

    + Mark span contents with label if span has an ID (Albert Krewinkel).
    + Made `--toc-depth` work well with books in latex/pdf output (#1210).
    + Handle line breaks in simple table cells (#1217).
    + Workaround for level 4-5 headers in quotes.  These previously produced
      invalid LaTeX: `\paragraph` or `\subparagraph` in a `quote` environment.
      This adds an `mbox{}` in these contexts to work around the problem.
      See <http://tex.stackexchange.com/a/169833/22451> (#1221).
    + Use `\/` to avoid en-dash ligature instead of `-{}-` (Vaclav Zeman).
      This is to fix LuaLaTeX output. The `-{}-` sequence does not avoid the
      ligature with LuaLaTeX but `\/` does.
    + Fixed string escaping in `hyperref` and `hyperdef` (#1130).

  * ConTeXt writer:  Improved autolinks (#1270).

  * DocBook writer:

    + Improve handling of hard line breaks in Docbook writer
      (Neil Mayhew).  Use a `<literallayout>` for the entire paragraph, not
      just for the newline character.
    + Don't let line breaks inside footnotes influence the enclosing
      paragraph (Neil Mayhew).
    + Distinguish tight and loose lists in DocBook output, using
      `spacing="compact"` (Neil Mayhew, #1250).

  * Docx writer:  When needed files are not present in the user's
    `reference.docx`, fall back on the versions in the `reference.docx`
    in pandoc's data files. This fixes a bug that occurs when a
    `reference.docx` saved by LibreOffice is used. (#1185)

  * EPUB writer:

    + Include extension in epub ids.  This fixes a problem with duplicate
      extensions for fonts and images with the same base name but different
      extensions (#1254).
    + Handle files linked in raw `img` tags (#1170).
    + Handle media in `audio` source tags (#1170).
      Note that we now use a `media` directory rather than `images`.
    + Incorporate files linked in `video` tags (#1170).  `src` and `poster`
      will both be incorporated into `content.opf` and the epub container.

  * HTML writer:

    + Add colgroup around col tags (#877).  Also affects EPUB writer.
    + Fixed bug with unnumbered section headings.  Unnumbered section
      headings (with class `unnumbered`) were getting numbers.
    + Improved detection of image links. Previously image links with
      queries were not recognized, causing `<embed>` to be used instead
      of `<img>`.

  * Man writer:  Ensure that terms in definition lists aren't line wrapped
    (#1195).

  * Markdown writer:

    + Use proper escapes to avoid unwanted lists (#980).  Previously we used
      0-width spaces, an ugly hack.
    + Use longer backtick fences if needed (#1206).  If the content contains a
      backtick fence and there are attributes, make sure longer fences are
      used to delimit the code.  Note:  This works well in pandoc, but github
      markdown is more limited, and will interpret the first string of three
      or more backticks as ending the code block.

  * RST writer:  Avoid stack overflow with certain tables (#1197).

  * RTF writer:  Fixed table cells containing paragraphs.

  * Custom writer:

    + Correctly handle UTF-8 in custom lua scripts (#1189).
    + Fix bugs with lua scripts with mixed-case filenames and
      paths containing `+` or `-` (#1267).  Note that `getWriter`
      in `Text.Pandoc` no longer returns a custom writer on input
      `foo.lua`.

  * AsciiDoc writer:  Handle multiblock and empty table cells
    (#1245, #1246).  Added tests.

  * `Text.Pandoc.Options`: Added `readerTrace` to `ReaderOptions`

  * `Text.Pandoc.Shared`:

    + Added `compactify'DL` (formerly in markdown reader) (Albert Krewinkel).
    + Fixed bug in `toRomanNumeral`:  numbers ending with '9' would
      be rendered as Roman numerals ending with 'IXIV' (#1249).  Thanks to
      Jesse Rosenthal.
    + `openURL`: set proxy with value of http_proxy env variable (#1211).
      Note:  proxies with non-root paths are not supported, due to
      limitations in `http-conduit`.

  * `Text.Pandoc.PDF`:

    + Ensure that temp directories deleted on Windows (#1192).  The PDF is
      now read as a strict bytestring, ensuring that process ownership will
      be terminated, so the temp directory can be deleted.
    + Use `/` as path separators in a few places, even on Windows.
      This seems to be necessary for texlive (#1151, thanks to Tim Lin).
    + Use `;` for `TEXINPUTS` separator on Windows (#1151).
    + Changes to error reporting, to handle non-UTF8 error output.

  * `Text.Pandoc.Templates`:

    + Removed unneeded datatype context (Merijn Verstraaten).

    + YAML objects resolve to "true" in conditionals (#1133).
      Note:  If `address` is a YAML object and you just have `$address$`
      in your template, the word `true` will appear, which may be
      unexpected.  (Previously nothing would appear.)

  * `Text.Pandoc.SelfContained`:  Handle `poster` attribute in `video`
    tags (#1188).

  * `Text.Pandoc.Parsing`:

    + Made `F` an instance of Applicative (#1138).
    + Added `stateCaption`.
    + Added `HasMacros`, simplified other typeclasses.
      Removed `updateHeaderMap`, `setHeaderMap`, `getHeaderMap`,
      `updateIdentifierList`, `setIdentifierList`, `getIdentifierList`.
    + Changed the smart punctuation parser to return `Inlines`
      rather than `Inline` (Matthew Pickering).
    + Changed `HasReaderOptions`, `HasHeaderMap`, `HasIdentifierList`
      from typeclasses of monads to typeclasses of states.  This simplifies
      the instance definitions and provides more flexibility.  Generalized
      type of `getOption` and added a default definition.  Removed
      `askReaderOption`.  Added `extractReaderOption`.  Added
      `extractHeaderMap` and `updateHeaderMap` in `HasHeaderMap`.
      Gave default definitions for `getHeaderMap`, `putHeaderMap`,
      `modifyHeaderMap`.  Added `extractIdentifierList` and
      `updateIdentifierList` in `HasIdentifierList`.  Gave defaults
      for `getIdentifierList`, `putIdentifierList`, and
      `modifyIdentifierList`.  The ultimate goal here is to allow different
      parsers to use their own, tailored parser states (instead of
      `ParserState`) while still using shared functions.

  * Template changes:

    + LaTeX template: Use `fontenc` package only with `pdflatex` (#1164).
    + LaTeX template:  Add `linestretch` and `fontfamily` variables.
    + LaTeX template:  Conditionalize author and date commands.
    + Beamer template: Consistent styles for figure and table captions
      (aaronwolen).
    + LaTeX and beamer template:  Adjust widths correctly for oversized
      images.  Use `\setkeys{Gin}{}` to set appropriate defaults for
      `\includegraphics` (Yihui Xie, Garrick Aden-Buie).  Load
      `upquote` only after `fontenc` (Yihui Xie).
    + Beamer template: Added caption package (#1200).
    + Beamer template:  changes for better unicode handling (KarolS).
    + DocBook template:  use `authorgroup` if there are authors.
    + revealjs template: Move `include-after` to end (certainlyakey).
    + revealjs template: Fixed PDF print function (#1220, kevinkenan).

  * Bumped version bounds of dependencies.

  * Added a `--trace` command line option, for debugging backtracking
    bugs.  So far this only works with the markdown reader.

  * MathMLinHTML:  Fixed deprecation warning (#362, gwern, Albert Krewinkel).

  * Updated travis script to test with multiple GHC versions.

  * Force failure of a Travis build if GHC produces warnings (Albert
    Krewinkel).

  * Add `.editorconfig` (Albert Krewinkel).
    See <http://editorconfig.org/> for details.

  * Give more useful error message if '-t pdf' is specified (#1155).

  * Added `Cite`, `SmallCaps` to `Arbitrary` instance (#1269).

  * Allow `html4` as a synonym of `html` as a reader (it already works
    as a writer).

  * README:

    + Added an explanation of how to use YAML metadata to
      force items to appear in the bibliography without citations in
      the text (like LaTeX `\nocite`).
    + Added note to `--bibtex/--natbib`: not for use in making PDF
      (#1194, thanks to nahoj).
    + Added explanatory notes about `--natbib` and `--biblatex`.
    + Added specification of legal syntax for citation keys.
    + Fixed variable defaults documentation (Albert Krewinkel).

  * Removed copyright statements for files that have been removed
    (Albert Krewinkel).

  * Moved some doc files from `data-files` to `extra-source-files` (#1123).
    They aren't needed at runtime.  We keep README and COPYRIGHT in data
    to ensure that they'll be available on all systems on which pandoc
    is installed.

  * Use cabal sandboxes in Windows build script.

## pandoc 1.12.3.3 (2014-02-03)

  * To changes to source; recompiled tarball with latest alex and
    happy, so they will work with GHC 7.8.

## pandoc 1.12.3.2 (2014-02-03)

  * Bumped version bounds for blaze-html, blaze-markup.

  * ImageSize:  Avoid use of lookAhead, which is not in binary >= 0.6
    (#1124).

  * Fixed mediawiki ordered list parsing (#1122).

  * HTML reader:  Fixed bug reading inline math with `$$` (#225).

  * Added support for LaTeX style literate Haskell code blocks in rST
    (Merijn Verstraaten).

## pandoc 1.12.3.1 (2014-01-14)

  * Relaxed version constraint on binary, allowing the use of binary 0.5.


## pandoc 1.12.3 (2014-01-10)

  * The `--bibliography` option now sets the `biblio-files` variable.
    So, if you're using `--natbib` or `--biblatex`, you can just use
    `--bibliography=foo.bib` instead of `-V bibliofiles=foo`.

  * Don't run pandoc-citeproc filter if `--bibliography` is
    used together with `--natbib` or `--biblatex` (Florian Eitel).

  * Template changes:

    + Updated beamer template to include booktabs.
    + Added `abstract` variable to LaTeX template.
    + Put `header-includes` after `title` in LaTeX template (#908).
    + Allow use of `\includegraphics[size]` in beamer.
      This just required porting a macro definition from the default
      LaTeX template to the default beamer template.

  * `reference.docx`:  Include `FootnoteText` style.
    Otherwise Word ignores the style, even when specified in the `pPr`.
    (#901)

  * `reference.odt`:  Tidied `styles.xml`.

  * Relaxed version bounds for dependencies.

  * Added `withSocketsDo` around http conduit code in `openURL`,
    so it works on Windows (#1080).

  * Added `Cite` function to `sample.lua`.

  * Markdown reader:

    + Fixed regression in title blocks (#1089).
      If author field was empty, date was being ignored.
    + Allow backslash-newline hard line breaks in grid and
      multiline table cells.
    + Citation keys may now start with underscores, and may contain
      underscores adjacent to internal punctuation.

  * LaTeX reader:

    + Add support for `Verb` macro (jrnold) (#1090).
    + Support babel-style quoting: `` "`..."' ``.

  * Properly handle script blocks in strict mode.  (That is,
    `markdown-markdown_in_html_blocks`.) Previously a spurious
    `<p>` tag was being added (#1093).

  * Docbook reader: Avoid failure if `tbody` contains no `tr` or `row`
    elements.

  * LaTeX writer:

    + Factored out function for table cell creation.
    + Better treatment of footnotes in tables.
      Notes now appear in the regular sequence, rather than in the
      table cell.  (This was a regression in 1.10.)

  * HTML reader: Parse name/content pairs from meta tags as metadata.
    Closes #1106.

  * Moved `fixDisplayMath` from Docx writer to `Writer.Shared`.

  * OpenDocument writer:  Fixed `RawInline`, `RawBlock` so they don't escape.

  * ODT writer:  Use mathml for proper rendering of formulas.
    Note:  LibreOffice's support for this seems a bit buggy.  But
    it should be better than what we had before.

  * RST writer: Ensure no blank line after def in definition list (#992).

  * Markdown writer: Don't use tilde code blocks with braced attributes in
    `markdown_github` output.  A consequence of this change is that the
    backtick form will be preferred in general if both are enabled.  That
    is good, as it is much more widespread than the tilde form.  (#1084)

  * Docx writer:  Fixed problem with some modified reference docx files.
    Include `word/_rels/settings.xml.rels` if it exists, as well as other
    `rels` files besides the ones pandoc generates explicitly.

  * HTML writer:

    + With `--toc`, headers no longer link to themselves (#1081).
    + Omit footnotes from TOC entries.  Otherwise we get doubled
      footnotes when headers have notes!

  * EPUB writer:

    + Avoid duplicate notes when headings contain notes.
      This arose because the headings are copied into the metadata
      "title" field, and the note gets rendered twice.  We strip the
      note now before putting the heading in "title".
    + Strip out footnotes from toc entries.
    + Fixed bug with `--epub-stylesheet`.  Now the contents of
      `writerEpubStylesheet` (set by `--epub-stylesheet`)
      should again work, and take precedence over a stylesheet specified
      in the metadata.

  * `Text.Pandoc.Pretty`:  Added `nestle`.  API change.

  * `Text.Pandoc.MIME`: Added `wmf`, `emf`.

  * `Text.Pandoc.Shared`:  `fetchItem` now handles image URLs beginning
    with `//`.

  * `Text.Pandoc.ImageSize`:  Parse EXIF format JPEGs.  Previously
    we could only get size information for JFIF format, which led
    to squished images in Word documents. Closes #976.

  * Removed old `MarkdownTest_1.0.3` directory (#1104).


## pandoc 1.12.2.1 (2013-12-08)

  * Markdown reader:  Fixed regression in list parser, involving
    continuation lines containing raw HTML (or even verbatim raw HTML).

## pandoc 1.12.2 (2013-12-07)

  * Metadata may now be included in YAML blocks in a markdown document.
    For example,

        ---
        title:
        - type: main
          text: My Book
        - type: subtitle
          text: An investigation of metadata
        creator:
        - role: author
          text: John Smith
        - role: editor
          text: Sarah Jones
        identifier:
        - scheme: DOI
          text: doi:10.234234.234/33
        publisher:  My Press
        rights:  (c) 2007 John Smith, CC BY-NC
        cover-image: img/mypic.jpg
        stylesheet: style.css
        ...

    Metadata may still be provided using `--epub-metadata`; it will
    be merged with the metadata in YAML blocks.

  * EPUB writer:

    + `meta` tags are now used instead of `opf` attributes for EPUB3.
    + Insert "svg" property as needed in opf (EPUB 3).
    + Simplify `imageTypeOf` using `getMimeType`.
    + Add properties attribute to `cover-image` item for EPUB 3.
    + Don't include node for `cover.xhtml` if no cover!
    + Ensure that same identifier is used throughout (#1044).
      If an identifier is given in metadata, we use that; otherwise
      we generate a random uuid.
    + Add cover reference to guide element (EPUB 2) (Shaun Attfield).
      Fixes an issue with Calibre putting the cover at the end of the book
      if the spine has `linear="no"`.  Apparently this is best practice
      for other converters as well:
      <http://www.idpf.org/epub/20/spec/OPF_2.0.1_draft.htm#Section2.6>.
    + Allow `stylesheet` in metadata.  The value is a path to the stylesheet.
    + Allow partial dates:  `YYYY`, `YYYY-MM`.

  * Markdown writer:  Fix rendering of tight sublists (#1050).
    Previously a spurious blank line was included after a tight sublist.

  * ODT writer:  Add `draw:name` attribute to `draw:frame` elements (#1069).
    This is reported to be necessary to avoid an error from recent
    versions of Libre Office when files contain more than one image
    Thanks to wmanley for reporting and diagnosing the problem.

  * ConTeXt writer:  Don't hardcode figure/table placement and numbering.
    Instead, let this be set in the template, using `\setupfloat`.
    Thanks to on4aa and Aditya Mahajan for the suggestion (#1067).

  * Implemented CSL flipflopping spans in DOCX, LaTeX, and HTML writers.

  * Fixed bug with markdown intraword emphasis.  Closes #1066.

  * Docbook writer:  Hierarchicalize block content in metadata.
    Previously headers just disappeared from block-level metadata
    when it was used in templates.  Now we apply the 'hierarchicalize'
    transformation.  Note that a block headed by a level-2 header will
    turn into a `<sect1>` element.

  * OpenDocument writer:  Skip raw HTML (#1035).
    Previously it was erroneously included as verbatim text.

  * HTML/EPUB writer, footnotes:  Put `<sup>` tag inside `<a>` tags.
    This allows better control of formatting, since the `<a>`
    tags have a distinguishing class (#1049).

  * Docx writer:

    + Use mime type info returned by fetchItem.
    + Fixed core metadata (#1046).
      Don't create empty date nodes if no date given.
      Don't create multiple `dc:creator` nodes; instead separate by
      semicolons.
    + Fix URL for core-properties in `_rels/.rels` (#1046).

  * Plain writer: don't print `<span>` tags.

  * LaTeX writer:

    + Fix definition lists with internal links in terms (#1032).
      This fix puts braces around a term that contains an internal
      link, to avoid problems with square brackets.
    + Properly escape pdftitle, pdfauthor (#1059).
    + Use booktabs package for tables (thanks to Jose Luis Duran).

  * Updated beamer template.  Now references should work properly
    (in a slide) when `--biblatex` or `--natbib` is used.

  * LaTeX reader:

    + Parse contents of curly quotes or matched `"` as quotes.
    + Support `\textnormal` as span with class `nodecor`.
      This is needed for pandoc-citeproc.
    + Improved citation parsing.  This fixes a run-time error that occurred
      with `\citet{}` (empty list of keys).  It also ensures that empty keys
      don't get produced.

  * MediaWiki reader:  Add automatic header identifiers.

  * HTML reader:

    + Use pandoc `Div` and `Span` for raw `<div>`, `<span>` when
      `--parse-raw`.
    + Recognize `svg` tags as block level content (thanks to MinRK).
    + Parse LaTeX math if appropriate options are set.

  * Markdown reader:

    + Yaml block must start immediately after `---`.  If there's a blank
      line after `---`, we interpreted it as a horizontal rule.
    + Correctly handle empty bullet list items.
    + Stop parsing "list lines" when we hit a block tag.
      This fixes exponential slowdown in certain input, e.g.
      a series of lists followed by `</div>`.

   * Slides:  Preserve `<div class="references">` in references slide.

  * `Text.Pandoc.Writer.Shared`:

    + Fixed bug in `tagWithAttrs`.  A space was omitted before key-value
      attributes, leading to invalid HTML.
    + `normalizeDate`: Allow dates with year only (thanks to Shaun Attfield).
    + Fixed bug in `openURL` with `data:` URIs.  Previously the base-64
      encoded bytestring was returned.  We now decode it so it's a proper
      image!

  * DocBook reader:  Handle numerical attributes starting with decimal.
    Also use `safeRead` instead of `read`.

  * `Text.Pandoc.Parsing`:

    + Generalized type of `registerHeader`, using new type classes
      `HasReadeOptions`, `HasIdentifierList`, `HasHeaderMap`.
      These allow certain common functions to be reused
      even in parsers that use custom state (instead of `ParserState`),
      such as the MediaWiki reader.
    + Moved inlineMath, displayMath from Markdown reader to Parsing.
      Generalize their types and export them from Parsing.  (API change.)

  * `Text.Pandoc.Readers.TexMath`: Export `readTeXMath'`, which attends
    to display/inline.  Deprecate `readTeXMath`, and use `readTeXMath'`
    in all the writers.  Require `texmath >= 0.6.5.2`.

  * `Text.Pandoc.MIME`:

    + Add entry for `jfif`.
    + In looking up extensions, drop the encoding info.
      E.g. for 'image/jpg;base64' we should lookup 'image/jpg'.

  * Templates:  Changed how array variables are resolved.  Previously if
    `foo` is an array (which might be because multiple values were set on
    the command line), `$foo$` would resolve to the concatenation of the
    elements of foo.  This is rarely useful behavior.  It has been changed
    so that the first value is rendered.  Of course, you can still iterate
    over the values using `$for(foo)$`.  This has the result that you can
    override earlier settings using `-V` by putting new values later on the
    command line, which is useful for many purposes.

  * `Text.Pandoc`:  Don't default to `pandocExtensions` for all writers.

  * Allow "epub2" as synonym for "epub", "html4" for "html".

  * Don't look for slidy files in data files with `--self-contained`.

  * Allow `https:` command line arguments to be downloaded.

  * Fixed `make_osx_package.sh` so data files embedded in `pandoc-citeproc`.

## pandoc 1.12.1 (2013-10-20)

  * `Text.Pandoc.Definition`:  Changed default JSON serialization format.
    Instead of `{"Str": "foo"}`, for example, we now have `{"t": "Str",
    "c": "foo"}`.  This new format is easier to work with outside of Haskell.
    Incidentally, "t" stands for "tag", "c" for "contents".

  * MediaWiki reader: Trim contents of `<math>` tags, to avoid problems
    when converting to markdown (#1027).

  * LaTeX reader:

    + Ensure that preamble doesn't contribute to the text of
      the document.
    + Fixed character escaping in `\url{}`.  Previously `\~` wasn't handled
      properly, among others.
    + Parse `{groups}` as `Span`.  This is needed for accurate conversion of
      bibtex titles, since we need to know what was protected from
      titlecase conversions.

  * LaTeX writer:

    + Specially escape non-ascii characters in labels.
      Otherwise we can get compile errors and other bugs when
      compiled with pdflatex (#1007).  Thanks to begemotv2718 for the fix.
    + Add link anchors for code blocks with identifiers (#1025).

  * Throughout the code, use `isURI` instead of `isAbsoluteURI`.
    It allows fragments identifiers.

  * Slide formats:

    + A Div element with class "notes" is treated as speaker
      notes.  Currently beamer goes to `\note{}`, revealjs to
      `<aside class="notes">`, and the notes are simply suppressed in
      other formats  (#925).
    + Fixed `. . .` (pause) on HTML slide formats.  Closes #1029.
      The old version caused a pause to be inserted before the first
      material on a slide. This has been fixed.
    + Removed data files for s5, slideous, slidy.
      Users of s5 and slideous will have to download the needed
      files, as has been documented for some time in the README.
      By default, slidy code will be sought on the web, as before.

  * HTML writer: Insert command to typeset mathjax only in slideous output
    (#966, #1012).

  * RST writer:  Skip spaces after display math.  Otherwise we get indentation
    problems, and part of the next paragraph may be rendered as part of the
    math.

  * OpenDocument writer:  Fix formatting of strikeout code (#995),
    thanks to wilx.  don't use `font-face-decls` variable.

  * Fixed test suite so it works with cabal sandboxes.

## pandoc 1.12.0.2 (2013-09-20)

  * Removed an unused dependency (`stringable`) from pandoc.cabal.
    This will help packagers, but users should not need to upgrade.

## pandoc 1.12.0.1 (2013-09-20)

  * Allow `--metadata` to be repeated for the same key to form a list.
    This also has the effect that `--bibliography` can be repeated,
    as before.

  * Handle boolean values in `--metadata`.  Note that anything not parseable
    as a YAML boolean or string is treated as a literal string.
    You can get a string value with "yes", or any of the strings interpretable
    as booleans, by quoting it:

        -M boolvalue=yes -M stringvalue='"yes"'

  * LaTeX writer: Don't print references if `--natbib` or `--biblatex`
    option used.

  * DOCX writer: Add `settings.xml` to the zip container.  Fixes a bug
    in which docx files could not be read by some versions of Word
    and LibreOffice (#990).

  * Fixed a regression involving slide shows with bibliographies.
    The Div container around references messed up the procedure for carving
    a document into slides.  So we now remove the surrounding Div in
    `prepSlides`.

  * More informative error message when a filter is not found in path.

  * Depend on pandoc-types 1.12.1.  This provide `ToJSONFilter`
    instances for `Data a => a -> [a]` and `Data a => a -> IO [a]`.

  * Don't use unicode_collation in building OSX package:
    it adds something like 50MB of dependencies to the package.

  * Declare alex and happy as build-tools (#986).

## pandoc 1.12 (2013-09-15)

### New features

  * Much more flexible metadata, including arbitrary fields and structured
    values.  Metadata can be specified flexibly in pandoc markdown using
    YAML metadata blocks, which may occur anywhere in the document:

        ---
        title: Here is my title.
        abstract: |
          This is the abstract.

          1. It can contain
          2. block content
             and *inline markup*

        tags: [cat, dog, animal]
        ...

    Metadata fields automatically populate template variables.

  * Added `opml` (OPML) as input and output format.  The `_note` attribute,
    used in OmniOutliner and supported by multimarkdown, is supported.
    We treat the contents as markdown blocks under a section header.

  * Added `haddock` (Haddock markup) as input format (David Lazar).

  * Added `revealjs` output format, for reveal.js HTML 5 slide shows.
    (Thanks to Jamie F. Olson for the initial patch.)
    Nested vertical stacks are used for hierarchical structure.
    Results for more than one level of nesting may be odd.

  * Custom writers can now be written in lua.

        pandoc -t data/sample.lua

    will load the script sample.lua and use it as a custom writer.
    (For a sample, do `pandoc --print-default-data-file sample.lua`.)
    Note that pandoc embeds a lua interpreter, so lua need not be
    installed separately.

  * New `--filter/-F` option to make it easier to run "filters"
    (Pandoc AST transformations that operate on JSON serializations).
    Filters are always passed the name of the output format, so their
    behavior can be tailored to it.  The repository
    <https://github.com/jgm/pandocfilters> contains
    a python module for writing pandoc filters in python, with
    a number of examples.

  * Added `--metadata/-M` option.
    This is like `--variable/-V`, but actually adds to metadata, not
    just variables.

  * Added `--print-default-data-file` option, which allows printing
    of any of pandoc's data files. (For example,
    `pandoc --print-default-data-file reference.odt` will print
    `reference.odt`.)

  * Added syntax for "pauses" in slide shows:

        This gives

        . . .

        me pause.

  * New markdown extensions:

    + `ignore_line_breaks`:  causes intra-paragraph line breaks to be ignored,
      rather than being treated as hard line breaks or spaces.  This is useful
      for some East Asian languages, where spaces aren't used between words,
      but text is separated into lines for readability.
    + `yaml_metadata_block`:  Parse YAML metadata blocks.  (Default.)
    + `ascii_identifiers`: This will force `auto_identifiers` to use ASCII
       only. (Default for `markdown_github`.) (#807)
    + `lists_without_preceding_blankline`:  Allow lists to start without
      preceding blank space.  (Default for `markdown_github`.) (#972)

### Behavior changes

  * `--toc-level` no longer implies `--toc`.
    Reason: EPUB users who don't want a visible TOC may still want
    to set the TOC level for in the book navigation.

  * `--help` now prints in and out formats in alphabetical order, and
    says something about PDF output (#720).

  * `--self-contained` now returns less verbose output (telling you
    which URLs it is fetching, but not giving the full header).  In
    addition, there are better error messages when fetching a URL fails.

  * Citation support is no longer baked in to core pandoc. Users who
    need citations will need to install and use a separate filter
    (`--filter pandoc-citeproc`).  This filter will take `bibliography`,
    `csl`, and `citation-abbreviations` from the metadata, though it
    may still be specified on the command line as before.

  * A `Cite` element is now created in parsing markdown whether or not
    there is a matching reference.

  * The `pandoc-citeproc` script will put the bibliography at the
    end of the document, as before.  However, it will be put inside a `Div`
    element with class "references", allowing users some control
    over the styling of references.  A final header, if any, will
    be included in the `Div`.

  * The markdown writer will not print a bibliography if the
    `citations` extension is enabled.  (If the citations are formatted
    as markdown citations, it is redundant to have a bibliography,
    since one will be generated automatically.)

  * Previously we used to store the directory of the first input file,
    even if it was local, and used this as a base directory for finding
    images in ODT, EPUB, Docx, and PDF.  This has been confusing to many
    users.  So we now look for images relative to the current
    working directory, even if the first file argument is in another
    directory.   Note that this change may break some existing workflows.
    If you have been assuming that relative links will be interpreted
    relative to the directory of the first file argument, you'll need
    to make that the current directory before running pandoc. (#942)

  * Better error reporting in some readers, due to changes in `readWith`:
    the line in which the error occurred is printed, with a caret pointing
    to the column.

  * All slide formats now support incremental slide view for definition lists.

  * Parse `\(..\)` and `\[..\]` as math in MediaWiki reader.
    Parse `:<math>...</math>` as display math.  These notations are used with
    the MathJax MediaWiki extension.

  * All writers: template variables are set automatically from metadata
    fields.  However, variables specified on the command line with
    `--variable` will completely shadow metadata fields.

  * If `--variable` is used to set many variables with the same name,
    a list is created.

  * Man writer:  The `title`, `section`, `header`, and `footer` can now
    all be set individually in metadata.  The `description` variable has been
    removed.  Quotes have been added so that spaces are allowed in the
    title.  If you have a title that begins

        COMMAND(1) footer here | header here

    pandoc will still parse it into a title, section, header, and
    footer.  But you can also specify these elements explicitly (#885).

  * Markdown reader

    + Added support for YAML metadata blocks, which can come anywhere
      in the document (not just at the beginning).  A document can contain
      multiple YAML metadata blocks.
    + HTML span and div tags are parsed as pandoc Span and Div elements.

  * Markdown writer

    + Allow simple tables to be printed as grid tables,
      if other table options are disabled.  This means you can do
      `pandoc -t markdown-pipe_tables-simple_tables-multiline_tables`
      and all tables will render as grid tables.
    + Support YAML title block (render fields in alphabetical order
      to make output predictable).

### API changes

  * `Meta` in `Text.Pandoc.Definition` has been changed to allow
    structured metadata.  (Note:  existing code that pattern-matches
    on `Meta` will have to be revised.)  Metadata can now contain
    indefinitely many fields, with content that can be a string,
    a Boolean, a list of `Inline` elements, a list of `Block`
    elements, or a map or list of these.

  * A new generic block container (`Div`) has been added to `Block`,
    and a generic inline container (`Span`) has been added to `Inline`.
    These can take attributes.  They will render in HTML, Textile,
    MediaWiki, Org, RST and Markdown (with `markdown_in_html`
    extension) as HTML `<div>` and `<span>` elements; in other formats
    they will simply pass through their contents.  But they can be
    targeted by scripts.

  * `Format` is now a newtype, not an alias for String.
    Equality comparisons are case-insensitive.

  * Added `Text.Pandoc.Walk`, which exports hand-written tree-walking
    functions that are much faster than the SYB functions from
    `Text.Pandoc.Generic`.  These functions are now used where possible
    in pandoc's code.  (`Tests.Walk` verifies that `walk` and `query`
    match the generic traversals `bottomUp` and `queryWith`.)

  * Added `Text.Pandoc.JSON`, which provides `ToJSON` and `FromJSON`
    instances for the basic pandoc types. They use GHC generics and
    should be faster than the old JSON serialization using
    `Data.Aeson.Generic`.

  * Added `Text.Pandoc.Process`, exporting `pipeProcess`.
    This is a souped-up version of `readProcessWithErrorcode` that
    uses lazy bytestrings instead of strings and allows setting
    environment variables.  (Used in `Text.Pandoc.PDF`.)

  * New module `Text.Pandoc.Readers.OPML`.

  * New module `Text.Pandoc.Writers.OPML`.

  * New module `Text.Pandoc.Readers.Haddock` (David Lazar).
    This is based on Haddock's own lexer/parser.

  * New module `Text.Pandoc.Writers.Custom`.

  * In `Text.Pandoc.Shared`, `openURL` and `fetchItem` now return an
    Either, for better error handling.

  * Made `stringify` polymorphic in `Text.Pandoc.Shared`.

  * Removed `stripTags` from `Text.Pandoc.XML`.

  * `Text.Pandoc.Templates`:

    + Simplified `Template` type to a newtype.
    + Removed `Empty`.
    + Changed type of `renderTemplate`: it now takes a JSON context
      and a compiled template.
    + Export `compileTemplate`.
    + Export `renderTemplate'` that takes a string instead of a compiled
      template.
    + Export `varListToJSON`.

  * `Text.Pandoc.PDF` exports `makePDF` instead of `tex2pdf`.

  * `Text.Pandoc`:

    + Made `toJsonFilter` an alias for `toJSONFilter` from `Text.Pandoc.JSON`.
    + Removed `ToJsonFilter` typeclass.  `ToJSONFilter` from
      `Text.Pandoc.JSON` should be used instead.  (Compiling against
      pandoc-types instead of pandoc will also produce smaller executables.)
    * Removed the deprecated `jsonFilter` function.
    + Added `readJSON`, `writeJSON` to the API (#817).

  * `Text.Pandoc.Options`:

    + Added `Ext_lists_without_preceding_blankline`,
      `Ext_ascii_identifiers`, `Ext_ignore_line_breaks`,
      `Ext_yaml_metadataBlock` to `Extension`.
    + Changed `writerSourceDirectory` to `writerSourceURL` and changed the
      type to a `Maybe`.  `writerSourceURL` is set to 'Just url' when the
      first command-line argument is an absolute URL.  (So, relative links
      will be resolved in relation to the first page.)  Otherwise, 'Nothing'.
    + All bibliography-related fields have been removed from
      `ReaderOptions` and `WriterOptions`: `writerBiblioFiles`,
      `readerReferences`, `readerCitationStyle`.

  * The `Text.Pandoc.Biblio` module has been removed.  Users of the
    pandoc library who want citation support will need to use
    `Text.CSL.Pandoc` from `pandoc-citeproc`.

### Bug fixes

  * In markdown, don't autolink a bare URI that is followed by `</a>`
    (#937).

  * `Text.Pandoc.Shared`

    + `openURL` now follows redirects (#701), properly handles `data:`
      URIs, and prints diagnostic output to stderr rather than stdout.
    + `readDefaultDataFile`: normalize the paths.  This fixes bugs in
      `--self-contained` on pandoc compiled with `embed_data_files` (#833).
    + Fixed `readDefaultDataFile` so it works on Windows.
    + Better error messages for `readDefaultDataFile`.  Instead of
      listing the last path tried, which can confuse people who are
      using `--self-contained`, so now we just list the data file name.
    + URL-escape pipe characters.  Even though these are legal, `Network.URI`
      doesn't regard them as legal in URLs.  So we escape them first (#535).

  * Mathjax in HTML slide shows:  include explicit "Typeset" call.
    This seems to be needed for some formats (e.g. slideous) and won't
    hurt in others (#966).

  * `Text.Pandoc.PDF`

    + On Windows, create temdir in working directory, since the system
      temp directory path may contain tildes, which can cause
      problems in LaTeX (#777).
    + Put temporary output directory in `TEXINPUTS` (see #917).
    + `makePDF` tries to download images that are not found locally,
      if the first argument is a URL (#917).
    + If compiling with `pdflatex` yields an encoding error, offer
      the suggestion to use `--latex-engine=xelatex`.

  * Produce automatic header identifiers in parsing textile, RST,
    and LaTeX, unless `auto_identifiers` extension is disabled (#967).

  * `Text.Pandoc.SelfContained`:  Strip off fragment, query of relative URL
     before treating as a filename.  This fixes `--self-contained` when used
     with CSS files that include web fonts using the method described here:
      <http://paulirish.com/2009/bulletproof-font-face-implementation-syntax/>
      (#739).  Handle `src` in `embed`, `audio`, `source`, `input` tags.

  * `Text.Pandoc.Parsing`: `uri` parser no longer treats punctuation before
    percent-encoding, or a `+` character, as final punctuation.

  * `Text.Pandoc.ImageSize`:  Handle EPS (#903).  This change will make
    EPS images properly sized on conversion to Word.

  * Slidy:  Use slidy.js rather than slidy.js.gz.
    Reason:  some browsers have trouble with the gzipped js file,
    at least on the local file system (#795).

  * Markdown reader

    + Properly handle blank line at beginning of input (#882).
    + Fixed bug in unmatched reference links.  The input
      `[*infile*] [*outfile*]` was getting improperly parsed:
      "infile" was emphasized, but "*outfile*" was literal (#883).
    + Allow internal `+` in citation identifiers (#856).
    + Allow `.` or `)` after `#` in ATX headers if no `fancy_lists`.
    + Do not generate blank title, author, or date metadata elements.
      Leave these out entirely if they aren't present.
    + Allow backtick code blocks not to be preceded by blank line (#975).

  * Textile reader:

    + Correctly handle entities.
    + Improved handling of `<pre>` blocks (#927). Remove internal HTML tags
      in code blocks, rather than printing them verbatim. Parse attributes
      on `<pre>` tag for code blocks.

  * HTML reader: Handle non-simple tables (#893).  Column widths are read from
    `col` tags if present, otherwise divided equally.

  * LaTeX reader

    + Support alltt environment (#892).
    + Support `\textasciitilde`, `\textasciicircum` (#810).
    + Treat `\textsl` as emphasized text reader (#850).
    + Skip positional options after `\begin{figure}`.
    + Support `\v{}` for hacek (#926).
    + Don't add spurious ", " to citation suffixes.
      This is added when needed in pandoc-citeproc.
    + Allow spaces in alignment spec in tables, e.g. `{ l r c }`.
    + Improved support for accented characters (thanks to Scott Morrison).
    + Parse label after section command and set id (#951).

  * RST reader:

    + Don't insert paragraphs where docutils doesn't.
      `rst2html` doesn't add `<p>` tags to list items (even when they are
      separated by blank lines) unless there are multiple paragraphs in the
      list.  This commit changes the RST reader to conform more closely to
      what docutils does (#880).
    + Improved metadata.  Treat initial field list as metadata when
      standalone specified.  Previously ALL fields "title", "author",
      "date" in field lists were treated as metadata, even if not at
      the beginning.  Use `subtitle` metadata field for subtitle.
    + Fixed 'authors' metadata parsing in reST.  Semicolons separate
      different authors.

  * MediaWiki reader

    + Allow space before table rows.
    + Fixed regression for `<ref>URL</ref>`.
      `<` is no longer allowed in URLs, according to the uri parser
      in `Text.Pandoc.Parsing`.  Added a test case.
    + Correctly handle indented preformatted text without preceding
      or following blank line.
    + Fixed `|` links inside table cells.  Improved attribute parsing.
    + Skip attributes on table rows.  Previously we just crashed if
      rows had attributes, now we ignore them.
    + Ignore attributes on headers.
    + Allow `Image:` for images (#971).
    + Parse an image with caption in a paragraph by itself as a figure.

  * LaTeX writer

    + Don't use ligatures in escaping inline code.
    + Fixed footnote numbers in LaTeX/PDF tables.  This fixes a bug
      wherein notes were numbered incorrectly in tables (#827).
    + Always create labels for sections.  Previously the labels were only
      created when there were links to the section in the document (#871).
    + Stop escaping `|` in LaTeX math.
      This caused problems with array environments (#891).
    + Change `\` to `/` in paths.  `/` works even on Windows in LaTeX.
      `\` will cause major problems if unescaped.
    + Write id for code block to label attribute in LaTeX when listings
      is used (thanks to Florian Eitel).
    + Scale LaTeX tables so they don't exceed columnwidth.
    + Avoid problem with footnotes in unnumbered headers (#940).

  * Beamer writer: when creating beamer slides, add `allowframebreaks` option
      to the slide if it is one of the header classes.  It is recommended
      that your bibliography slide have this attribute:

        # References {.allowframebreaks}

    This causes multiple slides to be created if necessary, depending
    on the length of the bibliography.

  * ConTeXt writer: Properly handle tables without captions.  The old output
    only worked in MkII. This should work in MkIV as well (#837).

  * MediaWiki writer: Use native mediawiki tables instead of HTML (#720).

  * HTML writer:

    + Fixed `--no-highlight` (Alexander Kondratskiy).
    + Don't convert to lowercase in email obfuscation (#839).
    + Ensure proper escaping in `<title>` and `<meta>` fields.

  * AsciiDoc writer:

    + Support `--atx-headers` (Max Rydahl Andersen).
    + Don't print empty identifier blocks `([[]])` on headers (Max
      Rydahl Andersen).

  * ODT writer:

    + Fixing wrong numbered-list indentation in open document format
      (Alexander Kondratskiy) (#369).
    + `reference.odt`: Added pandoc as "generator" in `meta.xml`.
    + Minor changes for ODF 1.2 conformance (#939). We leave the
      nonconforming `contextual-spacing` attribute, which is provided by
      LibreOffice itself and seems well supported.

  * Docx writer:

    + Fixed rendering of display math in lists.
      In 1.11 and 1.11.1, display math in lists rendered as a new list
      item.  Now it always appears centered, just as outside of lists,
      and in proper display math style, no matter how far indented the
      containing list item is (#784).
    + Use `w:br` with `w:type` `textWrapping` for linebreaks.
      Previously we used `w:cr` (#873).
    + Use Compact style for Plain block elements, to
      differentiate between tight and loose lists (#775).
    + Ignore most components of `reference.docx`.
      We take the `word/styles.xml`, `docProps/app.xml`,
      `word/theme/theme1.xml`, and `word/fontTable.xml` from
      `reference.docx`, ignoring everything else.  This should help
      with the corruption problems caused when different versions of
      Word resave the reference.docx and reorganize things.
    +  Made `--no-highlight` work properly.

  * EPUB writer

    + Don't add `dc:creator` tags if present in EPUB metadata.
    + Add `id="toc-title"` to `h1` in `nav.xhtml` (#799).
    + Don't put blank title page in reading sequence.
      Set `linear="no"` if no title block.  Addresses #797.
    + Download webtex images and include as data URLs.
      This allows you to use `--webtex` in creating EPUBs.
      Math with `--webtex` is automatically made self-contained.
    + In `data/epub.css`, removed highlighting styles (which
      are no longer needed, since styles are added by the HTML
      writer according to `--highlighting-style`).  Simplified
      margin fields.
    + If resource not found, skip it, as in Docx writer (#916).

  * RTF writer:

    + Properly handle characters above the 0000-FFFF range.
      Uses surrogate pairs.  Thanks to Hiromi Ishii for the patch.
    + Fixed regression with RTF table of contents.
    + Only autolink absolute URIs.  This fixes a regression, #830.

  * Markdown writer:

    + Only autolink absolute URIs.  This fixes a regression, #830.
    + Don't wrap attributes in fenced code blocks.
    + Write full metadata in MMD style title blocks.
    + Put multiple authors on separate lines in pandoc titleblock.
      Also, don't wrap long author entries, as new lines get treated
      as new authors.

  * `Text.Pandoc.Templates`:

    + Fixed bug retrieving default template for markdown variants.
    + Templates can now contain "record lookups" in variables;
      for example, `author.institution` will retrieve the `institution`
      field of the `author` variable.
    + More consistent behavior of `$for$`.  When `foo` is not a list,
      `$for(foo)$...$endfor$` should behave like $if(foo)$...$endif$.
      So if `foo` resolves to "", no output should be produced.
      See pandoc-templates#39.

  * Citation processing improvements (now part of pandoc-citeproc):

    + Fixed `endWithPunct` The new version correctly sees a sentence
      ending in '.)' as ending with punctuation.  This fixes a bug which
      led such sentences to receive an extra period at the end: '.).'.
      Thanks to Steve Petersen for reporting.
    + Don't interfere with Notes that aren't citation notes.
      This fixes a bug in which notes not generated from citations were
      being altered (e.g. first letter capitalized) (#898).
    + Only capitalize footnote citations when they have a prefix.
    + Changes in suffix parsing.  A suffix beginning with a digit gets 'p'
      inserted before it before passing to citeproc-hs, so that bare numbers
      are treated as page numbers by default.  A suffix not beginning with
      punctuation has a space added at the beginning (rather than a comma and
      space, as was done before for not-author-in-text citations).
      The result is that `\citep[23]{item1}` in LaTeX will be interpreted
      properly, with '23' treated as a locator of type 'page'.
    + Many improvements to citation rendering, due to fixes in citeproc-hs
      (thanks to Andrea Rossato).
    + Warnings are issued for undefined citations, which are rendered
      as `???`.
    + Fixed hanging behavior when locale files cannot be found.

### Template changes

  * DocBook:  Use DocBook 4.5 doctype.

  * Org: '#+TITLE:' is inserted before the title.
    Previously the writer did this.

  * LaTeX:  Changes to make mathfont work with xelatex.
    We need the mathspec library, not just fontspec, for this.
    We also need to set options for setmathfont (#734).

  * LaTeX: Use `tex-ansi` mapping for `monofont`.
    This ensures that straight quotes appear as straight, rather than
    being treated as curly.  See #889.

  * Made `\includegraphics` more flexible in LaTeX template.
    Now it can be used with options, if needed.  Thanks to Bernhard Weichel.

  * LaTeX/Beamer: Added `classoption` variable.
    This is intended for class options like `oneside`; it may
    be repeated with different options.  (Thanks to Oliver Matthews.)

  * Beamer: Added `fonttheme` variable.  (Thanks to Luis Osa.)

  * LaTeX: Added `biblio-style` variable (#920).

  * DZSlides: title attribute on title section.

  * HTML5: add meta tag to allow scaling by user (Erik Evenson)

### Under-the-hood improvements

  * Markdown reader:Improved strong/emph parsing, using the strategy of
    <https://github.com/jgm/Markdown>.  The new parsing algorithm requires
    no backtracking, and no keeping track of nesting levels.  It will give
    different results in some edge cases, but these should not affect normal
    uses.

  * Added `Text.Pandoc.Compat.Monoid`.
    This allows pandoc to compile with `base` < 4.5, where `Data.Monoid`
    doesn't export `<>`.  Thanks to Dirk Ullirch for the patch.

  * Added `Text.Pandoc.Compat.TagSoupEntity`.
    This allows pandoc to compile with `tagsoup` 0.13.x.
    Thanks to Dirk Ullrich for the patch.

  * Most of `Text.Pandoc.Readers.TeXMath` has been moved to the
    `texmath` module (0.6.4).  (This allows `pandoc-citeproc` to
    handle simple math in bibliography fields.)

  * Added `Text.Pandoc.Writers.Shared` for shared functions used
    only in writers.  `metaToJSON` is used in writers to create a
    JSON object for use in the templates from the pandoc metadata
    and variables.  `getField`, `setField`, and `defField` are
    for working with JSON template contexts.

  * Added `Text.Pandoc.Asciify` utility module.
    This exports functions to create ASCII-only versions of identifiers.

  * `Text.Pandoc.Parsing`

    + Generalized state type on `readWith` (API change).
    + Specialize readWith to `String` input. (API change).
    + In `ParserState`, replace `stateTitle`, `stateAuthors`, `stateDate`
      with `stateMeta` and `stateMeta'`.

  * `Text.Pandoc.UTF8`: use strict bytestrings in reading.  The use of lazy
     bytestrings seemed to cause problems using pandoc on 64-bit Windows
     7/8 (#874).

  * Factored out `registerHeader` from markdown reader, added to
    `Text.Pandoc.Parsing`.

  * Removed `blaze_html_0_5` flag, require `blaze-html` >= 0.5.
    Reason:  < 0.5 does not provide a monoid instance for Attribute,
    which is now needed by the HTML writer (#803).

  * Added `http-conduit` flag, which allows fetching https resources.
    It also brings in a large number of dependencies (`http-conduit`
    and its dependencies), which is why for now it is an optional flag
    (#820).

  * Added CONTRIBUTING.md.

  * Improved INSTALL instructions.

  * `make-windows-installer.bat`: Removed explicit paths for executables.

  * `aeson` is now used instead of `json` for JSON.

  * Set default stack size to 16M.  This is needed for some large
    conversions, esp. if pandoc is compiled with 64-bit ghc.

  * Various small documentation improvements.
    Thanks to achalddave and drothlis for patches.

  * Removed comment that chokes recent versions of CPP (#933).

  * Removed support for GHC version < 7.2, since pandoc-types now
    requires at least GHC 7.2 for GHC generics.


## pandoc 1.11.1 (2013-03-17)

  * Markdown reader:

    + Fixed regression in which parentheses were lost in link URLs.
      Added tests.  Closes #786.
    + Better handling of unmatched double quotes in `--smart` mode.
      These occur frequently in fiction, since it is customary not to
      close quotes in dialogue if the speaker does not change between
      paragraphs.  The unmatched quotes now get turned into literal
      left double quotes. (No `Quoted` inline is generated, however.)
      Closes #99 (again).

  * HTML writer: Fixed numbering mismatch between TOC and sections.
    `--number-offset` now affects TOC numbering as well
    as section numbering, as it should have all along.  Closes #789.

  * Markdown writer: Reverted 1.11 change that caused citations to be rendered
    as markdown citations, even if `--bibliography` was specified, unless
    `citation` extension is disabled.  Now, formatted citations are always
    printed if `--bibliography` was specified.  If you want to reformat
    markdown keeping pandoc markdown citations intact, don't use
    `--bibliography`.  Note that citations parsed from LaTeX documents will
    be rendered as pandoc markdown citations when `--bibliography` is not
    specified.

  * ODT writer: Fixed regression leading to corrupt ODTs.
    This was due to a change in the `Show` instance for
    `Text.Pandoc.Pretty.Doc`.  Closes #780.

  * Fixed spacing bugs involving code block attributes in
    RST reader and Markdown writer. Closes #763.

  * Windows package:  Various improvements due to Fyodor Sheremetyev.

    + Automatically set installation path (Program Files or Local App Data).
    + Set system PATH environment variable when installing for all users.
    + Pandoc can installed for all users using the following command.
      `msiexec /i pandoc-1.11.msi ALLUSERS=1`.

  * Bumped QuickCheck version bound.

## pandoc 1.11 (2013-03-09)

  * Added `--number-offset` option.  (See README for description.)

  * Added `--default-image-extension` option.  (See README for description.)

  * `--number-sections` behavior change: headers with class `unnumbered`
    will not be numbered.

  * `--version` now reports the default data directory.

  * `Text.Pandoc.Parsing` is no longer exposed. (API change.)

  * `Text.Pandoc.Highlighting` is no longer exposed. (API change.)

  * `Text.Pandoc.Shared`:  Changed type of `Element`.  `Sec` now includes
    a field for `Attr` rather than just `String`.  (API change.)

  * Added `markdown_github` as input format.  This was an accidental
    omission in 1.10.

  * Added `readerDefaultImageExtension` field to `ReaderOptions`.  (API
    change.)

  * Added `writerNumberOffset` field in `WriterOptions`. (API change.)

  * Beamer template:

    + Fixed captions with longtable.  Thanks to Joost Kremers.
    + Provide `\Oldincludegraphics` as in LaTeX template (Benjamin Bannier).

  * LaTeX template:

    + Load microtype after fonts.  Microtype needs to know
      what fonts are being used.  Thanks to dfc for the patch.
    + Set `secnumdepth` to 5 if `--number-sections` specified.
      This yields behavior equivalent to the other writers, numbering
      level 4 and 5 headers too.  Closes #753.

  * HTML reader:

    + Handle `<colgroup>` tag.
    + Preserve all header attributes.

  * LaTeX reader:

    + Parse `\hrule` as `HorizontalRule`.  Closes #746.
    + Parse starred variants of `\section` etc. as headers with
      attribute `unnumbered`.
    + Read optional attributes in `lstlisting` and `Verbatim` environments.
      We convert these to pandoc standard names, e.g. `numberLines`
      for `numbers=left`, `startFrom=100` from `firstnumber=100`.
    + Handle language attribute for lstlistings.
    + Better support for Verbatim and minted environments.  Closes #763.

  * Markdown reader:

    + `-` in an attribute context = `.unnumbered`.  The point of this
      is to provide a way to specify unnumbered headers in non-English
      documents.
    + Fixed bug parsing key/value attributes.  Parsing failed if you
      had an unquoted attribute immediately before the final '}'.
    + Make backslash escape work in attributes.
    + Fix title block parsing.  Now if `mmd_title_blocks` is specified,
      pandoc will parse a MMD title block if it sees one, even if
      `pandoc_title_blocks` is enabled.
    + Refactoring: `litChar` now includes entities, so we don't need
      to use `fromEntities` e.g. on titles.
    + Allow spaces around borders in pipe tables.  Closes #772.
    + Allow all punctuation in angle-bracket autolinks.  Previously
      things like `----` were disallowed, because the uri parser
      treated them as trailing punctuation.  Closes #768.
    + Make `implicit_header_references` work properly when
      headers are given explicit identifiers.
    + Check for tables before line blocks.  Otherwise some pipe
      tables get treated as line blocks.
    + Allow `&` in emails (for entities).
    + Properly handle entities in titles and links.  A markdown link
      `<http://g&ouml;ogle.com>` should be a link to `http://göogle.com`.
      Closes #723.

  * Textile reader:

    + Handle attributes on headers.

  * LaTeX reader:

    + Add `fig:` as title for images with captions.
      This is needed for them to be rendered as figures.  Closes #766.
    + Never emit an empty paragraph.  See #761.
    + Handle `\caption` for images in figures.  Closes #766.
    + Parse `\section*`, etc. as unnumbered sections.

  * HTML writer:

    + Support header attributes.  The attributes go on
      the enclosing `section` or `div` tag if `--section-divs` is specified.
    + Fixed a regression (only now noticed) in html+lhs output.
      Previously the bird tracks were being omitted.

  * LaTeX writer:

    + Omit lists with no items to avoid LaTeX errors.
    + Support line numbering with `--listings`.
      If `numberLines` class is present, we add `numbers=left`;
      if `startFrom` is present, we add `firstnumber=`. (#763)

  * ConTeXt writer:

    + Removed `\placecontent`.  This produced a duplicate toc,
      in conjunction with `\placelist`.
    + Use `\title`, `\subject` etc. for headers with
      `unnumbered` class.

  * Textile writer:

    + Support header attributes.

  * Markdown writer:

    + Use grid tables when needed, and if enabled.  Closes #740.
    + Render citations as pandoc-markdown citations.
      Previously citations were rendered as citeproc-formatted citations
      by default.  Now we render them as pandoc citations, e.g. `[@item1]`,
      unless the `citations` extension is disabled.
      If you still want formatted citations in your markdown output,
      use `pandoc -t markdown-citations`.

  * RST writer:

    + Support `:number-lines:` in code blocks.

  * Docx writer:

    + Better treatment of display math.  Display math inside a
      paragraph is now put in a separate paragraph, so it will render
      properly (centered and without extra blank lines around it).
      Partially addresses #742.
    + Content types and document rels xml files are now created from
      scratch, rather than being taken over from `reference.docx`.
      This fixes problems that arise when you edit the `reference.docx`
      with Word.
    + We also now encode mime types for each individual image rather
      than using defaults.  This should allow us to handle a wider
      range of image types (including PDF).   Closes #414.
    + Changed style names in `reference docx`.
      `FootnoteReference` -> `FootnoteRef`, `Hyperlink` -> `Link`.
      The old names got changed by Word when the `reference.docx` was
      edited.  Closes #414.

  * EPUB writer:

    + Fix section numbering.  Previously the numbering restarted from 1
      in each chapter (with `--number-sections`), though the numbers in
      the table of contents were correct.
    + Headers with "unnumbered" attribute are not numbered.  (Nor do they
      cause an increment in running numbering.) Section numbers now work
      properly, even when there is material before the first numbered section.
    + Include HTML TOC, even in epub2.  The TOC is included in `<spine>`,
      but `linear` is set to `no` unless the `--toc` option is specified.
      Include `<guide>` element in OPF.  This should allow the TOC to
      be usable in Kindles when converted with kindlegen. Closes #773.

  * `Text.Pandoc.Parsing`: Optimized `oneOfStringsCI`.
    This dramatically reduces the speed penalty that comes from enabling the
    `autolink_bare_uris` extension.  The penalty is still substantial (in one
    test, from 0.33s to 0.44s), but nowhere near what it used to be.
    The RST reader is also much faster now, as it autodetects URIs.

  * `Text.Pandoc.Shared`:  `hierarchicalize` will not number section
    with class "unnumbered".  Unnumbered sections get `[]` for their
    section number.

  * `Text.Pandoc.Pretty`:

    + Fixed `chomp` so it works inside `Prefixed` elements.
    + Changed `Show` instance so it is better for debugging.

  * `Text.Pandoc.ImageSize`:  Added `Pdf` to `ImageType`.

  * `Text.Pandoc.UTF8`:  Strip off BOM if present.  Closes #743.

  * Windows installer improvements:

    + The installer is now signed with a certificate (thanks to
      Fyodor Sheremetyev).
    + WiX is used instead of InnoSetup.  The installer is now a
      standard msi file.
    + The version number is now auto-detected, and need not be
      updated separately.

  * OSX installer improvements:

    + The package and pandoc executable are now signed with a
      certificate (thanks to Fyodor Sheremetyev).
    + RTF version of license is used.
    + Use full path for sysctl in `InstallationCheck` script (jonahbull).
      Closes #580.

  * Converted COPYING to markdown.

  * pandoc.cabal:  Require latest versions of highlighting-kate,
    texmath, citeproc-hs, zip-archive.

## pandoc 1.10.1 (2013-01-23)

  * Markdown reader:  various optimizations, leading to a
    significant performance boost.

  * RST reader:  Allow anonymous form of inline links:
    `` `hello <url>`__ `` Closes #724.

  * Mediawiki reader: Don't require newlines after tables.
    Thanks to jrunningen for the patch. Closes #733.

  * Fixed LaTeX macro parsing.  Now LaTeX macro definitions are preserved
    when output is LaTeX, and applied when it is another format.
    Partially addresses #730.

  * Markdown and RST readers:  Added parser to `block` that skips blank
    lines.  This fixes a subtle regression involving grid tables with
    empty cells.  Also added test for grid table with empty cells.
    Closes #732.

  * RST writer:  Use `.. code:: language` for code blocks with language.
    Closes #721.

  * DocBook writer:  Fixed output for hard line breaks, adding a newline
    between `<literallayout>` tags.

  * Markdown writer:  Use an autolink when link text matches url.
    Previously we also checked for a null title, but this
    test fails for links produced by citeproc-hs in bibliographies.
    So, if the link has a title, it will be lost on conversion
    to an autolink, but that seems okay.

  * Markdown writer:  Set title, author, date variables as before.
    These are no longer used in the default template, since we use
    titleblock, but we set them anyway for those who use custom templates.

  * LaTeX writer:  Avoid extra space at start/end of table cell.
    Thanks to Nick Bart for the suggestion of using @{}.

  * `Text.Pandoc.Parsing`:

    + More efficient version of `anyLine`.
    + Type of `macro` has changed; the parser now returns `Blocks`
      instead of `Block`.

  * Relaxed old-time version bound, allowing 1.0.*.

  * Removed obsolete `hsmarkdown` script.  Those who need `hsmarkdown`
    should create a symlink as described in the README.

## pandoc 1.10.0.5 (2013-01-23)

  * Markdown reader: Try `lhsCodeBlock` before `rawTeXBlock`.  Otherwise
    `\begin{code}...\end{code}` isn't handled properly in markdown+lhs.
    Thanks to Daniel Miot for noticing the bug and suggesting the fix.

  * Markdown reader: Fixed bug with headerless grid tables.
    The 1.10 code assumed that each table header cell contains exactly one
    block. That failed for headerless tables (0) and also for tables with
    multiple blocks in a header cell.  The code is fixed and tests provided.
    Thanks to Andrew Lee for pointing out the bug.

  * Markdown reader: Fixed regressions in fenced code blocks. Closes #722.

    + Tilde code fences can again take a bare language string
      (`~~~ haskell`), not just curly-bracketed attributes
      (`~~~ {.haskell}`).
    + Backtick code blocks can take the curly-bracketed attributes.
    + Backtick code blocks don't *require* a language.
    + Consolidated code for the two kinds of fenced code blocks.

  * LaTeX template: Use `\urlstyle{same}` to avoid monospace URLs.

  * Markdown writer: Use proportional font for email autolinks with
    obfuscation.  Closes #714.

  * Corrected name of `blank_before_blockquote` in README.  Closes #718.

  * `Text.Pandoc.Shared`: Fixed bug in `uri` parser.
    The bug prevented an autolink at the end of a string (e.g.
    at the end of a line block line) from counting as a link.  Closes #711.

  * Use the `hsb2hs` preprocessor instead of TH for embed_data_files.
    This should work on Windows, unlike the TH solution with
    file-embed.

  * Eliminated use of TH in test suite.

  * Added `Text.Pandoc.Data` (non-exported) to hold the association
    list of embedded data files, if the `embed_data_files` flag is selected.
    This isolates the code that needs special treatment with file-embed or
    `hsb2hs`.

  * Changes to `make-windows-installer.bat`.

    + Exit batch file if any of the cabal-dev installs fail.
    + There's no longer any need to reinstall `highlighting-kate`.
    + Don't start with a `cabal update`; leave that to the user.
    + Force reinstall of pandoc.

  * Fixed EPUB writer so it builds with blaze-html 0.4.x. Thanks to
    Jens Petersen.

## pandoc 1.10.0.4 (2013-01-20)

  * Fixed bug with escaped % in LaTeX reader. Closes #710.

## pandoc 1.10.0.3 (2013-01-20)

  * Added further missing fb2 tests to cabal file.

## pandoc 1.10.0.2 (2013-01-20)

  * Added fb2 tests to cabal file's extra-source-files.

## pandoc 1.10.0.1 (2013-01-20)

  * Bump version bounds on test-framework packages.

## pandoc 1.10 (2013-01-19)

### New features

  * New input formats:  `mediawiki` (MediaWiki markup).

  * New output formats:  `epub3` (EPUB v3 with MathML),
    `fb2` (FictionBook2 ebooks).

  * New `--toc-depth` option, specifying how many levels of
    headers to include in a table of contents.

  * New `--epub-chapter-level` option, specifying the header
    level at which to divide EPUBs into separate files.
    Note that this normally affects only performance, not the
    visual presentation of the EPUB in a reader.

  * Removed the `--strict` option.  Instead of using `--strict`,
    one can now use the format name `markdown_strict` for either input
    or output.  This gives more fine-grained control that `--strict`
    did, allowing one to convert from pandoc's markdown to strict
    markdown or vice versa.

  * It is now possible to enable or disable specific syntax
    extensions by appending them (with `+` or `-`) to the writer
    or reader name.  For example,

        pandoc -f markdown-footnotes+hard_line_breaks

    disables footnotes and enables treating newlines as hard
    line breaks.  The literate Haskell extensions are now implemented
    this way as well, using either `+lhs` or `+literate_haskell`.
    For a list of extension names, see the README under
    "Pandoc's Markdown."

  * The following aliases have been introduced for specific
    combinations of markdown extensions:  `markdown_phpextra`,
    `markdown_github`, `markdown_mmd`, `markdown_strict`.  These aliases
    work just like regular reader and writer names, and can be modified
    with extension modifiers as described above. (Note that conversion
    from one markdown dialect to another does not work perfectly,
    because there are differences in markdown parsers besides
    just the extensions, and because pandoc's internal document model is
    not rich enough to capture all of the extensions.)

  * New `--html-q-tags` option.  The previous default was to use `<q>`
    tags for smart quotes in HTML5.  But `<q>` tags are also valid HTML4.
    Moreover, they are not a robust way of typesetting quotes, since
    some user agents don't support them, and some CSS resets (e.g.
    bootstrap) prevent pandoc's quotes CSS from working properly.
    We now just insert literal quote characters by default in both
    `html` and `html5` output, but this option is provided for
    those who still want `<q>` tags.

  * The markdown reader now prints warnings (to stderr) about
    duplicate link and note references.  Closes #375.

  * Markdown syntax extensions:

    + Added pipe tables.  Thanks to François Gannaz for the initial patch.
      These conform to PHP Markdown Extra's pipe table syntax. A subset
      of org-mode table syntax is also supported, which means that you can
      use org-mode's nice table editor to create tables.

    + Added support for RST-style line blocks. These are
      useful for verse and addresses.

    + Attributes can now be specified for headers, using the same
      syntax as in code blocks.  (However, currently only the
      identifier has any effect in most writers.)  For example,

            # My header {#foo}

            See [the header above](#foo).

    + Pandoc will now act as if link references have been defined
      for all headers without explicit identifiers.
      So, you can do this:

            # My header

            Link to [My header].
            Another link to [it][My header].

      Closes #691.

  * LaTeX reader:

    + Command macros now work everywhere, including non-math.
      Environment macros still not supported.
    + `\input` now works, as well as `\include`.  TEXINPUTS is used.
      Pandoc looks recursively into included files for more included files.

### Behavior changes

  * The Markdown reader no longer puts the text of autolinks in a
    `Code` inline.  This means that autolinks will no longer appear
    in a monospace font.

  * The character `/` can now appear in markdown citation keys.

  * HTML blocks in strict_markdown are no longer required to begin
    at the left margin.  Technically this is required, according to
    the markdown syntax document, but `Markdown.pl` and other markdown
    processors are more liberal.

  * The `-V` option has been changed so that if there are duplicate
    variables, those specified later on the command line take precedence.

  * Tight lists now work in LaTeX and ConTeXt output.

  * The LaTeX writer no longer relien on the `enumerate` package.
    Instead, it uses standard LaTeX commands to change the list numbering
    style.

  * The LaTeX writer now uses `longtable` instead of `ctable`. This allows
    tables to be split over page boundaries.

  * The RST writer now uses a line block to render paragraphs containing
    linebreaks (which previously weren't supported at all).

  * The markdown writer now applies the `--id-prefix` to footnote IDs.
    Closes #614.

  * The plain writer no longer uses backslash-escaped line breaks
    (which are not very "plain").

  * `Text.Pandoc.UTF8`: Better error message for invalid UTF8.
    Read bytestring and use `Text`'s decodeUtf8 instead of using
    `System.IO.hGetContents`.  This way you get a message saying
    "invalid UTF-8 stream" instead of "invalid byte sequence."
    You are also told which byte caused the problem.

  * Docx, ODT, and EPUB writers now download images specified by a URL
    instead of skipping them or raising an error.

  * EPUB writer:

    + The default CSS now left-aligns headers by default, instead of
      centering.  This is more consistent with the rest of the writers.
    + A proper multi-level table of contents is now used in `toc.ncx`.
      There is no longer a subsidiary table of contents at the beginning
      of each chapter.
    + Code highlighting now works by default.
    + Section divs are used by default for better semantic markup.
    + The title is used instead of "Title Page" in the table of contents.
      Otherwise we have a hard-coded English string, which looks
      strange in ebooks written in other languages.  Closes #572.

  * HTML writer:

    + Put mathjax in span with class "math".  Closes #562.
    + Put citations in a span with class "citation." In HTML5, also include
      a `data-cite` attribute with a space-separated list of citation
      keys.

  * `Text.Pandoc.UTF8`:  use universalNewlineMode in reading.
    This treats both `\r\n` and `\n` as `\n` on input, no matter
    what platform we're running on.

  * Citation processing is now done in the Markdown and LaTeX
    readers, not in `pandoc.hs`.  This makes it easier for library users
    to use citations.

### Template changes

  * HTML: Added css to template to preserve spaces in `<code>` tags.
    Thanks to Dirk Laurie.

  * Beamer:  Remove English-centric strings in section pages.
    Section pages used to have "Section" and a number as well as the
    section title. Now they just have the title.  Similarly for part
    and subsection.  Closes #566.

  * LaTeX, ConTeXt: Added papersize variable.

  * LaTeX, Beamer templates: Use longtable instead of ctable.

  * LaTeX, Beamer templates: Don't require 'float' package for tables.
    We don't actually seem to use the '[H]' option.

  * Markdown, plain: Fixed titleblock so it is just a single string.
    Previously separate title, author, and date variables were used,
    but this didn't allow different kinds of title blocks.

  * EPUB:

    + Rationalized templates.  Previously there were three different
      templates involved in epub production. There is now just one
      template, `default.epub` or `default.epub3`. It can now be
      overridden using `--template`, just like other templates.
      The titlepage is now folded into the default template.
      A `titlepage` variable selects it.
    + UTF-8, lang tag, meta tags, title element.

  * Added scale-to-width feature to beamer template

### API changes

  * `Text.Pandoc.Definition`: Added `Attr` field to `Header`.
    Previously header identifiers were autogenerated by the writers.
    Now they are added in the readers (either automatically or explicitly).

  * `Text.Pandoc.Builder`:

    + `Inlines` and `Blocks` are now synonyms for `Many Inline` and
      `Many Block`.  `Many` is a newtype wrapper around `Seq`, with
      custom Monoid instances for `Many Inline` and `Many Block.  This
      allows `Many` to be made an instance of `Foldable` and `Traversable`.
    + The old `Listable` class has been removed.
    + The module now exports `isNull`, `toList`, `fromList`.
    + The old `Read` and `Show` instances have been removed; derived
      instances are now used.
    + Added `headerWith`.

  * The readers now take a `ReaderOptions` rather than a `ParserState`
    as a parameter.  Indeed, not all parsers use the `ParserState` type;
    some have a custom state.  The motivation for this change was to separate
    user-specifiable options from the accounting functions of parser state.

  * New module `Text.Pandoc.Options`.  This includes the `WriterOptions`
    formerly in `Text.Pandoc.Shared`, and its associated
    data types.  It also includes a new type `ReaderOptions`, which
    contains many options formerly in `ParserState`, and its associated
    data types:

    + `ParserState.stateParseRaw` -> `ReaderOptions.readerParseRaw`.
    + `ParserState.stateColumns` -> `ReaderOptions.readerColumns`.
    + `ParserState.stateTabStop` -> `ReaderOptions.readerTabStop`.
    + `ParserState.stateOldDashes` -> `ReaderOptions.readerOldDashes`.
    + `ParserState.stateLiterateHaskell` -> `ReaderOptions.readerLiterateHaskell`.
    + `ParserState.stateCitations` -> `ReaderOptions.readerReferences`.
    + `ParserState.stateApplyMacros` -> `ReaderOptions.readerApplyMacros`.
    + `ParserState.stateIndentedCodeClasses` ->
      `ReaderOptions.readerIndentedCodeClasses`.
    + Added `ReaderOptions.readerCitationStyle`.

  * `WriterOptions` now includes `writerEpubVersion`, `writerEpubChapterLevel`,
    `writerEpubStylesheet`, `writerEpubFonts`, `writerReferenceODT`,
    `writerReferenceDocx`, and `writerTOCDepth`.  `writerEPUBMetadata` has
    been renamed `writerEpubMetadata` for consistency.

  * Changed signatures of `writeODT`, `writeDocx`, `writeEPUB`, since they no
    longer stylesheet, fonts, reference files as separate parameters.

  * Removed `writerLiterateHaskell` from `WriterOptions`, and
    `readerLiterateHaskell` from `ReaderOptions`.  LHS is now handled
    by an extension (`Ext_literate_haskell`).

  * Removed deprecated `writerXeTeX`.

  * Removed `writerStrict` from `WriterOptions`.  Added `writerExtensions`.
    Strict is now handled through extensions.

  * `Text.Pandoc.Options` exports `pandocExtensions`, `strictExtensions`,
    `phpMarkdownExtraExtensions`, `githubMarkdownExtensions`,
    and `multimarkdownExtensions`, as well as the `Extensions` type.

  * New `Text.Pandoc.Readers.MediaWiki` module, exporting
    `readMediaWiki`.

  * New `Text.Pandoc.Writers.FB2` module, exporting `writeFB2`
    (thanks to Sergey Astanin).

  * `Text.Pandoc`:

    + Added `getReader`, `getWriter` to `Text.Pandoc`.
    + `writers` is now an association list `(String, Writer)`.
      A `Writer` can be a `PureStringWriter`, an `IOStringWriter`, or
      an `IOByteStringWriter`.  ALL writers are now in the 'writers'
      list, including the binary writers and FB2 writer.  This allows
      code in `pandoc.hs` to be simplified.
    + Changed type of `readers`, so all readers are in IO.
      Users who want pure readers can still get them form the reader
      modules; this just affects the function `getReader` that looks up
      a reader based on the format name.  The point of this change is to
      make it possible to print warnings from the parser.

  * `Text.Pandoc.Parsing`:

    + `Text.Parsec` now exports all Parsec functions used in pandoc code.
      No other module directly imports Parsec.  This will make it easier
      to change the parsing backend in the future, if we want to.
    + `Text.Parsec` is used instead of `Text.ParserCombinators.Parsec`.
    + Export the type synonym `Parser`.
    + Export `widthsFromIndices`, `NoteTable'`, `KeyTable'`, `Key'`, `toKey'`,
     `withQuoteContext`, `singleQuoteStart`, `singleQuoteEnd`,
     `doubleQuoteStart`, `doubleQuoteEnd`, `ellipses`, `apostrophe`,
     `dash`, `nested`, `F(..)`, `askF`, `asksF`, `runF`, `lineBlockLines`.
    + `ParserState` is no longer an instance of `Show`.
    + Added `stateSubstitutions` and `stateWarnings` to `ParserState`.
    + Generalized type of `withQuoteContext`.
    + Added `guardEnabled`, `guardDisabled`, `getOption`.
    + Removed `failIfStrict`.
    + `lookupKeySrc` and `fromKey` are no longer exported.

  * `Data.Default` instances are now provided for `ReaderOptions`,
    `WriterOptions`, and `ParserState`.  `Text.Pandoc` re-exports `def`.
    Now you can use `def` (which is re-exported by `Text.Pandoc`) instead
    of `defaultWriterOptions` (which is still defined).  Closes #546.

  * `Text.Pandoc.Shared`:

    + Added `safeRead`.
    + Renamed `removedLeadingTrailingSpace` to `trim`,
      `removeLeadingSpace` to `triml`, and `removeTrailingSpace` to `trimr`.
    + Count `\r` as space in `trim` functions.
    + Moved `renderTags'` from HTML reader and `Text.Pandoc.SelfContained`
      to `Shared`.
    + Removed `failUnlessLHS`.
    + Export `compactify'`, formerly in Markdown reader.
    + Export `isTightList`.
    + Do not export `findDataFile`.
    + `readDataFile` now returns a strict ByteString.
    + Export `readDataFileUTF8` which returns a String, like the
      old `readDataFile`.
    + Export `fetchItem` and `openURL`.

  * `Text.Pandoc.ImageSize`: Use strict, not lazy bytestrings.
    Removed `readImageSize`.

  * `Text.Pandoc.UTF8`: Export `encodePath`, `decodePath`,
     `decodeArg`, `toString`, `fromString`, `toStringLazy`,
     `fromStringLazy`.

  * `Text.Pandoc.UTF8` is now an exposed module.

  * `Text.Pandoc.Biblio`:

    + csl parameter now a `String` rather than a `FilePath`.
    + Changed type of `processBiblio`.  It is no longer in the IO monad.
      It now takes a `Maybe Style` argument rather than parameters for CSL
      and abbrev filenames.  (`pandoc.hs` now calls the functions to parse
      the style file and add abbreviations.)

  * Markdown reader now exports `readMarkdownWithWarnings`.

  * `Text.Pandoc.RTF` now exports `writeRTFWithEmbeddedImages` instead of
    `rtfEmbedImage`.

### Bug fixes

  * Make `--ascii` work properly with `--self-contained`.  Closes #568.

  * Markdown reader:

    + Fixed link parser to avoid exponential slowdowns.  Closes #620.
      Previously the parser would hang on input like this:

            [[[[[[[[[[[[[[[[[[hi

      We fixed this by making the link parser parser characters
      between balanced brackets (skipping brackets in inline code spans),
      then parsing the result as an inline list.  One change is that

            [hi *there]* bud](/url)

      is now no longer parsed as a link.  But in this respect pandoc behaved
      differently from most other implementations anyway, so that seems okay.

    + Look for raw html/latex blocks before tables.
      Otherwise the following gets parsed as a table:

            \begin{code}
            --------------
            -- My comment.
            \end{code}

      Closes #578.

  * RST reader:

    + Added support for `:target:` on `.. image::` blocks
      and substitutions.
    + Field list fixes:

        - Fixed field lists items with body beginning after a new line
          (Denis Laxalde).
        - Allow any char but ':' in names of field lists in RST reader
          (Denis Laxalde).
        - Don't allow line breaks in field names.
        - Require whitespace after field list field names.
        - Don't create empty definition list for metadata field lists.
          Previously a field list consisting only of metadata fields (author,
          title, date) would be parsed as an empty DefinitionList, which is
          not legal in LaTeX and not needed in any format.

    + Don't recognize inline-markup starts inside words.
      For example, `2*2 = 4*1` should not contain an emphasized
      section.  Added test case for "Literal symbols".  Closes #569.
    + Allow dashes as separator in simple tables.  Closes #555.
    + Added support for `container`, `compound`, `epigraph`,
      `rubric`, `highlights`, `pull-quote`.
    + Added support for `.. code::`.
    + Made directive labels case-insensitive.
    + Removed requirement that directives begin at left margin.
      This was (correctly) not in earlier releases; docutils doesn't
      make the requirement.
    + Added support for `replace::` and `unicode::` substitutions.
    + Ignore unknown interpreted roles.
    + Renamed image parser to `subst`, since it now handles all
      substitution references.

  * Textile reader:

    + Allow newlines before pipes in table.  Closes #654.
    + Fixed bug with list items containing line breaks.
      Now pandoc correctly handles hard line breaks inside list items.
      Previously they broke list parsing.
    + Implemented comment blocks.
    + Fixed bug affected words ending in hyphen.
    + Properly handle links with surrounding brackets.
      Square brackets need to be used when the link isn't surrounded by
      spaces or punctuation, or when the URL ending may be ambiguous.
      Closes #564.
    + Removed nullBlock.  Better to know about parsing problems than
      to skip stuff when we get stuck.
    + Allow ID attributes on headers.
    + Textile reader:  Avoid parsing dashes as strikeout.
      Previously the input

            text--
            text--
            text--
            text--

      would be parsed with strikeouts rather than dashes. This fixes
      the problem by requiring that a strikeout delimiting - not be
      followed by a -.  Closes #631.
    + Expanded list of `stringBreakers`.
      This fixes a bug on input like "(_hello_)" which should
      be a parenthesized emphasized "hello".
      The new list is taken from the PHP source of textile 2.4.
    + Fixed autolinks.  Previously the textile reader and writer
      incorrectly implemented RST-style autolinks for URLs and email
      addresses.  This has been fixed.  Now an autolink is done this way:
      `"$":http://myurl.com`.
    + Fixed footnotes bug in textile.  This affected notes occurring
      before punctuation, e.g. `foo[1].`.  Closes #518.

  * LaTeX reader:

    + Better handling of citation commands.
    + Better handling of `\noindent`.
    + Added a 'try' in rawLaTeXBlock, so we can handle `\begin` without `{`.
      Closes #622.
    + Made `rawLaTeXInline` try to parse block commands as well.  This
      is usually what we want, given how `rawLaTeXInline` is used in
      the markdown and textile readers.  If a block-level LaTeX command
      is used in the middle of a paragraph (e.g. `\subtitle` inside a title),
      we can treat it as raw inline LaTeX.
    + Handle `\slash` command.  Closes #605.
    + Basic `\enquote` support.
    + Fixed parsing of paragraphs beginning with a group.  Closes #606.
    + Use curly quotes for bare straight quotes.
    + Support obeylines environment.  Closes #604.
    + Guard against "begin", "end" in inlineCommand and
      blockCommand.
    + Better error messages for environments.  Now it should tell you that
      it was looking for \end{env}, instead of giving "unknown parse error."

  * HTML reader:

    + Added HTML 5 tags to list of block-level tags.
    + HTML reader: Fixed bug in `htmlBalanced`, which
      caused hangs in parsing certain markdown input using
      strict mode.
    + Parse `<q>` as `Quoted DoubleQuote`.
    + Handle nested `<q>` tags properly.
    + Modified `htmlTag` for fewer false positives.
      A tag must start with `<` followed by `!`,`?`, `/`, or a letter.
      This makes it more useful in the wikimedia and markdown parsers.

  * DocBook reader: Support title in "figure" element.  Closes #650.

  * MediaWiki writer:

    + Remove newline after `<br/>` in translation of `LineBreak`
      There's no particular need for a newline (other than making the
      generated MediaWiki source look nice to a human), and in fact
      sometimes it is incorrect: in particular, inside an enumeration, list
      items cannot have embedded newline characters. (Brent Yorgey)
    + Use `<code>` not `<tt>` for Code.

  * Man writer: Escape `-` as `\-`.
    Unescaped `-`'s become hyphens, while `\-`'s are left as ascii minus
    signs.  That is preferable for use with command-line options.  See
    <http://lintian.debian.org/tags/hyphen-used-as-minus-sign.html>.  Thanks
    to Andrea Bolognani for bringing the issue to our attention.

  * RST writer:

    + Improved line block output. Use nonbreaking spaces for
      initial indent (otherwise lost in HTML and LaTeX).
      Allow multiple paragraphs in a single line block.
      Allow soft breaks w continuations in line blocks.
    + Properly handle images with no alt text.  Closes #678.
    + Fixed bug with links with duplicate text.  We now (a) use anonymous
      links for links with inline URLs, and (b) use an inline link instead
      of a reference link if the reference link would require a label that
      has already been used for a different link.  Closes #511.
    + Fixed hyperlinked images. Closes #611. Use `:target:`
      field when you have a simple linked image.
    + Don't add `:align: center` to figures.

  * Texinfo writer:  Fixed internal cross-references.
    Now we insert anchors after each header, and use `@ref` instead of `@uref`
    for links.  Commas are now escaped as `@comma{}` only when needed;
    previously all commas were escaped.  (This change is needed, in part,
    because `@ref` commands must be followed by a real comma or period.) Also
    insert a blank line in from of `@verbatim` environments.

  * DocBook writer:

    + Made --id-prefix work in DocBook as well as HTML.
      Closes #607.
    + Don't include empty captions in figures.  Closes #581.

  * LaTeX writer:

    + Use `\hspace*` for nonbreaking space after line break,
      since `~` spaces after a line break are just ignored.
      Closes #687.
    + Don't escape `_` in URLs or hyperref identifiers.
    + Properly escape strings inside `\url{}`.  Closes #576.
    + Use `[fragile]` only for slides containing code rendered
      using listings. Closes #649.
    + Escape `|` as `\vert` in LaTeX math.  This avoids a clash with
      highlighting-kate's macros, which redefine `|` as a short verbatim
      delimiter.  Thanks to Björn Peemöller for raising this issue.
    + Use minipage rather than parbox for block containers in tables.
      This allows verbatim code to be included in grid tables.
      Closes #663.
    + Prevent paragraphs containing only linebreaks or spaces.

  * HTML writer:

    + Included `highlighting-css` for code spans, too.
      Previously it was only included if used in a code block.  Closes #653.
    + Improved line breaks with `<dd>` tags.  We now put a newline between
      `</dd>` and `<dd>` when there are multiple definitions.
    + Changed mathjax cdn url so it doesn't use https.  (This caused
      problems when used with `--self-contained`.) See #609.

  * EPUB writer:

    + `--number-sections` now works properly.
    + Don't strip meta and link elements in epub metadata.
      Patch from aberrancy. Closes #589.
    + Fixed a couple validation bugs.
    + Use ch001, ch002, etc. for chapter filenames.  This improves sorting
      of chapters in some readers, which apparently sort ch2 after ch10.
      Closes #610.

  * ODT writer: properly set title property (Arlo O'Keeffe).

  * Docx writer:

    + Fixed bug with nested lists.  Previously a list like

            1. one
                - a
                - b
            2. two

      would come out with a bullet instead of "2."
      Thanks to Russell Allen for reporting the bug.
    + Use `w:cr` in `w:r` instead of `w:br` for linebreaks.
      This seems to fix a problem viewing pandoc-generated
      docx files in LibreOffice.
    + Use integer ids for bookmarks.  Closes #626.
    + Added nsid to abstractNum elements.  This helps when merging
      word documents with numbered or bulleted lists.  Closes #627.
    + Use separate footnotes.xml for notes.
      This seems to help LibreOffice convert the file, even though
      it was valid docx before.  Closes #637.
    + Use rIdNN identifiers for r:embed in images.
    + Avoid reading image files again when we've already processed them.
    + Fixed typo in `referenc.docx` that prevented image captions from
      working. Thanks to Huashan Chen.

  * `Text.Pandoc.Parsing`:

    + Fixed bug in `withRaw`, which didn't correctly handle the case
      where nothing is parsed.
    + Made `emailAddress` parser more correct.  Now it is based on RFC 822,
      though it still doesn't implement quoted strings in email addresses.
    + Revised URI parser.  It now allows many more schemes, allows
      uppercase URIs, and better handles trailing punctuation and
      trailing slashes in bare URIs.  Added many tests.
    + Simplified and improved singleQuoteStart.  This makes `'s'`, `'l'`,
      etc. parse properly.  Formerly we had some English-centric heuristics,
      but they are no longer needed. Closes #698.

  * `Text.Pandoc.Pretty`:  Added wide punctuation range to `charWidth`.
    This fixes bug with Chinese commas in markdown and reST tables, and
    a bug that caused combining characters to be dropped.

  * `Text.Pandoc.MIME`: Added MIME types for .wof and .eot.  Closes #640.

  * `Text.Pandoc.Biblio`:

    + Run `mvPunc` and `deNote` on metadata too.
      This fixed a bug with notes on titles using footnote styles.
    + Fixed bug in fetching CSL files from CSL data directory.

  * `pandoc.hs`:  Give correct value to `writerSourceDirectory` when a URL
    is provided.  It should be the URL up to the path.

  * Fixed/simplified diff output for tests.
    Biblio: Make sure mvPunc and deNote run on metadata too.
    This fixed a bug with notes on titles using footnote styles.

### Under the hood improvements

  * We no longer depend on `utf8-string`.  Instead we use functions
    defined in `Text.Pandoc.UTF8` that use `Data.Text`'s conversions.

  * Use `safeRead` instead of using `reads` directly (various modules).

  * "Implicit figures" (images alone in a paragraph) are now handled
    differently.  The markdown reader gives their titles the prefix `fig:`; the
    writers look for this before treating the image as a figure.  Though this
    is a bit of a hack, it has two advantages: (i) implicit figures can be
    limited to the markdown reader, and (ii) they can be deactivated by turning
    off the `implicit_figures` extension.

  * `catch` from `Control.Exception` is now used instead of the
    old Preface `catch`.

  * `Text.Pandoc.Shared`:  Improved algorithm for `normalizeSpaces`
    and `oneOfStrings` (which is now non-backtracking).

  * `Text.Pandoc.Biblio`: Remove workaround for `toCapital`.
    Now citeproc-hs is fixed upstream, so this is no longer needed.
    Closes #531.

  * Textile reader: Improved speed of `hyphenedWords`.
    This speeds up the textile  reader by about a factor of 4.

  * Use `Text.Pandoc.Builder` in RST reader, for more flexibility,
    better performance, and automatic normalization.

  * Major rewrite of markdown reader:

    + Use `Text.Pandoc.Builder` instead of lists.  This also
      means that everything is normalized automatically.
    + Move to a one-pass parsing strategy, returning values in the reader
      monad, which are then run (at the end of parsing) against the final
      parser state.

  * In HTML writer, we now use `toHtml` instead of pre-escaping.
    We work around the problem that blaze-html unnecessarily escapes `'`
    by pre-escaping just the `'` characters, instead of the whole string.
    If blaze-html later stops escaping `'` characters, we can simplify
    `strToHtml` to `toHtml`.  Closes #629.

  * Moved code for embedding images in RTFs from `pandoc.hs` to the
    RTF writer (which now exports `writeRTFWithEmbeddedImages`).

  * Moved citation processing from `pandoc.hs` into the readers.
    This makes things more convenient for library users.

  * The man pages are now built by an executable `make-pandoc-man-pages`,
    which has its own stanza in the cabal file so that dependencies can be
    handled by Cabal. Special treatment in `Setup.hs` ensures that this
    executable never gets installed; it is only used to create the man pages.

  * The cabal file has been modified so that the pandoc library is used
    in building the pandoc executable.  (This required moving `pandoc.hs`
    from `src` to `.`.)  This cuts compile time in half.

  * The `executable` and `library` flags have been removed.

  * `-threaded` has been removed from ghc-options.

  * Version bounds of dependencies have been raised, and the
    `blaze_html_0_5` flag now defaults to True.  Pandoc now compiles on
    GHC 7.6.

  * We now require base >= 4.2.

  * Integrated the benchmark program into cabal.  One can now do:

        cabal configure --enable-benchmarks && cabal build
        cabal bench --benchmark-option='markdown' --benchmark-option='-s 20'

    The benchmark now uses README + testsuite, so benchmark results
    from older versions aren't comparable.

  * Integrated test suite with cabal.
    To run tests, configure with `--enable-tests`, then `cabal test`.
    You can specify particular tests using `--test-options='-t markdown'`.
    No output is shown unless tests fail.  The Haskell test modules
    have been moved from `src/` to `tests/`.

  * Moved all data files and templates to the `data/` subdirectory.

  * Added an `embed_data_files` cabal flag.  This causes all
    data files to be embedded in the binary, so that the binary
    is self-sufficient and can be relocated anywhere, copied on
    a USB key, etc.  The Windows installer now uses this.
    (Since we no longer have the option to build the executable
    without the library, this is the only way to get a relocatable
    binary on Windows.)

  * Removed pcre3.dll from windows package.
    It isn't needed unless highlighting-kate is compiled with the
    `pcre-light` flag. By default, regex-prce-builtin is used.


## pandoc 1.9.4.5 (2012-10-21)

  * Raised version bounds on network, base64-bytestring, json,
    and template-haskell.

## pandoc 1.9.4.4 (2012-10-20)

  * Removed `tests` flag and made test suite into a proper cabal
    test suite, which can now be enabled using `--enable-tests`
    and run with `cabal test`.

  * Moved man page creation out of `Setup.hs` and into an
    executable built by Cabal, but never installed.  This
    allows dependencies to be specified, and solves a problem
    with 1.9.4.3, which could only be installed if `data-default`
    had already been installed.

  * Updated `lhs-latex.tex` test for latest highlighting-kate
    representation of backticks.

## pandoc 1.9.4.3 (2012-10-20)

  * Removed `-threaded` from default compile flags.

  * Modified modules to compile with GHC 7.6 and latest version of time
    package.

## pandoc 1.9.4.2 (2012-06-29)

  * Don't encode/decode file paths if base >= 4.4.
    Prior to base 4.4, filepaths and command line arguments were treated
    as unencoded lists of bytes, not unicode strings, so we had to work
    around that by encoding and decoding them. This commit adds CPP
    checks for the base version that intelligibly enable encoding/decoding
    when needed. Fixes a bug with multilingual filenames when pandoc was
    compiled with ghc 7.4 (#540).

  * Don't generate an empty H1 after hrule slide breaks.
    We now use a slide-level header with contents `[Str "\0"]` to mark
    an hrule break.  This avoids creation of an empty H1 in these
    contexts.  Closes #484.

  * Docbook reader: Added support for "bold" emphasis.  Thanks to mb21.

  * In make_osx_package.sh, ensure citeproc-hs is built with the
    embed_data_files flag.

  * MediaWiki writer: Avoid extra blank lines after sublists (Gavin Beatty).

  * ConTeXt writer: Don't escape `&`, `^`, `<`, `>`, `_`,
    simplified escapes for `}` and `{` to `\{` and `\}` (Aditya Mahajan).

  * Fixed handling of absolute URLs in CSS imports with `--self-contained`.
    Closes #535.

  * Added webm to mime types. Closes #543.

  * Added some missing exports and tests to the cabal file
    (Alexander V Vershilov).

  * Compile with `-rtsopts` and `-threaded` by default.

## pandoc 1.9.4.1 (2012-06-08)

  * Markdown reader: Added `cf.` and `cp.` to list of likely abbreviations.

  * LaTeX template: Added `linkcolor`, `urlcolor` and `links-as-notes`
    variables.  Make TOC links black.

  * LaTeX template improvements.

    + Don't print date unless one is given explicitly in the document.
    + Simplified templates.
    + Use fontenc [T1] by default, and lmodern.
    + Use microtype if available.

  * Biblio:

    + Add comma to beginning of bare suffix, e.g. `@item1 [50]`.
      Motivation: `@item1 [50]` should be as close as possible to
      `[@item1, 50]`.
    + Added workaround for a bug in citeproc-hs 0.3.4 that causes footnotes
      beginning with a citation to be empty.  Closes #531.

  * Fixed documentation on mixed lists.  Closes #533.

## pandoc 1.9.4 (2012-06-03)

  * Simplified `Text.Pandoc.Biblio` and fixed bugs with citations inside
    footnotes and captions.  We now handle note citations by inserting
    footnotes during initial citation processing, and doing a separate
    pass later to remove notes inside notes.

  * Added 'zenburn' highlight style from highlighting-kate.

  * Added Slideous writer. Slideous is an HTML + javascript slide show
    format, similar to Slidy, but works with IE 7. (Jonas Smedegaard)

  * LaTeX writer:

    + Ensure we don't have extra blank lines at ends of cells.
      This can cause LaTeX errors, as they are interpreted as new paragraphs.
    + More consistent interblock spacing.
    + Require highlighting-kate >= 0.5.1, for proper highlighted inline
      code in LaTeX.  Closes #527.
    + Ensure that a Verbatim at the end of a footnote is followed by
      a newline. (Fixes a regression in the previous version.)
    + In default template, use black for internal links and TOC.
      Added commented-out code to use footnotes for links, as would
      be suitable in print output.

  * Beamer writer:  When `--incremental` is used, lists inside
    a block quote should appear all at once.  (This makes Beamer
    output consistent with the HTML slide show formats.)

  * ConTeXt writer:

    + Escape `%` as `\letterpercent{}` not `\letterpercent `,
      to avoid gobbling spaces after the `%` sign.
    + Ensure space after `\stopformula`.

  * Markdown writer:

    + Use `:` form instead of `~` in definition lists, for better
      compatibility with other markdown implementations.
    + Don't wrap the term, because it breaks definition lists.
    + Use a nonzero space to prevent false recognition
      of list marker in ordered lists.  Closes #516.

  * Org writer: Add space before language name.  Closes #523.

  * Docx writer: Simplified bullet characters so they work properly
    with Word 2007. Closes #520.

  * LaTeX reader: Support `\centerline`.

  * RST reader:  handle figures.  Closes #522.

  * Textile reader: fix for `<notextile>` and `==`.  Closes #517.
    (Paul Rivier)

## pandoc 1.9.3 (2012-05-12)

  * Added docbook reader (with contributions from Mauro Bieg).

  * Fixed bug in `fromEntities`.  The previous version would turn
    `hi & low you know;` into `hi &`.

  * HTML reader:

    + Don't skip nonbreaking spaces.
      Previously a paragraph containing just `&nbsp;` would be rendered
      as an empty paragraph. Thanks to Paul Vorbach for pointing out the bug.
    + Support `<col>` and `<caption>` in tables. Closes #486.

  * Markdown reader:

    + Don't recognize references inside delimited code blocks.
    + Allow list items to begin with lists.

  * LaTeX reader:

    + Handle `\bgroup`, `\egroup`, `\begingroup`, `\endgroup`.
    + Control sequences can't be followed by a letter.
      This fixes a bug where `\begingroup` was parsed as `\begin`
      followed by `group`.
    + Parse 'dimension' arguments to unknown commands.  e.g. `\parindent0pt`
    + Make `\label` and `\ref` sensitive to `--parse-raw`.
      If `--parse-raw` is selected, these will be parsed as raw latex
      inlines, rather than bracketed text.
    + Don't crash on unknown block commands (like `\vspace{10pt}`)
      inside `\author`; just skip them.  Closes #505.

  * Textile reader:

    + Implemented literal escapes with `==` and `<notextile>`.  Closes #473.
    + Added support for LaTeX blocks and inlines (Paul Rivier).
    + Better conformance to RedCloth inline parsing (Paul Rivier).
    + Parse '+text+' as emphasized (should be underlined, but this
      is better than leaving literal plus characters in the output.

  * Docx writer: Fixed multi-paragraph list items.  Previously they each
    got a list marker.  Closes #457.

  * LaTeX writer:

    + Added `--no-tex-ligatures` option to avoid replacing
      quotation marks and dashes with TeX ligatures.
    + Use `fixltx2e` package to provide `\textsubscript`.
    + Improve spacing around LaTeX block environments:
      quote, verbatim, itemize, description, enumerate.
      Closes #502.
    + Use blue instead of pink for URL links in latex/pdf output.

  * ConTeXt writer: Fixed escaping of `%`.
    In text, `%` needs to be escaped as `\letterpercent`, not `\%`
    Inside URLs, `%` needs to be escaped as `\%`
    Thanks to jmarca and adityam for the fix.  Closes #492.

  * Texinfo writer:  Escape special characters in node titles.
    This fixes a problem pointed out by Joost Kremers.  Pandoc used
    to escape an '@' in a chapter title, but not in the corresponding
    node title, leading to invalid texinfo.

  * Fixed document encoding in texinfo template.
    Resolves Debian Bug #667816.

  * Markdown writer:

    + Don't force delimited code blocks to be flush left.
      Fixes bug with delimited code blocks inside lists etc.
    + Escape `<` and `$`.

  * LaTeX writer: Use `\hyperref[ident]{text}` for internal links.
    Previously we used `\href{\#ident}{text}`, which didn't work on
    all systems. Thanks to Dirk Laurie.

  * RST writer: Don't wrap link references.  Closes #487.

  * Updated to use latest versions of blaze-html, mtl.


## pandoc 1.9.2 (2012-04-05)

  * LaTeX reader:

    + Made `lstlisting` work as a proper verbatim environment.
    + Fixed bug parsing LaTeX tables with one column.

  * LaTeX writer:

    + Use `{}` around `ctable` caption, so that formatting can be used.
    + Don't require eurosym package unless document has a €.

  * LaTeX template: Added variables for `geometry`, `romanfont`,
    `sansfont`, `mathfont`, `mainfont` so users can more easily
    customize fonts.

  * PDF writer:

    + Run latex engine at least two times, to ensure
      that PDFs will have hyperlinked bookmarks.
    + Added PDF metadata (title,author) in LaTeX standalone + PDF output.

  * Texinfo writer: retain directories in image paths.  (Peter Wang)

  * RST writer:  Better handling of inline formatting, in accord
    with docutils' "inline markup recognition rules" (though we don't
    implement the unicode rules fully). Now `hi*there*hi` gets
    rendered properly as `hi\ *there*\ hi`, and unnecessary
    `\ ` are avoided around `:math:`, `:sub:`, `:sup:`.

  * RST reader:

    + Parse `\ ` as null, not escaped space.
    + Allow `` :math:`...` `` even when not followed by blank
      or `\`.  This does not implement the complex rule docutils follows,
      but it should be good enough for most purposes.
    + Add support for the rST default-role directive. (Greg Maslov)

  * Text.Pandoc.Parsing: Added `stateRstDefaultRole` field to `ParserState`.
    (Greg Maslov)

  * Markdown reader: Properly handle citations nested in other inline
    elements.

  * Markdown writer:  don't replace empty alt in image with "image".

  * DZSlides:  Updated template.html and styles in default template.
    Removed bizarre CSS for `q` in dzslides template.

  * Avoid repeated `id` attribute in section and header in HTML slides.

  * README improvements:  new instructions on internal links,
    removed misleading note on reST math.

  * Build system:

    + Fixed Windows installer so that dzslides works.
    + Removed stripansi.sh.
    + Added .travis.yml for Travis continuous integration support..
    + Fixed upper bound for zlib (Sergei Trofimovich).
    + Fixed upper bound for test-framework.
    + Updated haddocks for haddock-2.10 (Sergei Trofimovich).


## pandoc 1.9.1.2 (2012-03-09)

  * Added `beamer+lhs` as output format.

  * Don't escape `<` in `<style>` tags with `--self-contained`.
    This fixes a bug which prevented highlighting from working
    when using `--self-contained`.

  * PDF: run latex engine three times if `--toc` specified.
    This fixes page numbers in the table of contents.

  * Docx writer: Added TableNormal style to tables.

  * LaTeX math environment fixes. `aligned` is now used instead of
    the nonexistent `aligned*`. `multline` instead of the nonexistent
    `multiline`.

  * LaTeX writer: Use `\textasciitilde` for literal `~`.

  * HTML writer: Don't escape contents of EQ tags with --gladtex.
    This fixes a regression from 1.8.

  * Use `<q>` tags for Quoted items for HTML5 output.
    The quote style can be changed by modifying the template
    or including a css file. A default quote style is included.

  * LaTeX reader: Fixed accents (\~{a}, `\c{c}`).
    Correctly handle \^{}. Support "minted" as a LaTeX verbatim block.

  * Updated LaTeX template for better language support.
    Use `polyglossia` instead of `babel` with xetex.
    Set `lang` as documentclass option.
    `\setmainlanguage` will use the last of a comma-separated
    list of languages. Thanks to François Gannaz.

  * Fixed default LaTeX template so `\euro` and `€` work.  The
    `eurosym` package is needed if you are using pdflatex.

  * Fixed escaping of period in man writer (thanks to Michael Thompson).

  * Fixed list label positions in beamer.

  * Set `mainlang` variable in context writer.
    This parallels behavior of latex writer.  `mainlang` is the last
    of a comma-separated list of languages in lang.

  * EPUB language metadat: convert e.g. `en_US` from locale to `en-US`.

  * Changed `-V` so that you can specify a key without a value.
    Such keys get the value `true`.

  * Fixed permissions on installed man pages - thanks Magnus Therning.

  * Windows installer: require XP or higher.  The installer is
    now compiled on a Windows 7 machine, which fixes a problem
    using citation functions on Windows 7.

  * OSX package: Check for 64-bit Intel CPU before installing.


## pandoc 1.9.1.1 (2012-02-11)

  * Better handling of raw latex environments in markdown.  Now

        \begin{equation}
        a_1
        \end{equation}

    turns into a raw latex block as expected.

  * Improvements to LaTeX reader:

    + Skip options after block commands.
    + Correctly handle `{\\}` in braced.
    + Added a needed 'try'.
    + Citations: add `, ` to suffix if it doesn't start with space or
      punctuation. Otherwise we get no space between the year and the
      suffix in author-date styles.

  * Added two needed data files for S5.  This fixes a problem with
    `pandoc -t s5 --self-contained`.  Also removed `slides.min.js`,
    which was no longer being used.

  * Fixed some minor problems in `reference.docx`:
    name on "Date" style, `xCs` instead of `xIs`.

  * Fixed a problem creating docx files using a reference docx
    modified using Word.  The problem seems to be that Word
    modifies `_rels/.rels`, changing the Type of the Relationship to
    `docProps/core.xml`. Pandoc now changes this back to the correct
    value if it has been altered, fixing the problem.

  * Fixed html5 template so it works properly with highlighting.

## pandoc 1.9.1 (2012-02-09)

  * LaTeX reader:

    + Fixed regression in 1.9; properly handle escaped $ in latex math.
    + Put LaTeX verse environments in blockquotes.

  * Markdown reader:

    + Limit nesting of strong/emph.  This avoids exponential lookahead
      in parasitic cases, like `a**a*a**a*a**a*a**a*a**a*a**a*a**a*a**`.
    + Improved attributes syntax (in code blocks/spans): (1)
      Attributes can contain line breaks. (2) Values in key-value
      attributes can be surrounded by either double or single quotes, or
      left unquoted if they contain no spaces.

  * Headers no longer wrap in markdown or RST writers.

  * Added `stateMaxNestingLevel` to `ParserState`.
    We set this to 6, so you can still have `Emph` inside `Emph`,
    just not indefinitely.

  * More efficient implementation of `nowrap` in `Text.Pandoc.Pretty`.

  * `Text.Pandoc.PDF`:  Only run latex twice if `\tableofcontents`
    is present.

  * Require highlighting-kate >= 0.5.0.2, texmath >= 0.6.0.2.

## pandoc 1.9.0.5 (2012-02-06)

  * Changed cabal file so that build-depends for the test program
    are not required unless the tests flag is used.

  * LaTeX writer:  insert `{}` between adjacent hyphens so they don't
    form ligatures (dashes) in code spans.

## pandoc 1.9.0.4 (2012-02-06)

  * Raised version bound on test-framework to avoid problems
    compiling tests on GHC 7.4.1.

  * LaTeX reader: Use raw LaTeX as fallback inline text for Cites,
    so citations don't just disappear unless you process with
    citeproc.  Ignore `\bibliographystyle`, `\nocite`.

  * Simplified tex2pdf; it will always run latex twice to
    resolve table of contents and hyperrefs.

## pandoc 1.9.0.3 (2012-02-06)

  * Require Cabal >= 1.10.

  * Tweaked cabal file to meet Cabal 1.10 requirements.

## pandoc 1.9.0.2 (2012-02-05)

  * Allow build with json 0.4 or 0.5.  Otherwise we can't build with
    ghc 6.12.

## pandoc 1.9 (2012-02-05)

### New features

  * Added a Microsoft Word `docx` writer. The writer includes support
    for highlighted code and for math (which is converted from TeX to OMML,
    Office's native math markup language, using texmath's new OMML module).
    A new option `--reference-docx` allows the user to customize the
    styles.

  * Added an `asciidoc` writer (<http://www.methods.co.nz/asciidoc/>).

  * Better support for slide shows:

    + Added a `dzslides` writer. DZSlides is a lightweight HTML5/javascript
      slide show format due to Paul Rouget (<http://paulrouget.com/dzslides/>).

    + Added a LaTeX `beamer` writer. Beamer is a LaTeX package for creating
      slide presentations.

    + New, flexible rules for dividing documents into sections and slides
      (see the "Structuring the slide show" in the User's Guide).  These
      are backward-compatible with the old rules, but they allow slide
      shows to be organized into sections and subsections containing
      multiple slides.

    + A new `--slide-level` option allows users to override defaults
      and select a slide level below the first header level with content.

  * A new `--self-contained` option produces HTML output that does not
    depend on an internet connection or the presence of any external
    files. Linked images, CSS, and javascript is downloaded (or fetched
    locally) and encoded in `data:` URIs. This is useful for making portable
    `HTML slide shows. The --offline` option has been deprecated and is now
    `treated as a synonym or --self-contained`.

  * Support for PDF output:

    + Removed the old `markdown2pdf`.
    + `pandoc` can now create PDFs (assuming you have latex and a set of
      appropriate packages installed): just specify an output file with the
      `.pdf` extension.
    + A new option `--latex-engine` allows you to specify `pdflatex`,
      `xelatex`, or `lualatex` as the processor.

  * Highlighting changes:

    + Syntax highlighting is now a standard feature; the `highlighting`
      flag is no longer needed when compiling.
    + A new `--no-highlight` option allows highlighting to be disabled.
    + Highlighting now works in `docx`, `latex`, and `epub`, as well as
      `html`, `html5`, `dzslides`, `s5`, and `slidy`.
    + A new `--highlight-style` option selects between various highlighting
      color themes.

  * Internal links to sections now work in ConTeXt and LaTeX as well as HTML.

  * LaTeX `\include` and `\usepackage` commands are now processed,
    provided the files are in the working directory.

  * EPUB improvements:

    + Internal and external links now work in EPUB.
    + Raw HTML is allowed.
    + New `--epub-embed-font` option.
    + Customizable templates for EPUB pages offer more control over
      formatting: `epub-page.html`, `epub-coverimage.html`,
      `epub-titlepage.html`.

  * `--mathml` now works with DocBook.

  * Added support for math in RST reader and writer.  Inline math uses the
    `` :math:`...` `` construct.  Display math uses

        .. math:: ...

    or if the math is multiline,

        .. math::

           ...

    These constructions are now supported now by `rst2latex.py`.

  * Github syntax for fenced code blocks is supported in pandoc's
    markdown.  You can now write

        ```ruby
        x = 2
        ```

    instead of

        ~~~ {.ruby}
        x = 2
        ~~~~

  * Easier scripting:  a new `toJsonFilter` function makes it easier to
    write Haskell scripts to manipulate the Pandoc AST.
    See [Scripting with pandoc](scripting.html#pandoc-1.9-changes).

### Behavior changes

  * Fixed parsing of consecutive lists in markdown.
    Pandoc previously behaved like Markdown.pl for consecutive
    lists of different styles. Thus, the following would be parsed
    as a single ordered list, rather than an ordered list followed
    by an unordered list:

        1. one
        2. two

        - one
        - two

    This change makes pandoc behave more sensibly, parsing this as
    two lists.  Any change in list type (ordered/unordered) or in
    list number style will trigger a new list. Thus, the following
    will also be parsed as two lists:

        1. one
        2. two

        a. one
        b. two

    Since we regard this as a bug in Markdown.pl, and not something
    anyone would ever rely on, we do not preserve the old behavior
    even when `--strict` is selected.

  * Dashes work differently with `--smart`: `---` is always em-dash,
    and `--` is always en-dash.  Pandoc no longer tries to guess when
    `-` should be en-dash.  *Note:* This may change how existing documents
    look when processed with pandoc. A new option, `--old-dashes`,
    is provided for legacy documents.

  * The markdown writer now uses setext headers for levels 1-2.
    The old behavior (ATX headers for all levels) can be restored
    using the new `--atx-headers` option.

  * Links are now allowed in markdown image captions.  They are also
    allowed in links, but will appear there as regular text. So,

        [link with [link](/url)](/url)

    will turn into

        <p><a href="/url">link with link</a></p>

  * Improved handling of citations using `citeproc-hs-0.3.4`.
    Added `--citation-abbreviations` option.

  * Citation keys can no longer end with a punctuation character.
    This means that `@item1.` will be parsed as a citation with key
    'item1', followed by a period, instead of a citation with key
    'item1.', as was the case previously.

  * In HTML output, citations are now put in a span with class `citation`.

  * The markdown reader now recognizes DocBook block and inline tags.
    It was always possible to include raw DocBook tags in a markdown
    document, but now pandoc will be able to distinguish block from
    inline tags and behave accordingly. Thus, for example,

        <sidebar>
        hello
        </sidebar>

    will not be wrapped in `<para>` tags.

  * The LaTeX parser has been completely rewritten; it is now much more
    accurate, robust, and extensible. However, there are two important
    changes in how it treats unknown LaTeX. (1) Previously, unknown
    environments became BlockQuote elements; now, they are treated
    as "transparent", so `\begin{unknown}xyz\end{unknown}` is the
    same as `xyz`.  (2) Previously, arguments of unknown commands
    were passed through with their braces; now the braces are stripped
    off.

  * `--smart` is no longer selected automatically with `man` output.

  * The deprecated `--xetex` option has been removed.

  * The `--html5`/`-5` option has been deprecated. Use `-t html5`
    instead. `html5` and `html5+lhs` are now separate output formats.

  * Single quotes are no longer escaped in HTML output.  They do not
    need to be escaped outside of attributes.

  * Pandoc will no longer transform leading newlines in code
    blocks to `<br/>` tags.

  * The ODT writer now sizes images appropriately, using the image
    size and DPI information embedded in the image.

  * `--standalone` is once again implicitly for a non-text output format
    (ODT, EPUB).  You can again do `pandoc test.txt -o test.odt`
    and get a standalone ODT file.

  * The Docbook writer now uses `<sect1>`, `<sect2>`, etc. instead of
    `<section>`.

  * The HTML writer now uses `<del>` for strikeout.

  * In HTML output with `--section-divs`, the classes `section` and
    `level[1,2,..6]` are put on the `div` tags so they can be styled.
    In HTML 5 output with `--section-divs`, the classes
    `level[1,2,...6]` are put on `section` tags.

  * EPUB writer changes:

    + The `lang` variable now sets the language
      in the metadata (if it is not set, we default to the locale).
    + EPUB:  UTF-8 is used rather than decimal entities.

  * Added `titleslide` class to title slide in S5 template.

  * In HTML, EPUB, and docx metadata, the date is normalized into
    YYYY-MM-DD format if possible. (This is required for validation.)

  * Attributes in highlighted code blocks are now preserved in HTML.
    The container element will have the classes, id, and key-value attributes
    you specified in the delimited code block. Previously these were stripped
    off.

  * The reference backlink in the HTML writer no longer has a special
    `footnoteBacklink` class.

  * The HTML template has been split into `html` and `html5` templates.

  * Author and date are treated more consistently in HTML templates.
    Authors are now `<h2>`, date `<h3>`.

  * URLs are hyphenated in the ConTeXt writer (B. Scott Michel).

  * In `Text.Pandoc.Builder`, `+++` has been replaced by `<>`.

### Bug fixes

  * Better support for combining characters and East Asian wide characters
    in markdown and reST.

  * Better handling of single quotes with `--smart`.
    Previously `D'oh l'*aide*` would be parsed with left and right single
    quotes instead of apostrophes. This kind of error is now fixed.

  * Highlighting: Use `reads` instead of `read` for better error handling.
    Fixes crash on `startNum="abc"`.

  * Added blank comment after directives in rst template.

  * Unescape entities in citation `refId`. The `refId`s coming
    from citeproc contain XML numeric entities, and these don't match with the
    citation keys parsed by pandoc. Solution is to unescape them.

  * HTML reader: Fixed bug parsing tables with both thead and tbody.

  * Markdown reader:

    + Better handling of escapes in link URLs and titles.
    + Fixed backslash escapes in reference links.
    + Fixed bug in table/hrule parsing, by checking that the top
      line of a table is not followed by a blank line. This bug caused
      slowdowns on some files with hrules and tables, as pandoc tried to
      interpret the hrules as the tops of multiline tables.
    + Fixed bug in code block attribute parser. Previously the ID attribute
      got lost if it didn't come first. Now attributes can come in any order.

  * RST reader: allow footnotes followed by newline without space characters.

  * LaTeX reader:

    + Ignore empty groups {}, { }.
    + LaTeX reader: Handle `\@`.
    + LaTeX reader:  Don't crash on commands like `\itemsep`.
    + LaTeX reader:  Better handling of letter environments.

  * RST writer: Fixed bug involving empty table cells. isSimple was being
    calculated in a way that assumed there were no non-empty cells.

  * ConTeXt writer:

    + Made `--toc` work even without `--number-sections`.
    + Escape # in link URLs.
    + Use buffering for footnotes containing code blocks.
    + Changed 'descr' to 'description', fixed alignment.

  * LaTeX writer:

    + Escape euro character.
    + Don't escape `~` inside `\href{...}`.
    + Escape `#` in href URLs.
    + Improved detection of book classes.  We now check the
      `documentclass` variable, and if that is not set, we look through
      the template itself.  Also, we have added the KOMA classes scrreprt
      and scrbook.  You can now make a book using
      `pandoc -V documentclass:book mybook.txt -o mybook.pdf`
    + LHS files now set the "listings" variable, so that the definition
      of the `code` environment will be included in the template.
    + Links are colored blue by default (this can be changed by
      modifying `hyperref` settings in the template).
    + Added `lang` variable to LaTeX template.

  * HTML writer:

    + Fixed bug in HTML template with html5 and mathml.
    + Don't use self-closing img, br, hr tags for HTML5.
    + Use `<section>` for footnotes if HTML5.
    + Update HTML templates to use Content-Style-Type meta tag.
    + Use separate variables for meta-date, meta-author.
      This makes footnotes work in author and date fields.
    + Use 'vertical-align:middle' in WebTeX math for better alignment.

  * S5/slidy writer:  Make footnotes appear on separate slide at end.

  * MIME: Added 'layout-cache' to getMimeType. This ensures that the
    META-INF/manifest.xml for ODT files will have everything it needs, so
    that ODT files modified by LibreOffice can be used as `--reference-odt`.

  * `Text.Pandoc.Templates`: Return empty string for json template.

  * `Text.Pandoc.Biblio`:

    + Expand citations recursively inside nested inlines.
    + Treat `\160` as space when parsing locator and suffix.
      This fixes a bug with "p. 33" when `--smart` is used. Previously
      the whole "p. 33" would be included in the suffix, with no locator.
    + Put whole author-in-text citation in a Cite.  Previously just the
      date and other info went in the Cite.
    + Don't add comma+space to prefix if it ends in punctuation.

  * Updated chicago-author-date.csl.  The old version did not work
    properly for edited volumes with no author.

  * EPUB writer:

    + Add date to EPUB titlepage and metadata.
    + Added TOC identifier in EPUB page template.
    + Don't generate superfluous file `cover-image.jpg`.

###  Under the hood improvements

  * Modified `make_osx_package.sh` to use cabal-dev.
    Items are no longer installed as root.
    Man pages are zipped and given proper permissions.

  * Modified windows installer generator to use cabal-dev.

  * Setup: Making man pages now works with cabal-dev (at least on OSX). In
    Setup.hs we now invoke 'runghc' in a way that points it to the correct
    package databases, instead of always falling back to the default user
    package db.

  * Updated to work with GHC 7.4.1.

  * Removed dependency on old-time.

  * Removed dependency on dlist.

  * New slidy directory for "self-contained."

  * TeXMath writer:  Use unicode thin spaces for thin spaces.

  * Markdown citations: don't strip off initial space in locator.

### API changes

  * Removed `Apostrophe`, `EmDash`, `EnDash`, and `Ellipses`
    from the native `Inline` type in pandoc-types.  Now we use `Str`
    elements with unicode.

  * Improvements to `Text.Pandoc.Builder`:

    + `Inlines` and `Blocks` are now newtypes (not synonyms for
      sequences).
    + Instances are defined for `IsString`, `Show`, `Read`, `Monoid`,
      and a new `Listable` class, which allows these to be manipulated
      to some extent like lists. Monoid append includes automatic
      normalization.
    + `+++` has been replaced by `<>` (mappend).

  * Use blaze-html instead of xhtml for HTML generation.
    This changes the type of `writeHtml`.

  * `Text.Pandoc.Shared`:

    + Added `warn` and `err`.
    + Removed `unescapeURI`, modified `escapeURI`.
      (See under [behavior changes], above.)

  * Changes in URI escaping:  Previously the readers escaped URIs by
    converting unicode characters to octets and then percent encoding.
    Now unicode characters are left as they are, and `escapeURI` only
    percent-encodes space characters.  This gives more readable
    URIs, and works well with modern user agents. URIs are no longer unescaped
    at all on conversion to `markdown`, `asciidoc`, `rst`, `org`.

  * New module `Text.Pandoc.SelfContained`.

  * New module `Text.Pandoc.Docx`.

  * New module `Text.Pandoc.PDF`.

  * Added `writerBeamer` to `WriterOptions`.

  * Added `normalizeDate` to `Text.Pandoc.Shared`.

  * Added `splitStringWithIndices` in `Text.Pandoc.Shared`.
    This is like `splitWithIndices`, but it is sensitive to distinctions
    between wide, combining, and regular characters.

  * `Text.Pandoc.Pretty`:

    + Added `chomp` combinator.
    + Added `beforeNonBreak` combinator.  This allows you to include
      something conditionally on it being before a nonblank.
      Used for RST inline math.
    + Added `charWidth` function. All characters marked W or F in the unicode
      spec EastAsianWidth.txt get width 2.
    + Added `realLength`, based on `charWidth`. `realLength` is now
      used in calculating offsets.

  * New module `Text.Pandoc.Slides`, for common functions for breaking
    a document into slides.

  * Removed `Text.Pandoc.S5`, which is no longer needed.

  * Removed `Text.Pandoc.CharacterReferences`.  Moved
    `characterReference` to `Text.Pandoc.Parsing`.
    `decodeCharacterReferences` is replaced by `fromEntities`
    in `Text.Pandoc.XML`.

  * Added `Text.Pandoc.ImageSize`.  This is intened for use
    in `docx` and `odt` writers, so the size and dpi of images
    can be calculated.

  * Removed `writerAscii` in `WriterOptions`.

  * Added `writerHighlight` to `WriterOptions`.

  * Added `DZSlides` to `HTMLSlideVariant`.

  * `writeEPUB` has a new argument for font files to embed.

  * Added `stateLastStrPos` to `ParserState`. This lets us keep track
    of whether we're parsing the position immediately after a regular
    (non-space, non-symbol) string, which is useful for distinguishing
    apostrophes from single quote starts.

  * `Text.Pandoc.Parsing`:

    + `escaped` now returns a `Char`.
    + Removed `charsInBalanced'`, added a character parser as
      a parameter of `charsInBalanced`.  This is needed for
      proper handling of escapes, etc.
    + Added `withRaw`.

  * Added `toEntities` to `Text.Pandoc.XML`.

  * `Text.Pandoc.Readers.LaTeX`:

    + Export `handleIncludes`.
    + Export `rawLaTeXBlock` instead of `rawLaTeXEnvironment'`.

  * Added `ToJsonFilter` class and `toJsonFilter` function to
    `Text.Pandoc`, deprecating the old `jsonFilter` function.

  * `Text.Pandoc.Highlighting`:

     + Removed `highlightHtml`, `defaultHighlightingCss`.
     + Export `formatLaTeXInline`, `formatLaTeXBlock`, and `highlight`, plus
       key functions from highlighting-kate.
     + Changed types of highlighting function.  `highlight` returns a
       `Maybe`, not an `Either`.


## pandoc 1.8.2.1 (2011-08-01)

  * Adjusted Arbitrary instance to help avoid timeouts in tests.

  * Added `Tests.Writers.Markdown` to cabal file.

  * Relaxed version bounds on pandoc-types, test-framework.

## pandoc 1.8.2 (2011-07-30)

  * Added script to produce OS X package.

  * Made `templates` directory a git submodule.  This should make it
    easier for people to revise their custom templates when the default
    templates change.

  * Changed template naming scheme: `FORMAT.template` -> `default.FORMAT`.
    **Note:** If you have existing templates in `~/.pandoc/templates`, you
    must rename them to conform to the new scheme!

  * Default template improvements:

    + HTML:  Display author and date after title.
    + HTML:  Made table of contents more customizable.  The container
      for the TOC is now in the template, so users can insert a header
      or other styling. (Thanks to Bruce D'Arcus for the suggestion.)
    + HTML, Slidy, S5:  Enclose scripts in CDATA tags.
    + Slidy, S5: Added `s5-url` and `slidy-url` variables, instead of
      hard-coding.  If you want to put your slidy files in the slidy
      subdirectory, for example, you can do
      `pandoc -t slidy -V slidy-url=slidy -s`.
    + LaTeX: Use `\and` to separate authors in LaTeX documents (reader
      & writer).  Closes #279.
    + LaTeX: Set `\emergencystretch` to prevent overfull lines.
    + LaTeX: Use different `hyperref` options for `xetex`, fixing
      problems with unicode bookmarks (thanks to CircleCode).
    + LaTeX: Removed `ucs` package, use `utf8` rather than `utf8x`
      with `inputenc`.  This covers fewer characters but is more
      robust with other packages, and `ucs` is unmaintained.  Users
      who need better unicode support should use xelatex or lualatex.

  * If a template specified with `--template` is not found, look for it
    in `datadir`.  Also, if no extension is provided, supply one based
    on the writer.  So now you can put your `special.latex` template in
    `~/.pandoc/templates`, and use it from any directory via
    `pandoc -t latex --template special`.

  * Added `nonspaceChar` to `Text.Pandoc.Parsing`.

  * Fixed smart quotes bug, now handling `'...hi'` properly.

  * RST reader:

    + Partial support for labeled footnotes.
    + Improved accuracy of `simpleReferenceName` parser.

  * HTML reader:

    + Substitute correct unicode characters for
      characters in the 128..159 range, which are often found even in
      HTML that purports to be UTF-8.

  * LaTeX reader:  Handle `\subtitle` command (a subtitle is added
    to the title, after a colon and linebreak). Closes #280.

  * Leaner `reference.odt`.

  * Added unexported module `Text.Pandoc.MIME` for use in
    the ODT writer.

  * ODT writer:  Construct `manifest.xml` based on archive contents.
    This fixes a bug in ODTs containing images. Recent versions of
    LibreOffice would reject these as corrupt, because `manifest.xml`
    did not contain a reference to the image files.

  * LaTeX writer:

    + Make verbatim environments flush to avoid spurious
      blank lines.  Closes #277.
    + Use `\texttt` and escapes insntead of `\verb!...!`, which
      is too fragile (doesn't work in command arguments).
    + Use `\enquote{}` for quotes if the template includes
      the `csquotes` package. This provides better support for
      local quoting styles. (Thanks to Andreas Wagner for the idea.)

  * ConTeXt writer:  Make `\starttyping`/`\stoptyping` flush with
    margin, preventing spurious blank lines.

  * Slidy writer:

    + Use non-minimized version of `slidy.css` with `--offline`
      option, so users can more easily edit it.
    + Also fixed a bug in the CSS that prevented proper centering
      of title (now reported and fixed upstream).

  * S5 writer:

    + Replaced `s5/default/slides.js.{comment,packed}` with
      new compressed `s5/default/slides.min.js`.
    + Use `data:` protocol to embed S5 CSS in `<link>` tags,
      when `--offline` is specified. Using inline CSS didn't
      work with Chrome or Safari.  This fixes offline
      S5 on those browsers.

  * HTML writer:  Removed English title on footnote backlinks.
    This is incongrous in non-English documents.

  * Docbook writer:

    + Use CALS tables.  (Some older docbook software does not work
      well with XHTML tables.)  Closes #77.
    + Use `programlisting` tags (instead of `screen`) for code blocks.

  * `markdown2pdf`:

    + Calls latex with `-halt-on-error -interaction nonstopmode` instead
      of `-interaction=batchmode`, which essentially just ignored errors,
      leading to bad results. Better to know when something is wrong.
    + Fixed issues with non-UTF-8 output of `pdflatex`.
    + Better error reporting.

  * `--mathjax` now takes an optional URL argument. If it is not
    provided, pandoc links directly to the (secure) mathjax CDN,
    as now recommended (thanks to dsanson).

  * Deprecated `--xetex` option in `pandoc`.  It is no longer needed,
    since the LaTeX writer now produces a file that can be processed by
    `latex`, `pdflatex`, `lualatex`, or `xelatex`.

  * Introduced `--luatex` option to `markdown2pdf`. This causes `lualatex`
    to be used to create the PDF.


## pandoc 1.8.1.2 (2011-07-16)

  * Added `--epub-cover-image` option.

  * Documented `--biblatex` and `--natbib` options.

  * Allow `--section-divs` with slidy output.  Resolves Issue #296.

  * Disallow notes within notes in reST and markdown.
    These previously caused infinite looping and stack overflows.
    For example:

        [^1]

        [^1]: See [^1]

    Note references are allowed in reST notes, so this isn't a full
    implementation of reST. That can come later. For now we need to
    prevent the stack overflows.  Partially resolves Issue #297.

  * EPUB writer: Allow non-plain math methods.

  * Forbid ()s in citation item keys.  Resolves Issue #304: problems with
    `(@item1; @item2)` because the final paren was being parsed as part of
    the item key.

  * Changed URI parser so it doesn't include trailing punctuation.
    So, in RST, `http://google.com.` should be parsed as a link followed by a
    period. The parser is smart enough to recognize balanced parentheses, as
    often occur in wikipedia links: `http://foo.bar/baz_(bam)`.

  * Markdown+lhs reader: Require space after inverse bird tracks, so that
    HTML tags can be used freely at the left margin of a markdown+lhs document.
    Thanks to Conal Elliot for the suggestion.

  * Markdown reader: Fixed bug in footnote order (reported by CircleCode).

  * RST reader:
      + Fixed bug in in field lists with multi-line items at the
        end of the list.
      + Added parentheses to RST `specialChars`, so
        `(http://google.com)` will be parsed as a link in parens.
        Resolves Issue #291.
      + Allow `|` followed by newline in RST line block.

  * LaTeX reader:
      + Support `\dots`.
      + Gobble option & space after linebreak `\\[10pt]`.

  * Textile reader:
      + Make it possible to have colons after links.  (qerub)
      + Make it possible to have colons after links.  (Christoffer Sawicki)

  * HTML reader:
      + Skip spaces after `<b>`, `<emph>`, etc.
      + Handle tbody, thead in simple tables.  Closes #274.
      + Implicit `Para`s instead of `Plains` in some contexts.

  * OpenDocument writer:  Use special `First paragraph` style for
    first paragraph after most non-paragraph blocks. This allows users to
    specify e.g. that only paragraphs after the first paragraph of a block are
    to be indented. Thanks to Andrea Rossato for the patch. Closes #20.

  * LaTeX writer:  use `deVerb` on table and picture captions.
    Otherwise LaTeX complains about `\verb` inside command argument.
    Thanks to bbanier for reporting the bug.

  * Markdown writer: Insert HTML comment btw list and indented code block.
    This prevents the code block from being interpreted as part of the list.

  * EPUB writer: Add a meta element specify the cover.
    Some EPUB e-readers, such as the Nook, require a meta element inside the
    OPF metadata block to ensure the cover image is properly displayed.
    (Kelsey Hightower)

  * HTML writer: Use embed tag for images with non-image extensions.
    (e.g.  PDFs).  Closes #264.

  * LaTeX writer: Improved tables.

      + More space between lines, top-align cells.
      + Use ctable package, which allows footnotes and
        provides additional options.
      + Made cell alignments work in multiline tables.
      + Closes #271, #272.

  * Un-URI-escape image filenames in LaTeX, ConTeXt, RTF, Texinfo.
    Also do this when copying image files into EPUBs and ODTs.
    Closes #263.

  * Changed to github issue tracker.

  * Added failing emph/strong markdown test case due to Perry Wagle.

  * Slidy improvements:
      + Updated to use Slidy2.
      + Fixed bug, unclosed div tag.
      + Added `duration` variable in template.
        Setting this activates the timer.
      + Use 'titlepage' instead of 'cover' for title div.


## pandoc 1.8.1.1 (2011-02-13)

  * `markdown2pdf`:  Removed some debugging lines accidentally included
    in the 1.8.1 release. With those lines, the temp directory is created
    in the working directory, and it is not deleted.  This fix restores
    the original behavior.

## pandoc 1.8.1 (2011-02-13)

  * Added `--ascii` option.  Currently supported only in HTML writer,
    which it causes to use numerical entities instead of UTF-8.

  * EPUB writer: `--toc` now works to provide a table of contents
    at the beginning of each chapter.

  * LaTeX writer:  Change figure defaults to `htbp`.
    This prevents "too many unprocessed floats."  Resolves
    Issue #285.

  * `Text.Pandoc.UTF8`:  Encode filenames even when using recent
    base.

  * `markdown2pdf`: Fixed filename encoding issues. With help from Paulo
    Tanimoto. Resolves Issue #286.

  * HTML writer: Put line breaks in section divs.

  * `Text.Pandoc.Shared`: Make `writerSectionDivs` default to False.

## pandoc 1.8.0.3 (2011-02-05)

  * Fixed Source-repository stanza in cabal file.

## pandoc 1.8.0.2 (2011-02-05)

  * HTML writer:

    + Stringify alt text instead of converting to HTML.
    + Break lines after block elements, not inside tags.
      HTML output now closely resembles that of tidy. Resolves Issue #134.

  * Markdown reader: Fixed bug in footnote block parser (pointed out
    by Jesse Rosenthal).  The problem arose when the blank line
    at the end of a footnote block contained indenting spaces.

  * Shared: Improved 'normalize' function so it normalizes Spaces too.
    In normal form, Space elements only occur to separate two non-Space
    elements.  So, we never have [Space], or [, ..., Space].

  * Tests:

    + Improved Arbitrary instance.
    + Added timeout for test instances.

  * README:

    + Added section on four-space rule for lists.  Resolves Issue #283.
    + Clarified optional arguments on math options.

  * markdown2pdf: Fixed bug with output file extensions.
    Previously `markdown2pdf test.txt -o test.en.pdf` would produce
    `test.pdf`, not `test.en.pdf`.  Thanks to Paolo Tanimoto for the fix.

## pandoc 1.8.0.1 (2011-01-31)

  * Revised Interact.hs so that it works with the CPP macros
    in the UTF8 module.

  * Revised Setup.hs so that we don't call MakeManPage.hs unless
    the man pages are out of date.

## pandoc  1.8 (2011-01-30)

### New features

  * Support for citations using Andrea Rossato's `citeproc-hs` 0.3.
    You can now write, for example,

        Water is wet [see @doe99, pp. 33-35; also @smith04, ch. 1].

    and, when you process your document using `pandoc`, specifying
    a citation style using `--csl` and a bibliography using `--bibliography`,
    the citation will be replaced by an appropriately formatted
    citation, and a list of works cited will be added to the end
    of the document.

    This means that you can switch effortlessly between different citation
    and bibliography styles, including footnote, numerical, and author-date
    formats. The bibliography can be in any of the following formats: MODS,
    BibTeX, BibLaTeX, RIS, EndNote, EndNote XML, ISI, MEDLINE, Copac, or JSON.
    See the README for further details.

    Citations are supported in the markdown reader, using a special
    syntax, and in the LaTeX reader, using natbib or biblatex syntax.
    (Thanks to Nathan Gass for the natbib and biblatex support.)

  * New `textile` reader and writer.  Thanks to Paul Rivier for contributing
    the `textile` reader, an almost complete implementation of the textile
    syntax used by the ruby [RedCloth library](http://redcloth.org/textile).
    Resolves Issue #51.

  * New `org` writer, for Emacs Org-mode, contributed by Puneeth Chaganti.

  * New `json` reader and writer, for reading and writing a JSON
    representation of the native Pandoc AST.  These are much faster
    than the `native` reader and writer, and should be used for
    serializing Pandoc to text.  To convert between the JSON representation
    and native Pandoc, use `encodeJSON` and `decodeJSON` from
    `Text.JSON.Generic`.

  * A new `jsonFilter` function in `Text.Pandoc` makes it easy
    to write scripts that transform a JSON-encoded pandoc document.
    For example:

        -- removelinks.hs - removes links from document
        import Text.Pandoc
        main = interact $ jsonFilter $ bottomUp removeLink
                 where removeLink (Link xs _) = Emph xs
                       removeLink x = x

    To use this to remove links while translating markdown to LaTeX:

        pandoc -t json | runghc removelinks.hs | pandoc -f json -t latex

  * Attributes are now allowed in inline `Code` elements, for example:

        In this code, `ulist ! [theclass "special"] << elts`{.haskell} is...

    The attribute syntax is the same as for delimited code blocks.
    `Code` inline has an extra argument place for attributes, just like
    `CodeBlock`. Inline code will be highlighted in HTML output, if pandoc
    is compiled with highlighting support. Resolves Issue #119.

  * New `RawBlock` and `RawInline` elements (replacing `RawHtml`,
    `HtmlInline`, and `TeX`) provide lots of flexibility in writing
    scripts to transform Pandoc documents. Scripts can now change
    how each element is rendered in each output format.

  * You can now define LaTeX macros in markdown documents, and pandoc
    will apply them to TeX math.  For example,

        \newcommand{\plus}[2]{#1 + #2}
        $\plus{3}{4}$

    yields `3+4`.  Since the macros are applied in the reader, they
    will work in every output format, not just LaTeX.

  * LaTeX macros can also be used in LaTeX documents (both in math
    and in non-math contexts).

  * A new `--mathjax` option has been added for displaying
    math in HTML using MathJax.  Resolves issue #259.

  * Footnotes are now supported in the RST reader. (Note, however,
    that unlike docutils, pandoc ignores the numeral or symbol used in
    the note; footnotes are put in an auto-numbered ordered list.)
    Resolves Issue #258.

  * A new `--normalize` option causes pandoc to normalize the AST
    before writing the document.  This means that, for example,
    `*hi**there*` will be rendered as `<em>hithere</em>`
    instead of `<em>hi</em><em>there</em>`.  This is not the default,
    because there is a significant performance penalty.

  * A new `--chapters` command-line option causes headers
    in DocBook, LaTeX, and ConTeXt to start with "chapter" (level one).
    Resolves Issue #265.

  * In DocBook output, `<chapter>` is now used for top-level
    headers if the template contains `<book>`. Resolves Issue #265.

  * A new `--listings` option in `pandoc` and `markdown2pdf` causes
    the LaTeX writer to use the listings package for code blocks.
    (Thanks to Josef Svennigsson for the pandoc patch, and Etienne
    Millon for the markdown2pdf patch.)

  * `markdown2pdf` now supports `--data-dir`.

  * URLs in autolinks now have class "url" so they can be styled.

  * Improved prettyprinting in most formats.  Lines will be wrapped
    more evenly and duplicate blank lines avoided.

  * New `--columns` command-line option sets the column width for
    line wrapping and relative width calculations for tables.

  * Made `--smart` work in HTML, RST, and Textile readers, as well
    as markdown.

  * Added `--html5` option for HTML5 output.

  * Added support for listings package in LaTeX reader
    (Puneeth Chaganti).

  * Added support for simple tables in the LaTeX reader.

  * Added support for simple tables in the HTML reader.

  * Significant performance improvements in many readers and writers.

### API and program changes

  * Moved `Text.Pandoc.Definition` from the `pandoc` package to a new
    auxiliary package, `pandoc-types`. This will make it possible for other
    programs to supply output in Pandoc format, without depending on the whole
    pandoc package.

  * Added `Attr` field to `Code`.

  * Removed `RawHtml`, `HtmlInline`, and `TeX` elements; added generic
    `RawBlock` and `RawInline`.

  * Moved generic functions to `Text.Pandoc.Generic`. Deprecated
    `processWith`, replacing it with two functions, `bottomUp` and `topDown`.
    Removed previously deprecated functions `processPandoc` and `queryPandoc`.

  * Added `Text.Pandoc.Builder`, for building `Pandoc` structures.

  * `Text.Pandoc` now exports association lists `readers` and `writers`.

  * Added `Text.Pandoc.Readers.Native`, which exports `readNative`.
    `readNative` can now read full pandoc documents, block lists, blocks,
    inline lists, or inlines.  It will interpret `Str "hi"`
    as if it were `Pandoc (Meta [] [] []) [Plain [Str "hi"]]`.
    This should make testing easier.

  * Removed deprecated `-C/--custom-header` option.
    Use `--template` instead.

  * `--biblio-file` has been replaced by `--bibliography`.
    `--biblio-format` has been removed; pandoc now guesses the format
    from the file extension (see README).

  * pandoc will treat an argument as a URI only if it has an
    `http(s)` scheme.  Previously pandoc would treat some
    Windows pathnames beginning with `C:/` as URIs.

  * The `--sanitize-html` option and the `stateSanitize` field in
    `ParserState` have been removed. Sanitization is better done in the
    resulting HTML using `xss-sanitize`, which is based on pandoc's
    sanitization, but improved.

  * pandoc now adds a newline to the end of its output in fragment
    mode (= not `--standalone`).

  * Added support for `lang` in `html` tag in the HTML template,
    so you can do `pandoc -s -V lang=es`, for example.

  * `highlightHtml` in `Text.Pandoc.Highlighting` now takes
    a boolean argument that selects between "inline" and
    "block" HTML.

  * `Text.Pandoc.Writers.RTF` now exports `rtfEmbedImage`.
    Images are embedded in RTF output when possible (png, jpeg).
    Resolves Issue #275.

  * Added `Text.Pandoc.Pretty`. This is better suited for pandoc than the
    `pretty` package.  Changed all writers that used
    `Text.PrettyPrint.HughesPJ` to use `Text.Pandoc.Pretty` instead.

  * Rewrote `writeNative` using the new prettyprinting module. It is
    now much faster. The output has been made more consistent and compressed.
    `writeNative` is also now sensitive to writerStandalone`, and will simply
    `print a block list if writerStandalone` is False.

  * Removed `Text.Pandoc.Blocks`. `Text.Pandoc.Pretty` allows you to define
    blocks and concatenate them, so a separate module is no longer needed.

  * `Text.Pandoc.Shared`:

    + Added `writerColumns`, `writerChapters`, and `writerHtml5` to
      `WriterOptions`.
    + Added `normalize`.
    + Removed unneeded prettyprinting functions:
      `wrapped`, `wrapIfNeeded`, `wrappedTeX`, `wrapTeXIfNeeded`, `hang'`,
      `BlockWrapper`, `wrappedBlocksToDoc`.
    + Made `splitBy` take a test instead of an element.
    + Added `findDataFile`, refactored `readDataFile`.
    + Added `stringify`. Rewrote `inlineListToIdentifier` using `stringify`.
    + Fixed `inlineListToIdentifier` to treat `\160` as ' '.

  * `Text.Pandoc.Readers.HTML`:

    + Removed `rawHtmlBlock`, `anyHtmlBlockTag`, `anyHtmlInlineTag`,
      `anyHtmlTag`, `anyHtmlEndTag`, `htmlEndTag`, `extractTagType`,
      `htmlBlockElement`, `htmlComment`
    + Added `htmlTag`, `htmlInBalanced`, `isInlineTag`, `isBlockTag`,
      `isTextTag`

  * Moved `smartPunctuation` from `Text.Pandoc.Readers.Markdown`
    to `Text.Pandoc.Readers.Parsing`, and parameterized it with
    an inline parser.

  * Ellipses are no longer allowed to contain spaces.
    Previously we allowed '. . .', ' . . . ', etc.  This caused
    too many complications, and removed author's flexibility in
    combining ellipses with spaces and periods.

  * Allow linebreaks in URLs (treat as spaces). Also, a string of
    consecutive spaces or tabs is now parsed as a single space. If you have
    multiple spaces in your URL, use `%20%20`.

  * `Text.Pandoc.Parsing`:

    + Removed `refsMatch`.
    + Hid `Key` constructor.
    + Removed custom `Ord` and `Eq` instances for `Key`.
    + Added `toKey` and `fromKey` to convert between `Key` and `[Inline]`.
    + Generalized type on `readWith`.

  * Small change in calculation of relative widths of table columns.
    If the size of the header > the specified column width, use
    the header size as 100% for purposes of calculating
    relative widths of columns.

  * Markdown writer now uses some pandoc-specific features when `--strict`
    is not specified: `\` newline is used for a hard linebreak instead of
    two spaces then a newline. And delimited code blocks are used when
    there are attributes.

  * HTML writer:  improved gladTeX output by setting ENV appropriately
    for display or inline math (Jonathan Daugherty).

  * LaTeX writer: Use `\paragraph`, `\subparagraph` for level 4,5 headers.

  * LaTeX reader:

    + `\label{foo}` and `\ref{foo}` now become `{foo}` instead of `(foo)`.
    + `\index{}` commands are skipped.

  * Added `fontsize` variable to default LaTeX template.
    This makes it easy to set the font size using `markdown2pdf`:
    `markdown2pdf -V fontsize=12pt input.txt`.

  * Fixed problem with strikeout in LaTeX headers when using
    hyperref, by adding a command to the default LaTeX template
    that disables `\sout` inside pdf strings. Thanks to Joost Kremers
    for the fix.

  * The `COLUMNS` environment variable no longer has any effect.

### Under-the-hood improvements

  * Pandoc now compiles with GHC 7. (This alone leads to a
    significant performance improvement, 15-20%.)

  * Completely rewrote HTML reader using tagsoup as a lexer. The
    new reader is faster and more accurate.  Unlike the
    old reader, it does not get bogged down on some input
    (Issues #277, 255). And it handles namespaces in tags
    (Issue #274).

  * Replaced `escapeStringAsXML` with a faster version.

  * Rewrote `spaceChar` and some other parsers in Text.Pandoc.Parsing
    for a significant performance boost.

  * Improved performance of all readers by rewriting parsers.

  * Simplified Text.Pandoc.CharacterReferences by using
    entity lookup functions from TagSoup.

  * `Text.Pandoc.UTF8` now uses the unicode-aware IO functions
    from `System.IO` if base >= 4.2.  This gives support for
    windows line endings on windows.

  * Remove duplications in documentation by generating the
    pandoc man page from README, using `MakeManPage.hs`.

  * README now includes a full description of markdown syntax,
    including non-pandoc-specific parts.  A new `pandoc_markdown`
    man page is extracted from this, so you can look up markdown
    syntax by doing `man pandoc_markdown`.

  * Completely revised test framework (with help from Nathan Gass).
    The new test framework is built when the `tests` Cabal flag is set. It
    includes the old integration tests, but also some new unit and quickcheck
    tests. Test output has been much improved, and you can now specify a glob
    pattern after `cabal test` to indicate which tests should be run;
    for example `cabal test citations` will run all the citation tests.

  * Added a shell script, `stripansi.sh`, for filtering ANSI control
    sequences from test output: `cabal test | ./stripansi.sh > test.log`.

  * Added `Interact.hs` to make it easier to use ghci while developing.
    `Interact.hs` loads `ghci` from the `src` directory, specifying
    all the options needed to load pandoc modules (including
    specific package dependencies, which it gets by parsing
    dist/setup-config).

  * Added `Benchmark.hs`, testing all readers + writers using criterion.

  * Added `stats.sh`, to make it easier to collect and archive
    benchmark and lines-of-code stats.

  * Added upper bounds to all cabal dependencies.

  * Include man pages in extra-source-files.  This allows users to
    install pandoc from the tarball without needing to build the man
    pages.

### Bug fixes

  * Filenames are encoded as UTF8.  Resolves Issue #252.

  * Handle curly quotes better in `--smart` mode. Previously, curly quotes
    were just parsed literally, leading to problems in some output formats.
    Now they are parsed as `Quoted` inlines, if `--smart` is specified.
    Resolves Issue #270.

  * Text.Pandoc.Parsing: Fixed bug in grid table parser.
    Spaces at end of line were not being stripped properly,
    resulting in unintended LineBreaks.

  * Markdown reader:

    + Allow HTML comments as inline elements in markdown.
      So, `aaa <!-- comment --> bbb` can be a single paragraph.
    + Fixed superscripts with links: `^[link](/foo)^` gets
      recognized as a superscripted link, not an inline note followed by
      garbage.
    + Fixed regression, making markdown reference keys case-insensitive again.
      Resolves Issue #272.
    + Properly handle abbreviations (like `Mr.`) at the end of a line.
    + Better handling of intraword underscores, avoiding exponential
      slowdowns in some cases.  Resolves Issue #182.
    + Fixed bug in alignments in tables with blank rows in the header.

  * RST reader:

    + Field lists now allow spaces in field names, and
      block content in field values. (Thanks to Lachlan Musicman
      for pointing out the bug.)
    + Definition list items are now always `Para` instead of
      `Plain`, matching behavior of `rst2xml.py`.
    + In image blocks, the description is parsed properly and
      used for the alt attribute, not also the title.
    + Skip blank lines at beginning of file. Resolves
      Debian #611328.

  * LaTeX reader:

    + Improved parsing of preamble.
      Previously you'd get unexpected behavior on a document that
      contained `\begin{document}` in, say, a verbatim block.
    + Allow spaces between `\begin` or `\end` and `{`.
    + Support `\L` and `\l`.
    + Skip comments inside paragraphs.

  * LaTeX writer:

    + Escape strings in `\href{..}`.
    + In nonsimple tables, put cells in `\parbox`.

  * OpenDocument writer:  don't print raw TeX.

  * Markdown writer:

    + Fixed bug in `Image`.  URI was getting unescaped twice!
    + Avoid printing extra blank lines at the end if there are
      no notes or references.

  * LaTeX and ConTeXt: Escape `[` and `]` as `{[}` and `{]}`.
    This avoids unwanted interpretation as an optional argument.

  * ConTeXt writer: Fixed problem with inline code.  Previously
    `}` would be rendered `\type{}}`. Now we check the string for '}' and '{'.
    If it contains neither, use `\type{}`; otherwise use `\mono{}`
    with an escaped version of the string.

  * `:` now allowed in HTML tags. Resolves Issue #274.



## pandoc  1.6 (2010-07-24)

+ New EPUB and HTML Slidy writers. (Issue #122)

    - [EPUB] is a standard ebook format, used in Apple's iBooks for
      the iPad and iPhone, Barnes and Noble's nook reader, the Sony
      reader, and many other devices, and by online ebook readers like
      [bookworm]. (Amazon's Kindle uses a different format, MobiPocket,
      but EPUB books can easily be converted to Kindle format.) Now you
      can write your book in markdown and produce an ebook with a single
      command! I've put up a short [tutorial here].
    - [Slidy], like S5, is a system for producing HTML+javascript slide shows.

+ All input is assumed to be UTF-8, no matter what the locale and ghc
  version, and all output is UTF-8. This reverts to pre-1.5 behavior.
  Also, a BOM, if present, is stripped from the input.

+ Markdown now supports grid tables, whose cells can contain
  arbitrary block elements. (Issue #43)

+ Sequentially numbered example lists in markdown with `@` marker.

+ Markdown table captions can begin with a bare colon and no longer need
  to include the English word "table." Also, a caption can now occur
  either before or after the table. (Issue #227)

+ New command-line options:

    - `--epub-stylesheet` allows you to specify a CSS file that will
      be used to style your ebook.
    - `--epub-metadata` allows you to specify metadata for the ebook.
    - `--offline` causes the generated HTML slideshow to include all
      needed scripts and stylesheets.
    - `--webtex` causes TeX math to be converted to images using the
      Google Charts API (unless a different URL is specified).
    - `--section-divs` causes div tags to be added around each section
      in an HTML document. (Issue #230, 239)

+ Default behavior of S5 writer in standalone mode has changed:
  previously, it would include all needed scripts and stylesheets
  in the generated HTML; now, only links are included unless
  the `--offline` option is used.

+ Default behavior of HTML writer has changed. Between 1.2 and 1.5,
  pandoc would enclose sections in div tags with identifiers on the
  div tags, so that the sections can be manipulated in javascript.
  This caused undesirable interactions with raw HTML div tags. So,
  starting with 1.6, the default is to put the identifiers directly
  on the header tags, and not to include the divs.  The `--section-divs`
  option selects the 1.2-1.5 behavior.

+ API changes:

    - `HTMLMathMethod`: Added `WebTeX`, removed `MimeTeX`.
    - `WriterOptions`: Added `writerUserDataDir`, `writerSourceDirectory`,
      `writerEPUBMetadata` fields. Removed `writerIncludeBefore`,
      `writerIncludeAfter`.
    - Added `headerShift` to `Text.Pandoc.Shared`.
    - Moved parsing code and `ParserState` from `Text.Pandoc.Shared`
      to a new module, `Text.Pandoc.Parsing`.
    - Added `stateHasChapters` to `ParserState`.
    - Added `HTMLSlideVariant`.
    - Made `KeyTable` a map instead of an association list.
    - Added accessors for `Meta` fields (`docTitle`, `docAuthors`, `docDate`).
    - `Pandoc`, `Meta`, `Inline`, and `Block` have been given `Ord` instances.
    - Reference keys now have a type of their own (`Key`), with its
      own `Ord` instance for case-insensitive comparison.
    - Added `Text.Pandoc.Writers.EPUB`.
    - Added `Text.Pandoc.UUID`.
    - Removed `Text.Pandoc.ODT`, added `Text.Pandoc.Writers.ODT`.
      Removed `saveOpenDocumentAsODT`, added `writeODT`.
    - Added `Text.Pandoc.Writers.Native` and `writeNative`.
      Removed `prettyPandoc`.
    - Added `Text.Pandoc.UTF8` for portable UTF8 string IO.
    - Removed `Text.Pandoc.Writers.S5` and the `writeS5` function.
      Moved `s5Includes` to a new module, `Text.Pandoc.S5`.
      To write S5, you now use `writeHtml` with `writerSlideVariant`
      set to `S5Slides` or `SlidySlides`.

+ Template changes.  If you use custom templates, please update them,
  particularly if you use syntax highlighting with pandoc. The old HTML
  templates hardcoded highlighting CSS that will no longer work with
  the most recent version of highlighting-kate.

    - HTML template: avoid empty meta tag if no date.
    - HTML template: Use default highlighting CSS from highlighting-kate
      instead of hard-coding the CSS into the template.
    - HTML template: insert-before text goes before the title, and
      immediately after the `<body>` tag, as documented. (Issue #241)
    - Added slidy and s5 templates.
    - Added amssymb to preamble of latex template. (github Issue 1)

+ Removed excess newlines at the end of output. Note: because output
  will not contain an extra newline, you may need to make adjustments
  if you are inserting pandoc's output into a template.

+ In S5 and slidy, horizontal rules now cause a new slide, so you
  are no longer limited to one slide per section.

+ Improved handling of code in man writer. Inline code is now monospace,
  not bold, and code blocks now use .nf (no fill) and .IP (indented para).

+ HTML reader parses `<tt>` as Code. (Issue #247)

+ html+lhs output now contains bird tracks, even when compiled without
  highlighting support. (Issue #242)

+ Colons are now no longer allowed in autogenerated XML/HTML identifiers,
  since they have a special meaning in XML.

+ Code improvements in ODT writer.  Remote images are now replaced with
  their alt text rather than a broken link.

+ LaTeX reader improvements:

    - Made latex `\section`, `\chapter` parsers more forgiving of whitespace.
    - Parse `\chapter{}` in latex.
    - Changed `rawLaTeXInline` to accept `\section`, `\begin`, etc.
    - Use new `rawLaTeXInline'` in LaTeX reader, and export `rawLaTeXInline`
      for use in markdown reader.
    - Fixes bug wherein `\section{foo}` was not recognized as raw TeX
      in markdown document.

+ LaTeX writer:  images are automatically shrunk if they would extend
  beyond the page margin.

+ Plain, markdown, RST writers now use unicode for smart punctuation.

+ Man writer converts math to unicode when possible, as in other writers.

+ `markdown2pdf` can now recognize citeproc options.

+ Command-line arguments are converted to UTF-8. (Issue #234)

+ `Text.Pandoc.TeXMath` has been rewritten to use texmath's parser.
  This allows it to handle a wider range of formulas. Also, if a formula
  cannot be converted, it is left in raw TeX; formulas are no longer
  partially converted.

+ Unicode curly quotes are left alone when parsing smart quotes. (Issue #143)

+ Cabal file changes:

    - Removed parsec < 3 restriction.
    - Added 'threaded' flag for architectures where GHC lacks a threaded
      runtime.
    - Use 'threaded' only for markdown2pdf; it is not needed for pandoc.
    - Require highlighting-kate 0.2.7.

+ Use explicit imports from `Data.Generics`. Otherwise we have a
  conflict with the 'empty' symbol, introduced in syb >= 0.2. (Issue #237)

+ New data files:  slidy/slidy.min.js, slidy/slidy.min.css, epub.css.

[EPUB]: http://en.wikipedia.org/wiki/EPUB
[Slidy]: http://www.w3.org/Talks/Tools/Slidy
[bookworm]: http://bookworm.oreilly.com/
[tutorial here]: http://johnmacfarlane.net/pandoc/epub.html

## pandoc  1.5.1.1 (2010-03-29)

+ Fixed header identifiers (uniqueIdent in Shared) so they
  work as advertized in README and are guaranteed to be
  valid XHTML names. Thanks to Xyne for reporting the bug.

## pandoc  1.5.1 (2010-03-23)

+ Fixed treatment of unicode characters in URIs.
+ Revised Setup.hs so it works with debian's build process.
+ Fixed bug in OpenDocument writer that led to invalid
  XML for some input.

## pandoc  1.5.0.1 (2010-03-21)

+ HTML writer: Fixed error in math writer (with MathML option)
  that caused an infinite loop for unparsable MathML.

## pandoc  1.5 (2010-03-20)

+ Moved repository to [github](http://github.com/jgm/pandoc).
+ New `--mathml` option, for display of TeX math as MathML.
+ New `--data-dir` option, allowing users to specify a data
  directory other than `~/.pandoc`.  Files placed in this directory
  will be used instead of system defaults.
+ New `--base-header-level` option. For example, `--base-header-level=2`
  changes level 1 headers to level 2, level 2 to level 3, etc.
+ New 'plain' output format: plain text without pictures, hyperlinks,
  inline formatting, or anything else that looks even vaguely
  markupish.
+ Titles and authors in title blocks can now span multiple lines,
  as long as the continuation lines begin with a space character.
+ When given an absolute URI as a parameter, pandoc will fetch
  the content via HTTP.
+ The HTML reader has been made much more forgiving. It no
  longer requires well-formed xhtml as input.
+ `html2markdown` has been removed; it is no longer necessary, given
  the last two changes. `pandoc` can be used by itself to convert
  web pages to markdown or other formats.
+ `hsmarkdown` has also been removed.  Use `pandoc --strict` instead.
  Or symlink pandoc's executable to `hsmarkdown`; `pandoc` will then
  behave like `hsmarkdown` used to.
+ An image in a paragraph by itself is now rendered as a figure
  in most writers, with the alt text as the caption.
+ Incomplete support for reST tables (simple and grid). Thanks to
  Eric Kow. Colspans and rowspans not yet supported.
+ In mediawiki, links with relative URLs are now formatted as wikilinks.
  Also, headers have been promoted: `= head =` is now level 1 instead of
  level 2.
+ The markdown reader now handles "inverse bird tracks" when parsing
  literate haskell.  These are used for haskell example code that
  is not part of the literate program.
+ The `-B` and `-A` options now imply `-s` and no longer work in
  fragment mode.
+ Headerless tables are now printed properly in all writers.
  In addition, tbody, thead, and cols are used in HTML and Docbook
  tables.
+ Improved build system; removed obsolete Makefile.
+ In LaTeX writer, `\chapter` is now used instead of `\section`.
  when the documentclass is book, report, or memoir.
+ Many small bug fixes. See [changelog] for details.

## pandoc  1.4 (2010-01-02)

+ New template system replaces old headers, giving users much
  more control over pandoc's output in `--standalone` mode.
  Added `--template` and `--variable` options.  The `--print-default-header`
  option is now `--print-default-template`.  See README under
  "Templates" for details.
+ The old `--custom-header` option should still work, but it has
  been deprecated.
+ New `--reference-odt` option allows users to customize styles
  in ODT output.
+ Users may now put custom templates, s5 styles, and a reference
  ODT in the `~/.pandoc` directory, where they will override system
  defaults. See README for details.
+ Unicode is now used whenever possible in HTML and XML output. Entities
  are used only where necessary (`&gt;`, `&lt;`, `&quot;`, `&amp;`).
+ Authors and dates may now include formatting and notes.
+ Added `--xetex` option for `pandoc` and `markdown2pdf`.
+ Windows installer now includes highlighting support and
  `markdown2pdf` and `hsmarkdown` wrappers.
+ Pandoc no longer requires Template Haskell, which should make
  it more portable.
+ Pandoc can now be built on GHC 6.12, as well as earlier versions.
+ See README for other small improvements and bug fixes.

## pandoc  1.3 (2009-12-10)

+ Added `--id-prefix` option to help prevent duplicate
  identifiers when you're generating HTML fragments.
+ Added `--indented-code-classes` option, which specifies
  default highlighting syntax for indented code blocks.
+ `--number-sections` now affects HTML output.
+ Improved syntax for markdown definition lists.
+ Better looking simple tables.
+ Markdown tables without headers are now possible.
+ New hard line break syntax:  backslash followed by newline.
+ Improved performance of markdown reader by ~10% by eliminating the
  need for a separate parsing pass for notes.
+ Improved syntax highlighting for literate Haskell.
+ Support for "..code-block" directive in RST reader.
+ Windows binary now includes highlighting support.
+ Many bug fixes and small improvements. See [changelog]
  for details.

## pandoc  1.2.1 (2009-07-18)

+ Improved the efficiency of the markdown reader's
  abbreviation parsing (should give a big performance
  boost with `--smart`).
+ HTML writer now wraps sections in divs with unique
  identifiers, for easier manipulation.
+ Improved LaTeX reader's coverage of math modes.
+ Added a portable Haskell version of markdown2pdf (thanks
  to Paolo Tanimoto).
+ Made `--strict` compatible with `--standalone` and `--toc.`
+ Many other small improvements and bug fixes.
  See [changelog] for details.

## pandoc  1.2 (2009-03-01)

+ Added support for literate Haskell. lhs support is triggered by
  '+lhs' suffixes in formats. For example, 'latex+lhs' is literate
  Haskell LaTeX. '.lhs' files are treated by default as literate
  markdown.
+ Added `--email-obfuscation` option.
+ Brought citeproc support up to date for citeproc-hs-0.2.
+ Many bugs fixed. See [changelog] for details.

## pandoc  1.1 (2008-11-06)

+ New `--jsmath` option supporting use of pandoc with [jsMath].
+ Classes on HTML table output for better CSS styling.
+ Windows installer no longer requires admin privileges.
+ Many bugs fixed.  See [changelog] for details.

## pandoc  1.0 (2008-09-13)

+ New writers for MediaWiki, GNU Texinfo (thanks to Peter Wang),
  OpenDocument XML (thanks to Andrea Rossato), and ODT (OpenOffice
  document).
+ New [delimited code blocks](README.html#delimited-code-blocks),
  with optional syntax highlighting.
+ Reorganized build system:  pandoc can now be built using standard
  Cabal tools.  It can be compiled on Windows without Cygwin.
  The tests can also be run without perl or unix tools.
+ LaTeXMathML replaces ASCIIMathML for rendering math in HTML.
+ Support for "displayed" math.
+ Common abbreviations are now handled more intelligently, with
  a non-breaking space (and not a sentence-ending space) after
  the period.
+ Code is -Wall clean.
+ Many bug fixes and small improvements.  See [changelog] for
  full details.

## pandoc  0.46 (2008-01-08)

+ Added a `--sanitize-html` option (and a corresponding parameter
  in `ParserState` for those using the pandoc libraries in programs).
  This option causes pandoc to sanitize HTML (in HTML or Markdown
  input) using a whitelist method. Possibly harmful HTML elements
  are replaced with HTML comments. This should be useful in the
  context of web applications, where pandoc may be used to convert
  user input into HTML.
+ Made -H, -A, and -B options cumulative: if they are specified
  multiple times, multiple files will be included.
+ Many bug fixes and small improvements.  See [changelog] for full
  details.

## pandoc  0.45 (2007-12-09)

+ Many bug fixes and structural improvements.  See [changelog] for
  full details.
+ Improved treatment of math. Math is now rendered using unicode
  by default in HTML, RTF, and DocBook output. For more accurate
  display of math in HTML, `--gladtex`, `--mimetex`, and `--asciimathml`
  options are provided. See the [User's Guide](README.html#math) for
  details.
+ Removed support for box-style block quotes in markdown.
+ More idiomatic ConTeXt output.
+ Text wrapping in ConTeXt and LaTeX output.
+ Pandoc now correctly handles all standard line endings
  (CR, LF, CRLF).
+ New `--no-wrap` option that disables line wrapping and minimizes
  whitespace in HTML output.
+ Build process is now compatible with both GHC 6.8 and GHC 6.6.
  GHC and GHC_PKG environment variables may be used to specify
  which version of the compiler to use, when multiple versions are
  installed.

## pandoc 0.44 (2007-09-03)

  [ John MacFarlane ]

  * Fixed bug in HTML writer:  when --toc was used, anchors were put around
    headers, which is invalid XHTML (block content within inline element).
    Now the anchors are put inside the header tags.  Resolves Issue #23.

  * Added xmlns attribute to html element in html writer tests.
    This attribute is added by more recent versions of the
    xhtml library (>= 3000), and is required for valid XHTML.

  [ Recai Oktaş ]

  * On configure, compile 'Setup.hs' to 'setup' and use 'setup' as the build
    command instead of 'runhaskell', which, on some platforms (such as s390,
    alpha, m68k), throws the following error:

        runhaskell Setup.hs configure --prefix=/usr
        ghc-6.6.1: not built for interactive use

    This causes a serious FTBFS bug.  Closes: #440668.

## pandoc 0.43 (2007-09-02)

  [ John MacFarlane ]

  * The focus of this release is performance.  The markdown parser
    is about five times faster than in 0.42, based on benchmarks
    with the TextMate manual.

  * Main.hs: Replaced CRFilter and tabFilter with single function
    tabFilter, which operates on the whole string rather than breaking
    it into lines, and handles dos-style line-endings as well as tabs.

  * Added separate LaTeX reader and native reader tests; removed
    round-trip tests.

  * Text.Pandoc.Shared:

    + Removed tabsToSpaces and tabsInLine (they were used only in Main.hs.)
    + General code cleanup (to elimante warnings when compiling with -Wall.)
    + Added 'wrapped' function, which helps wrap text into paragraphs,
      using the prettyprinting library.
    + Rewrote charsInBalanced and charsInBalanced'.
        - Documented restriction: open and close must be distinct characters.
        - Rearranged options for greater efficiency.
        - Bug fix: Changed inner call to charsInBalanced inside
          charsInBalanced' to charsInBalanced'.
    + anyLine now requires that the line end with a newline (not eof).
      This is a harmless assumption, since we always add newlines to the
      end of a block before parsing with anyLine, and it yields a 10% speed
      boost.
    + Removed unnecessary 'try' in anyLine.
    + Removed unneeded 'try' from romanNumeral parser.
    + Use notFollowedBy instead of notFollowedBy' in charsInBalanced.
    + Removed unneeded 'try' in parseFromString.
    + Removed unneeded 'try' from stringAnyCase.  (Now it behaves
      like 'string'.)
    + Changed definition of 'enclosed' in Text.Pandoc.Shared so that
      'try' is not automatically applied to the 'end' parser. Added
      'try' in calls to 'enclosed' where needed. Slight speed increase.

  * Writers:

    + Replaced individual wrapping routines in RST, Man, and Markdown
      writers with 'wrapped' from Text.Pandoc.Shared.
    + Rewrote LaTeX writer to use the prettyprinting library,
      so we get word wrapping, etc.
    + Modified latex writer tests for new latex writer using prettyprinter.
    + Fixed bug in LaTeX writer: autolinks would not cause
      `\usepackage{url}` to be put in the document header. Also, changes
      to state in enumerated list items would be overwritten.
    + In Markdown writer, escape paragraphs that begin with ordered list
      markers, so they don't get interpreted as ordered lists.

  * Text.Pandoc.Reades.LaTeX:

    + Fixed bug in LaTeX reader, which wrongly assumed that the roman
      numeral after "enum" in "setcounter" would consist entirely of
      "i"s. 'enumiv' is legitimate.
    + LaTeX command and environment names can't contain numbers.
    + Rearranged order of parsers in inline for slight speed improvement.
    + Added '`' to special characters and 'unescapedChar'.

  * Text.Pandoc.Readers.RST:

    + Removed unneeded try's in RST reader; also minor code cleanup.
    + Removed tabchar.
    + Rearranged parsers in inline (doubled speed).

  * Text.Pandoc.Readers.Markdown:

    + Skip notes parsing if running in strict mode. (This yields a nice
      speed improvement in strict mode.)
    + Simplify autolink parsing code, using Network.URI to test for
      URIs. Added dependency on network library to debian/control and
      pandoc.cabal.
    + More perspicuous definition of nonindentSpaces.
    + Removed unneeded 'try' in 'rawLine'.
    + Combined linebreak and whitespace into a new whitespace parser, to
      avoid unnecessary reparsing of space characters.
    + Removed unnecessary 'try' in 'codeBlock', 'ellipses', 'noteMarker',
      'multilineRow', 'dashedLine', 'rawHtmlBlocks'.
    + Use lookAhead in parsers for setext headers and definition lists
      to see if the next line begins appropriately; if not, don't waste
      any more time parsing.
    + Don't require blank lines after code block. (It's sufficient to
      end code block with a nonindented line.)
    + Changed definition of 'emph': italics with '_' must not
      be followed by an alphanumeric character. This is to help
      prevent interpretation of e.g. `[LC_TYPE]: my_type` as
      `[LC<em>TYPE]:my</em>type`.
    + Improved Markdown.pl-compatibility in referenceLink: the two parts
      of a reference-style link may be separated by one space, but not
      more... [a] [link], [not]    [a link].
    + Fixed markdown inline code parsing so it better accords with
      Markdown.pl: the marker for the end of the code section is a clump
      of the same number of `'s with which the section began, followed
      by a non-` character. So, for example,
         ` h ``` i ` -> `<code>h ``` i</code>`.
    + Split 'title' into 'linkTitle' and 'referenceTitle', since the
      rules are slightly different.
    + Rewrote 'para' for greater efficiency.
    + Rewrote link parsers for greater efficiency.
    + Removed redundant 'referenceLink' in definition of inline (it's
      already in 'link').
    + Refactored escapeChar so it doesn't need 'try'.
    + Refactored hrule for performance in Markdown reader.
    + More intelligent rearranging of 'inline' so that most frequently
      used parsers are tried first.
    + Removed tabchar parser, as whitespace handles tabs anyway.

  * Text.Pandoc.CharacterReferences:

    + Refactored.
    + Removed unnecessary 'try's for a speed improvement.
    + Removed unnecessary '&' and ';' from the entity table.

  * Build process:

    + Makefile: Get VERSION from cabal file, not Main.hs.
    + Modified MacPorts Portfile:
        - Depend on haddock
        - Build and install libraries and library documentation in
          addition to pandoc executable
        - Added template item for md5 sum in Portfile.in.
        - Incorporated changes from MacPorts repository (r28278).
    + FreeBSD port:  Don't try to generate distinfo in Makefile.
      It can be made using 'make makesum' in FreeBSD.
    + Make both freebsd and macports targets depend on tarball.

  * Website and documentation:

    + Updated INSTALL instructions.
    + Added pandocwiki demo to website.
    + Removed local references to Portfile, since pandoc is now in the
      MacPorts repository.

## pandoc 0.42 (2007-08-26)

  [ John MacFarlane ]

  * Main.hs: Use utf8 conversion on the extra files loaded with
    the -H, -C, -B, and -A options.  This fixes problems with unicode
    characters in these files.

  * Exposed Text.Pandoc.ASCIIMathML, since it is imported in
    Text.Pandoc.Readers.HTML and without it we get a linking error when
    using the library.

  * Markdown reader:

    + Added new rule for enhanced markdown ordered lists: if the list
      marker is a capital letter followed by a period (including a
      single-letter capital roman numeral), then it must be followed by
      at least two spaces. The point of this is to avoid accidentally
      treating people's initials as list markers: a paragraph might begin,
      "B. Russell was an English philosopher," and this shouldn't be
      treated as a list.  Documented change in README.
    + Blocks that start with "p. " and a digit are no longer treated
      as ordered lists (it's a page number).
    + Added a needed 'try' to listItem.
    + Removed check for a following setext header in endline.
      A full test is too inefficient (doubles benchmark time), and the
      substitute we had before is not 100% accurate.
    + Don't use Code elements for autolinks if --strict specified.

  * LaTeX writer:  When a footnote ends with a Verbatim environment, the
    close } of the footnote cannot occur on the same line or an error occurs.
    Fixed this by adding a newline before the closing } of every footnote.

  * HTML writer:
    + Removed incorrect "{}" around style information in HTML tables.
      Column widths now work properly in HTML.
    + If --strict option is specified (and --toc is not), don't include
      identifiers in headers, for better Markdown compatibility.

  * Build process:

    + Separated $(web_dest) and website targets.
    + In website, index.txt is now constructed from template index.txt.in.
    + Added freebsd target to Markefile. This creates the freebsd Makefile
      from Makefile.in, and creates distinfo.  Removed Makefile and distinfo
      from the repository.
    + Added macport target to Makefile. Portfile is built from template
      Portfile.in.
    + Removed OSX package targets.  (Too many difficulties involving
      dependencies on dynamic libraries.)
    + More complete INSTALL instructions for all architectures.

  * Website:
    + Added a programming demo, pandocwiki.

  [ Recai Oktaş ]

  * Do not forget to close pandoc's ITP.  Closes: #391666

## pandoc 0.41 (2007-08-19)

  [ John MacFarlane ]

  * Fixed bugs in HTML reader:
    + Skip material at end *only if* `</html>` is present (previously,
      only part of the document would be parsed if an error was
      found; now a proper error message is given).
    + Added new constant eitherBlockOrInline with elements that may
      count either as block-level or as inline. Modified isInline and
      isBlock to take this into account.
    + Modified rawHtmlBlock to accept any tag (even an inline tag):
      this is innocuous, because rawHtmlBlock is tried only if a regular
      inline element can't be parsed.
    + Added a necessary 'try' in definition of 'para'.

  * Fixed bug in markdown ordered list parsing.  The problem was that
    anyOrderedListStart did not check for a space following the
    ordered list marker.  So in 'A.B. 2007' the parser would be
    expecting a list item, but would not find one, causing an error.
    Fixed a similar bug in the RST reader.  Resolves Issue #22.

  * Refactored RST and Markdown readers using parseFromString.

  * LaTeX reader will now skip anything after \end{document}.

  * Fixed blockquote output in markdown writer: previously, block
    quotes in indented contexts would be indented only in the first
    line.

  * Added note to INSTALL about variations in versions of the xhtml
    library that can lead to failed tests (thanks to Leif LeBaron).

## pandoc 0.4 (2007-01-16)

  [ John MacFarlane ]

  * Added two new output formats: groff man pages and ConTeXt. By
    default, output files with extensions ".ctx" and ".context" are
    assumed to be ConTeXt, and output files with single-digit extensions
    are assumed to be man pages.

  * Enhanced ordered lists (documented in README, under Lists):
    + The OrderedList block element now stores information about
      list number style, list number delimiter, and starting number.
    + The readers parse this information when possible.
    + The writers use this information to style ordered lists.
    + The enhancement can be disabled using the --strict option.

  * Added support for tables (with a new Table block element). Two kinds
    of tables are supported: a simple table with one-line rows, and a
    more complex variety with multiline rows. All output formats are
    supported, but only markdown tables are parsed at the moment. The
    syntax is documented in README.

  * Added support for definition lists (with a new DefinitionList block
    element). All output and input formats are supported. The syntax is
    documented in README.

  * Added support for superscripts and subscripts (with new Superscript
    and Subscript inline elements).  All input and output
    formats.  The syntax is documented in README.

  * Added support for strikeout (with a new Strikeout inline element).
    All input and output formats are supported. Thanks to Bradley Kuhn,
    who contributed a patch. The syntax is documented in README. Resolves
    Issue #18.

  * Added a --toc|--table-of-contents option.  This causes an automatically
    generated table of contents (or an instruction that creates one) to
    be inserted at the beginning of the document. Not supported in S5,
    DocBook, or man page writers.

  * Modified the -m|--asciimathml option:

    + If an optional URL argument is provided, a link is inserted
      instead of the contents of the ASCIIMathML.js script.
    + Nothing is inserted unless the document actually contains
      LaTeX math.

  * Removed Blank block element as unnecessary.

  * Removed Key and Note blocks from the Pandoc data structure. All
    links are now stored as explicit links, and note contents are
    stored with the (inline) notes.

    + All link Targets are now explicit (URL, title) pairs; there
      is no longer a 'Ref' target.
    + Markdown and RST parsers now need to extract data from key and
      note blocks and insert them into the relevant inline elements.
      Other parsers have been simplified, since there is no longer any need
      to construct separate key and note blocks.
    + Markdown, RST, and HTML writers need to construct lists of
      notes; Markdown and RST writers need to construct lists of link
      references (when the --reference-links option is specified); and
      the RST writer needs to construct a list of image substitution
      references. All writers have been rewritten to use the State monad
      when state is required.
    + Several functions (generateReference, keyTable,
      replaceReferenceLinks, replaceRefLinksBlockList, and some auxiliaries
      used by them) have been removed from Text.Pandoc.Shared, since
      they are no longer needed. New functions and data structures
      (Reference, isNoteBlock, isKeyBlock, isLineClump) have been
      added. The functions inTags, selfClosingTag, inTagsSimple, and
      inTagsIndented have been moved to the DocBook writer, since that
      is now the only module that uses them. NoteTable is now exported
      in Text.Pandoc.Shared.
    + Added stateKeys and stateNotes to ParserState; removed stateKeyBlocks,
      stateKeysUsed, stateNoteBlocks, stateNoteIdentifiers, stateInlineLinks.
    + Added writerNotes and writerReferenceLinks to WriterOptions.

  * Added Text.Pandoc module that exports basic readers, writers,
    definitions, and utility functions. This should export everything
    needed for most uses of Pandoc libraries. The haddock documentation
    includes a short example program.

  * Text.Pandoc.ASCIIMathML is no longer an exported module.

  * Added Text.Pandoc.Blocks module to help in printing markdown
    and RST tables.  This module provides functions for working with
    fixed-width blocks of text--e.g., placing them side by side, as
    in a table row.

  * Refactored to avoid reliance on Haskell's Text.Regex library, which
    (a) is slow, and (b) does not properly handle unicode.  This fixed
    some strange bugs, e.g. in parsing S-cedilla, and improved performance.

    + Replaced 'gsub' with a general list function  'substitute'
      that does not rely on Text.Regex.
    + Rewrote extractTagType in HTML reader so that it doesn't use
      regexs.
    + In Markdown reader, replaced email regex test with a custom email
      autolink parser (autoLinkEmail). Also replaced selfClosingTag regex
      with a custom function isSelfClosingTag.
    + Modified Docbook writer so that it doesn't rely on Text.Regex for
      detecting 'mailto' links.
    + Removed escapePreservingRegex and reamped entity-handling
      functions in Text.Pandoc.Shared and Text.Pandoc.CharacterReferences to
      avoid reliance on Text.Regex (see below on character reference
      handling changes).

  * Renamed Text.Pandoc.Entities as Text.Pandoc.CharacterReferences.

  * Changed handling of XML entities.  Entities are now parsed (and unicode
    characters returned) in the Markdown and HTML readers, rather than being
    handled in the writers.  In HTML and Docbook writers, UTF-8 is now used
    instead of entities for characters above 128.  This makes the HTML and
    DocBook output much more readable and more easily editable.

    + Removed sgmlHexEntity, sgmlDecimalEntity, sgmlNamedEntity, and
      sgmlCharacterEntity regexes from Text.Pandoc.Shared.
    + Renamed escapeSGMLChar to escapeCharForXML.  Added escapeStringForXML.
      Moved both functions to Text.Pandoc.Writers.Docbook.
    + Added characterReference parser to Text.Pandoc.CharacterReferences.
      This parses a string and return a unicode character.
    + Rewrote decodeCharacterReferences to use the new parser instead of
      Text.Regex.
    + Added new charRef parser for Markdown and HTML, which replaces the
      old 'entity' parser. Added '&' as a special character in Markdown reader.
    + Modified HTML and Markdown readers to call decodeEntities on all raw
      strings (e.g. authors, dates, link titles), to ensure that no
      unprocessed entities are included in the native representation of
      the document.  (In the HTML reader, most of this work is done by a
      change in extractAttributeName.)
    + In XML and Markdown output, escape unicode nonbreaking space as '&nbsp;',
      since a unicode non-breaking space is impossible to distinguish visually
      from a regular space.  (Resolves Issue #3.)
    + Removed encodeEntitiesNumerical.
    + Use Data.Map for entityTable and (new) reverseEntityTable, for a
      slight performance boost over the old association list.
    + Removed unneeded decodeEntities from 'str' parser in HTML and
      Markdown readers.

  * Text.Pandoc.UTF8:  Renamed encodeUTF8 to toUTF8, decodeUTF8 to
    fromUTF8, for clarity.

  * Replaced old haskell98 module names replaced by hierarchical module
    names, e.g. List by Data.List.  Removed haskell98 from dependencies
    in pandoc.cabal, and added mtl (needed for state monad). Substituted
    xhtml for html.

  * Refactored and cleaned up character escaping in writers, using
    backslashEscapes and escapeStringUsing functions.

  * Instead of adding `\n\n` to the end of an input string in Main.hs,
    this is now done in the readers. This makes the libraries behave
    the way you'd expect from the pandoc program. Resolves Issue #10.

  * URLs and email addresses in autolinks are now typeset as Code.

  * In Main.hs, changed putStr to putStrLn -- mainly because MacOS X
    doesn't display the whole output unless there's a line ending.

  * Major code cleanup in all modules, for greater consistency, concision,
    and readability.

  * HTML reader:

    + Fixed several bugs (extractTagType, attribute parsing).
    + Remove Null blocks in lists of blocks when possible.
    + Allow HTML comments as raw HTML inline.

  * Markdown reader:

    + Ordered list items may no longer begin with uppercase letters, or
      letters greater than 'n'.  (This prevents first initials and page
      reference, e.g. 'p. 400', from being parsed as beginning lists.)
      Also, numbers beginning list items may no longer end with ')',
      which is now allowed only after letters.  Note: These changes
      may cause documents to be parsed differently. Users should take
      care in upgrading.
    + Changed autoLink parsing to conform better to Markdown.pl's
      behavior. `<google.com>` is not treated as a link, but
      `<http://google.com>`, `<ftp://google.com>`, and
      `<mailto:google@google.com>` are.
    + Cleaned up handling of embedded quotes in link titles.  Now these are
      stored as a '"' character, not as '&quot;'.
    + Use lookAhead parser for the 'first pass' (looking for reference keys),
      instead of parsing normally, then using setInput to reset input.  This
      yields a slight performance boost.
    + Fixed several bugs in smart quote recognition.
    + Fixed bug in indentSpaces (which didn't properly handle
      cases with mixed spaces and tabs).
    + Consolidated 'text', 'special', and 'inline' into 'inline'.
    + Fixed bug which allowed URL and title to be separated by multiple blank
      lines in links and reference keys.  They can be on separate lines but
      can't have blank lines between them.
    + Correctly handle bracketed text inside inline footnotes and links,using
      new function inlinesInBalanced.  Resolves Issue #14.
    + Fixed bug in footnotes: links in footnotes were not being
      processed. Solution: three-stage parse. First, get all the
      reference keys and add information to state. Next, get all the
      notes and add information to state. (Reference keys may be needed
      at this stage.) Finally, parse everything else.
    + Replaced named constants like 'emphStart' with literals.
    + Removed an extra occurrence of escapedChar in definition of inline.

  * RST reader:

    + Allow the URI in a RST hyperlink target to start on the line
      after the reference key.
    + Added 'try' in front of 'string', where needed, or used a different
      parser.  This fixes a bug where ````` would not be correctly parsed as
      a verbatim `.
    + Fixed slow performance in parsing inline literals in RST reader.  The
      problem was that ``#`` was seen by 'inline' as a potential link or image.
      Fix:  inserted 'notFollowedBy (char '`')' in link parsers.
      Resolves Issue #8.
    + Use lookAhead instead of getInput/setInput in RST reader.  Removed
      unneeded getState call, since lookAhead automatically saves and
      restores the parser state.
    + Allow hyperlink target URIs to be split over multiple lines, and
      to start on the line after the reference. Resolves Issue #7.
    + Fixed handling of autolinks.

  * LaTeX reader:

    + Replaced `choice [(try (string ...), ...]` idiom with `oneOfStrings`,
      for clarity.
    + Added clauses for tilde and caret. Tilde is `\ensuremath{\sim}`, and
      caret is `\^{}`, not `\^` as before.
    + Added parsing for `\url`.
    + Parse `\texttt{}` as code, provided there's nothing fancy inside.

  * HTML writer:

    + Modified HTML writer to use the Text.XHtml library. This results
      in cleaner, faster code, and it makes it easier to use Pandoc in
      other projects, like wikis, which use Text.XHtml. Two functions are
      now provided, writeHtml and writeHtmlString: the former outputs an
      Html structure, the latter a rendered string. The S5 writer is also
      changed, in parallel ways (writeS5, writeS5String).
    + The Html header is now written programmatically, so it has been
      removed from the 'headers' directory. The S5 header is still
      needed, but the doctype and some of the meta declarations have
      been removed, since they are written programmatically. This change
      introduces a new dependency on the xhtml package.
    + Fixed two bugs in email obfuscation involving improper escaping
      of '&' in the `<noscript>` section and in `--strict` mode. Resolves
      Issue #9.
    + Fixed another bug in email obfuscation: If the text to be obfuscated
      contains an entity, this needs to be decoded before obfuscation.
      Thanks to thsutton for the patch. Resolves Issue #15.
    + Changed the way the backlink is displayed in HTML footnotes.
      Instead of appearing on a line by itself, it now generally
      appears on the last line of the note.  (Exception:  when the
      note does not end with a Plain or Para block.) This saves space
      and looks better.
    + Added automatic unique identifiers to headers:
      - The identifier is derived from the header via a scheme
        documented in README.
      - WriterState now includes a list of header identifiers and a table
        of contents in addition to notes.
      - The function uniqueIdentifiers creates a list of unique identifiers
        from a list of inline lists (e.g. headers).
      - This list is part of WriterState and gets consumed by blockToHtml
        each time a header is encountered.
    + Include CSS for .strikethrough class in header only if strikethrough
      text appears in the document.
    + If the 'strict' option is specified, elements that do not appear in
      standard markdown (like definition lists) are passed through as
      raw HTML.
    + Simplified treatment of autolinks, using pattern matching instead of
      conditionals.

  * Markdown writer:

    + Links in markdown output are now printed as inline links by default,
      rather than reference links.  A --reference-links option has been added
      that forces links to be printed as reference links.  Resolves Issue #4.
    + Use autolinks when possible.  Instead of `[site.com](site.com)`,
      use `<site.com>`.

  * LaTeX writer:

    + Rewrote to use the State monad. The preamble now includes only those
      packages that are actually required, given the document's content.
      Thus, for example, if strikeout is not used, ulem is not required.
      Modified LaTeXHeader accordingly.
    + Modified LaTeX writer to insert `\,` between consecutive quotes.
    + Removed unused function tableRowColumnWidths.
    + Simplified code for escaping special characters.
    + Leave extra blank line after `\maketitle`.
    + Include empty `\author{}` when no author specified to avoid LaTeX
      errors.
    + Include fancyvrb code in header only if needed -- that is, only
      if there is actually code in a footnote.
    + Use `\url{}` for autolinks.
    + Include [mathletters] option in ucs package, so that basic unicode
      Greek letters will work correctly.

  * RST writer:  Force blank line before lists, so that sublists will
    be handled correctly.

  * Docbook writer:  Fixed a bug:  email links with text, like
    [foo](me@bar.baz), were being incorrectly treated as autolinks.

  * Removed Text.ParserCombinators.Pandoc and moved all its functions to
    Text.Pandoc.Shared.

  * Text.Pandoc.Shared:

    + Added defaultWriterOptions.
    + Added writerTableOfContents to WriterOptions.
    + Added writerIgnoreNotes option to WriterOptions.  This is needed
      for processing header blocks for a table of contents, since notes on
      headers should not appear in the TOC.
    + Added prettyprinting for native Table format.
    + Removed some unneeded imports.
    + Moved escape and nullBlock parsers from
      Text.ParserCombinators.Pandoc, since the latter is for
      general-purpose parsers that don't depend on Text.Pandoc.Definition.
    + Moved isHeaderBlock from Text.Pandoc.Writers.HTML.
    + Moved Element, headerAtLeast, and hierarchicalize from Docbook
      writer, because HTML writer now uses these in constructing a table
      of contents.
    + Added clauses for new inline elements (Strikeout, Superscript,
      Subscript) to refsMatch.
    + Removed backslashEscape; added new functions escapeStringUsing and
      backslashEscapes.
    + Moved failIfStrict from markdown reader, since it is now used also
      by the HTML reader.
    + Added a 'try' to the definition of indentSpaces.
    + In definition of 'reference', added check to make sure it's not a note
      reference.
    + Added functions: camelCaseToHyphenated, toRomanNumeral,
      anyOrderedListMarker, orderedListmarker, orderedListMarkers,
      charsInBalanced', withHorizDisplacement, romanNumeral
    + Fixed a bug in the anyLine parser. Previously it would parse an empty
      string "", but it should fail on an empty string, or we get an error
      when it is used inside "many" combinators.
    + Removed followedBy' parser, replacing it with the lookAhead parser from
      Parsec.
    + Added some needed 'try's before multicharacter parsers, especially in
      'option' contexts.
    + Removed the 'try' from the 'end' parser in 'enclosed', so that
      'enclosed' behaves like 'option', 'manyTill', etc.
    + Added lineClump parser, which parses a raw line block up to and
      including any following blank lines.
    + Renamed parseFromStr to parseFromString.
    + Added a 'try' to the 'end' parser in 'enclosed'.  This makes errors in
      the use of 'enclosed' less likely. Removed some now-unnecessary 'try's
      in calling code.
    + Removed unneeded 'try' in blanklines.
    + Removed endsWith function and rewrote calling functions to use
      isSuffixOf instead.
    + Added >>~ combinator.
    + Fixed bug in normalizeSpaces:  Space:Str "":Space should compress to
      Space.

  * Refactored runtests.pl; added separate tests for tables.

  * Shell scripts:

    + Added -asxhtml flag to tidy in html2markdown. This will
      perhaps help the parser, which expects closing tags.
    + Modified markdown2pdf to run pdflatex a second time if --toc or
      --table-of-contents was specified; otherwise the table of
      contents won't appear.
    + Modified markdown2pdf to print a helpful message if the 'ulem'
      LaTeX package is required and not found.

  * Changes to build process:

    + Dropped support for compilation with GHC 6.4.  GHC 6.6 or higher
      is now required.
    + Removed cabalize and Pandoc.cabal.in. The repository now contains
      pandoc.cabal itself.
    + Pandoc.cabal has been changed to pandoc.cabal, because HackageDB
      likes the cabal file to have the same name as the tarball.
    + Expanded and revised the package description in pandoc.cabal.
      Revised the package synopsis.
    + The tarball built by 'make tarball' now contains files built from
      templates (including man pages and shell scripts), so pandoc can
      be built directly using Cabal tools, without preprocessing.
    + Executable binaries are now stripped before installing.
    + Man pages are now generated from markdown sources, using pandoc's
      man page writer.
    + Use HTML version of README (instead of RTF) in Mac OS X installer.
    + Instead of testing for the existence of a pandoc symlink in build-exec,
      use ln -f.

  * Documentation:

    + Updated README and man pages with information on new features.
    + Updated INSTALL instructions with some useful clarifications and
      links.
    + Updated web content.

  * Added FreeBSD port.

  [ Recai Oktaş ]

  * debian/control:

    + Changed pandoc's Build-Depends to include libghc6-mtl-dev and
      libghc6-xhtml-dev.  Removed libghc6-html-dev.
    + Suggest texlive-latex-recommended | tetex-extra instead of
      tetex-bin.  This brings in fancyvrb and unicode support.


## pandoc 0.3 (2007-01-05)

  [ John MacFarlane ]

  * Changes in pandoc options:

    + Allow options to follow or precede arguments.
    + Changed '--smartypants' to '--smart' and adjusted symbols accordingly.
    + Added '--strict' option.
    + Added '-o/--output' option.
    + Added '--dump-args' and '--ignore-args' options (for use in wrappers).
    + Modified '-v' and '-h' output to go to STDERR, not STDOUT, and return
      error conditions.  This is helpful for writing wrappers.
    + Added copyright message to '-v' output, modeled after FSF messages.
    + Reformatted usage message so that it doesn't wrap illegibly.
    + Removed extra blanks after '-h' and '-D' output.

  * Added docbook writer.

  * Added implicit setting of default input and output format based
    on input and output filename extensions.  These defaults are
    overridden if explicit input and output formats are specified using
    '-t', '-f', '-r', or '-w' options.  Documented in pandoc(1) man page
    and README.

  * Allow ordered list items to begin with (single) letters, as well
    as numbers.  The list item marker may now be terminated either by
    '.' or by ')'.  This extension to standard markdown is documented
    in README.

  * Revised footnote syntax.  (See README for full details.)  The
    '[^1]' format now standard in markdown extensions is supported,
    as are inline footnotes with this syntax: `^[My note.]`.
    The earlier footnote syntax `^(1)` is no longer supported.

  * Improved HTML representation of footnotes.  All footnotes
    are now auto-numbered and appear in an ordered list at the
    end of the HTML document.  Since the default appearance is now
    acceptable, the old footnote styles have been removed from the
    HTML header.

  * Bug fixes:

    + Fixed a serious bug in the markdown, LaTeX, and RST readers.
      These readers ran 'runParser' on processed chunks of text to handle
      embedded block lists in lists and quotation blocks.  But then
      any changes made to the parser state in these chunks was lost,
      as the state is local to the parser.  So, for example, footnotes
      didn't work in quotes or list items.  The fix:  instead of calling
      runParser on some raw text, use setInput to make it the input, then
      parse it, then use setInput to restore the input to what it was
      before.  This is shorter and more elegant, and it fixes the problem.
    + Fixed bug in notFollowedBy' combinator (adding 'try' before
      'parser').  Adjusted code that uses this combinator accordingly.
    + Fixed bug in RTF writer that caused improper indentation on
      footnotes occurring in indented blocks like lists.
    + Fixed parsing of metadata in LaTeX reader.  Now the title, author,
      and date are parsed correctly.  Everything else in the preamble
      is skipped.
    + Modified escapedChar in LaTeX reader to allow a `\` at the end of a
      line to count as escaped whitespace.
    + Modified LaTeX reader to produce inline links rather than reference
      links.  Otherwise, links in footnotes aren't handled properly.
    + Fixed handling of titles in links in Markdown reader, so that
      embedded quotation marks are now handled properly.
    + Fixed Markdown reader's handling of embedded brackets in links.
    + Fixed Markdown reader so that it only parses bracketed material
      as a reference link if there is actually a corresponding key.
    + Revised inline code parsing in Markdown reader to conform to
      markdown standard.  Now any number of `s can begin inline code,
      which will end with the same number of `s.  For example, to
      have two backticks as code, write ``` `` ```.  Modified Markdown
      writer accordingly.
    + Fixed bug in text-wrapping routine in Markdown and RST writers.
      Now LineBreaks no longer cause wrapping problems.
    + Supported hexadecimal numerical entity references as well as
      decimal ones.
    + Fixed bug in Markdown reader's handling of underscores and other
      inline formatting markers inside reference labels:  for example,
      in '[A_B]: /url/a_b', the material between underscores was being
      parsed as emphasized inlines.
    + Changed Markdown reader's handling of backslash escapes so that
      only non-alphanumeric characters can be escaped.  Strict mode
      follows Markdown.pl in only allowing a select group of punctuation
      characters to be escaped.
    + Modified HTML reader to skip a newline following a `<br>` tag.
      Otherwise the newline will be treated as a space at the beginning
      of the next line.

  * Made handling of code blocks more consistent.  Previously, some
    readers allowed trailing newlines, while others stripped them.
    Now, all readers strip trailing newlines in code blocks. Writers
    insert a newline at the end of code blocks as needed.

  * Modified readers to make spacing at the end of output more consistent.

  * Minor improvements to LaTeX reader:

    + `\thanks` now treated like a footnote.
    + Simplified parsing of LaTeX command arguments and options.
      commandArgs now returns a list of arguments OR options (in
      whatever order they appear).  The brackets are included, and
      a new stripFirstAndLast function is provided to strip them off
      when needed.  This fixes a problem in dealing with \newcommand
      and \newenvironment.

  * Revised RTF writer:

    + Default font is now Helvetica.
    + An `\f0` is added to each `\pard`, so that font resizing works
      correctly.

  * Moved handling of "smart typography" from the writers to the Markdown
    and LaTeX readers.  This allows great simplification of the writers
    and more accurate smart quotes, dashes, and ellipses.  DocBook can
    now use `<quote>`.  The '--smart' option now toggles an option in
    the parser state rather than a writer option.  Several new kinds
    of inline elements have been added: Quoted, Ellipses, Apostrophe,
    EmDash, EnDash.

  * Changes in HTML writer:

    + Include title block in header even when title is null.
    + Made javascript obfuscation of emails even more obfuscatory,
      by combining it with entity obfuscation.

  * Changed default ASCIIMathML text color to black.

  * Test suite:

    + Added --strip-trailing-cr option to diff in runtests.pl, for
      compatibility with Windows.
    + Added regression tests with footnotes in quote blocks and lists.

  * Makefile changes:

    + osx-pkg target creates a Mac OS X package (directory). New osx
      directory contains files needed for construction of the package.
    + osx-dmg target creates a compressed disk image containing the package.
    + win-pkg target creates Windows binary package.
    + tarball target creates distribution source tarball.
    + website target generates pandoc's website automatically, including
      demos.  New 'web' directory contains files needed for construction
      of the website (which will be created as the 'pandoc' subdirectory
      of 'web').
    + Makefile checks to see if we're running Windows/Cygwin; if so,
      a '.exe' extension is added to each executable in EXECS.

  * Removed all wrappers except markdown2pdf and html2markdown.

  * Added new wrapper hsmarkdown, to be used as a drop-in replacement
    for Markdown.pl.  hsmarkdown calls pandoc with the '--strict'
    option and disables other options.

  * Added code to html2markdown that tries to determine the character
    encoding of an HTML file, by parsing the "Content-type" meta tag.

    + If the encoding can't be determined, then if the content is local,
      the local encoding is used; if it comes from a URL, UTF-8 is used
      by default.
    + If input is from STDIN, don't try to determine character encoding.
    + Encoding can be specified explicitly using '-e' option.

  * Improved warning messages in wrappers:

    + Print warning if iconv not available
    + More user-friendly error messages in markdown2pdf, when
      pdflatex fails.

  * Code cleanup:

    + Renamed 'Text/Pandoc/HtmlEntities' module to
      'Text/Pandoc/Entities'. Also changed function names so as
      not to be HTML-specific.
    + Refactored SGML string escaping functions from HTML and Docbook
      writers into Text/Pandoc/Shared.  (escapeSGML, stringToSGML)
    + Removed 'BlockQuoteContext' from ParserContext, as it isn't
      used anywhere.
    + Removed splitBySpace and replaced it with a general, polymorphic
      splitBy function.
    + Refactored LaTeX reader for clarity (added isArg function).
    + Converted some CR's to LF's in src/ui/default/print.css.
    + Added license text to top of source files.
    + Added module data for haddock to source files.
    + Reformatted code for consistency.

  * Rewrote documentation and man pages.  Split README into INSTALL
    and README.

  * Split LICENSE into COPYING and COPYRIGHT.

  * Removed TODO, since we now maintain ToDo on the wiki.

  * Made COPYRIGHT in top level a symlink to debian/copyright, to avoid
    duplication.

  [ Recai Oktaş ]

  * Revamped build process to conform to debian standards and created
    a proper debian package.  Closes: #391666.

  * Modified build process to support GHC 6.6.

    + The package can still be compiled using GHC 6.4.2, though because
      of dependencies the "make deb" target works only with GHC 6.6+.
    + The script 'cabalize' is used to create an appropriate
      'Pandoc.cabal' from 'Pandoc.cabal.in', depending on the GHC and
      Cabal versions.

  * Refactored template processing (fillTemplates.pl).

  * Modified wrapper scripts to make them more robust and portable.
    To avoid code duplication and ensure consistency, wrappers are
    generated via a templating system from templates in src/wrappers.

    + Wrappers now accept multiple filenames, when appropriate.
    + Spaces and tabs allowed in filenames.
    + getopts shell builtin is used for portable option parsing.
    + Improved html2markdown's web grabber code, making it more robust,
      configurable and verbose.  Added '-e', '-g' options.


## pandoc 0.2 (2006-08-14)

  * Fixed unicode/utf-8 translation

## pandoc 0.1 (2006-08-14)

  * Initial creation of debian package

