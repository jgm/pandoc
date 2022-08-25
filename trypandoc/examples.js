const examples = {
  ["Hello world"]:
    { text: '*Hello* world!',
      from: 'markdown',
      to: 'html5'},
  ["BibTeX to CSL JSON"]:
    { text: `@BOOK{Wurm2011-ho,
  title     = "{Substanz und Qualität : Ein Beitrag zur Interpretation der
               plotinischen Traktate VI,1, 2 und 3}",
  author    = "Wurm, Klaus",
  publisher = "De Gruyter",
  series    = "Quellen und Studien zur Philosophie",
  edition   = "Reprint 2011",
  year      =  2011,
  address   = "Berlin",
  keywords  = "!!! Plotinus translation",
  language  = "de"
}`,
      from: 'bibtex',
      to: 'csljson' },
  ["Markdown to Docbook with citations"]:
  { text: `---
references:
- author:
  - family: Salam
    given: Abdus
  container-title: "Elementary particle theory: Relativistic groups and
    analyticity. Proceedings of the eighth Nobel symposium"
  editor:
  - family: Svartholm
    given: Nils
  event-date: 1968-05-19/1968-05-25
  event-place: Aspenäsgarden, Lerum
  id: salam
  issued: 1968
  page: 367-377
  publisher: Almquist & Wiksell
  publisher-place: Stockholm
  title: Weak and electromagnetic interactions
  type: paper-conference
---

@salam [p. 370] says some interesting things.`,
    from: 'markdown',
    to: 'docbook5',
    standalone: true,
    citeproc: true,
    files: {} },
  ["MediaWiki to docx with equations"]:
  { text: `Just as the components of a vector change when we change the [[basis (linear algebra)|basis]] of the vector space, the components of a tensor also change under such a transformation.  Each type of tensor comes equipped with a ''transformation law'' that details how the components of the tensor respond to a [[change of basis]].  The components of a vector can respond in two distinct ways to a [[change of basis]] (see [[covariance and contravariance of vectors]]), where the new [[basis vectors]] <math>\\mathbf{\\hat{e}}_i</math> are expressed in terms of the old basis vectors <math>\\mathbf{e}_j</math> as,
:<math>\\mathbf{\\hat{e}}_i = \\sum_{j=1}^n \\mathbf{e}_j R^j_i = \\mathbf{e}_j R^j_i .</math>

Here ''R''<sup>'' j''</sup><sub>''i''</sub> are the entries of the change of basis matrix, and in the rightmost expression the [[summation]] sign was suppressed: this is the [[Einstein summation convention]], which will be used throughout this article.<ref group="Note">The Einstein summation convention, in brief, requires the sum to be taken over all values of the index whenever the same symbol appears as a subscript and superscript in the same term.  For example, under this convention <math>B_i C^i = B_1 C^1 + B_2 C^2 + \\cdots B_n C^n</math></ref>  The components ''v''<sup>''i''</sup> of a column vector '''v''' transform with the [[matrix inverse|inverse]] of the matrix ''R'',
:<math>\\hat{v}^i = \\left(R^{-1}\\right)^i_j v^j,</math>

where the hat denotes the components in the new basis.  This is called a ''contravariant'' transformation law, because the vector components transform by the ''inverse'' of the change of basis.  In contrast, the components, ''w''<sub>''i''</sub>, of a covector (or row vector), '''w''', transform with the matrix ''R'' itself,
:<math>\\hat{w}_i = w_j R^j_i .</math>`,
    from: 'mediawiki',
    to: 'docx',
    standalone: true },

  ["Man page to ConTeXt"]:
  { text: `.TP
\\f[C]-L\\f[R] \\f[I]SCRIPT\\f[R], \\f[C]--lua-filter=\\f[R]\\f[I]SCRIPT\\f[R]
Transform the document in a similar fashion as JSON filters (see
\\f[C]--filter\\f[R]), but use pandoc\\[cq]s built-in Lua filtering system.
The given Lua script is expected to return a list of Lua filters which
will be applied in order.
Each Lua filter must contain element-transforming functions indexed by
the name of the AST element on which the filter function should be
applied.
.RS
.PP
The \\f[C]pandoc\\f[R] Lua module provides helper functions for element
creation.
It is always loaded into the script\\[cq]s Lua environment.
.PP
See the Lua filters documentation for further details.
.PP
In order of preference, pandoc will look for Lua filters in
.IP "1." 3
a specified full or relative path,
.IP "2." 3
\\f[C]$DATADIR/filters\\f[R] where \\f[C]$DATADIR\\f[R] is the user data
directory (see \\f[C]--data-dir\\f[R], above).
.PP
Filters, Lua filters, and citeproc processing are applied in the order
specified on the command line.
.RE
.TP
\\f[C]-M\\f[R] \\f[I]KEY\\f[R][\\f[C]=\\f[R]\\f[I]VAL\\f[R]], \\f[C]--metadata=\\f[R]\\f[I]KEY\\f[R][\\f[C]:\\f[R]\\f[I]VAL\\f[R]]
Set the metadata field \\f[I]KEY\\f[R] to the value \\f[I]VAL\\f[R].
A value specified on the command line overrides a value specified in the
document using YAML metadata blocks.
Values will be parsed as YAML boolean or string values.
If no value is specified, the value will be treated as Boolean true.
Like \\f[C]--variable\\f[R], \\f[C]--metadata\\f[R] causes template
variables to be set.
But unlike \\f[C]--variable\\f[R], \\f[C]--metadata\\f[R] affects the
metadata of the underlying document (which is accessible from filters
and may be printed in some output formats) and metadata values will be
escaped when inserted into the template.`,
    from: 'man',
    to: 'context' },
  ["LaTeX with macros to reStructuredText"]:
  { text: `% from https://en.wikibooks.org/wiki/LaTeX/Macros
\\newcommand{\\wbalTwo}[2][Wikimedia]{
This is the Wikibook about LaTeX
supported by {#1} and {#2}!}

\\begin{itemize}
\\item \\wbalTwo{John Doe}
\\item \\wbalTwo[lots of users]{John Doe}
\\end{itemize}`,
    from: 'latex',
    to: 'rst',
    standalone: true,
    citeproc: false,
    files: {} },

  ["CSV table to org"]:
  { text: `"Year", "Score", "Title"
1968,  86, "Greetings"
1970,  17, "Bloody Mama"
1970,  73, "Hi, Mom!"
1971,  40, "Born to Win"
1973,  98, "Mean Streets"
1973,  88, "Bang the Drum Slowly"
1974,  97, "The Godfather, Part II"
1976,  41, "The Last Tycoon"
1976,  99, "Taxi Driver"`,
    from: 'csv',
    to: 'org' }

}


