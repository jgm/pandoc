.\" **** Custom macro definitions *********************************
.\" * Super/subscript
.\" (https://lists.gnu.org/archive/html/groff/2012-07/msg00046.html)
.ds { \v'-0.3m'\\s[\\n[.s]*9u/12u]
.ds } \s0\v'0.3m'
.ds < \v'0.3m'\s[\\n[.s]*9u/12u]
.ds > \s0\v'-0.3m'
.\" * Horizontal line
.de HLINE
.LP
.ce
\l'20'
..
.\" **** Settings *************************************************
.\" text width
.nr LL 5.5i
.\" left margin
.nr PO 1.25i
.\" top margin
.nr HM 1.25i
.\" bottom margin
.nr FM 1.25i
.\" header/footer width
.nr LT \n[LL]
.\" point size
.nr PS 10p
.\" line height
.nr VS 12p
.\" font family: A, BM, H, HN, N, P, T, ZCM
.fam P
.\" paragraph indent
.nr PI 0m
.\" interparagraph space
.nr PD 0.4v
.\" footnote width
.nr FL \n[LL]
.\" footnote point size
.nr FPS (\n[PS] - 2000)
.\" color used for strikeout
.defcolor strikecolor rgb 0.7 0.7 0.7
.\" color for links (rgb)
.ds PDFHREF.COLOUR   0.35 0.00 0.60
.\" border for links (default none)
.ds PDFHREF.BORDER   0 0 0
.\" point size difference between heading levels
.nr PSINCR 1p
.\" heading level above which point size no longer changes
.nr GROWPS 2
.\" comment these out if you want a dot after section numbers:
.als SN SN-NO-DOT
.als SN-STYLE SN-NO-DOT
.\" page numbers in footer, centered
.rm CH
.ds CF %
.\" pdf outline fold level
.nr PDFOUTLINE.FOLDLEVEL 3
.\" start out in outline view
.pdfview /PageMode /UseOutlines
.\" ***************************************************************
.\" PDF metadata
.pdfinfo /Title "Pandoc Test Suite"
.pdfinfo /Author "John MacFarlane; Anonymous"
.hy
.EQ
delim @@
.EN
.TL
Pandoc Test Suite
.AU
John MacFarlane
.AU
Anonymous
.AU
.sp 0.5
.ft R
July 17, 2006
.\" 1 column (use .2C for two column)
.1C
.LP
This is a set of tests for pandoc.
Most of them are adapted from John Gruber\[cq]s markdown test suite.
.HLINE
.SH 1
Headers
.pdfhref O 1 "Headers"
.pdfhref M "headers"
.SH 2
Level 2 with an \c
.pdfhref W -D "/url" -A "\c" \
 -- "embedded link"
\&
.pdfhref O 2 "Level 2 with an embedded link"
.pdfhref M "level-2-with-an-embedded-link"
.SH 3
Level 3 with \f[BI]emphasis\f[B]
.pdfhref O 3 "Level 3 with emphasis"
.pdfhref M "level-3-with-emphasis"
.SH 4
Level 4
.pdfhref O 4 "Level 4"
.pdfhref M "level-4"
.SH 5
Level 5
.pdfhref O 5 "Level 5"
.pdfhref M "level-5"
.SH 1
Level 1
.pdfhref O 1 "Level 1"
.pdfhref M "level-1"
.SH 2
Level 2 with \f[BI]emphasis\f[B]
.pdfhref O 2 "Level 2 with emphasis"
.pdfhref M "level-2-with-emphasis"
.SH 3
Level 3
.pdfhref O 3 "Level 3"
.pdfhref M "level-3"
.LP
with no blank line
.SH 2
Level 2
.pdfhref O 2 "Level 2"
.pdfhref M "level-2"
.LP
with no blank line
.HLINE
.SH 1
Paragraphs
.pdfhref O 1 "Paragraphs"
.pdfhref M "paragraphs"
.LP
Here\[cq]s a regular paragraph.
.PP
In Markdown 1.0.0 and earlier.
Version 8.
This line turns into a list item.
Because a hard-wrapped line in the middle of a paragraph looked like a list
item.
.PP
Here\[cq]s one with a bullet.
* criminey.
.PP
There should be a hard line break
.br
here.
.HLINE
.SH 1
Block Quotes
.pdfhref O 1 "Block Quotes"
.pdfhref M "block-quotes"
.LP
E-mail style:
.QS
.LP
This is a block quote.
It is pretty short.
.QE
.QS
.LP
Code in a block quote:
.IP
.nf
\f[C]
sub status {
    print \[dq]working\[dq];
}
\f[]
.fi
.LP
A list:
.IP " 1." 4
item one
.IP " 2." 4
item two
.LP
Nested block quotes:
.QS
.LP
nested
.QE
.QS
.LP
nested
.QE
.QE
.LP
This should not be a block quote: 2 > 1.
.PP
And a following paragraph.
.HLINE
.SH 1
Code Blocks
.pdfhref O 1 "Code Blocks"
.pdfhref M "code-blocks"
.LP
Code:
.IP
.nf
\f[C]
---- (should be four hyphens)

sub status {
    print \[dq]working\[dq];
}

this code block is indented by one tab
\f[]
.fi
.LP
And:
.IP
.nf
\f[C]
    this code block is indented by two tabs

These should not be escaped:  \[rs]$ \[rs]\[rs] \[rs]> \[rs][ \[rs]{
\f[]
.fi
.HLINE
.SH 1
Lists
.pdfhref O 1 "Lists"
.pdfhref M "lists"
.SH 2
Unordered
.pdfhref O 2 "Unordered"
.pdfhref M "unordered"
.LP
Asterisks tight:
.IP \[bu] 3
asterisk 1
.IP \[bu] 3
asterisk 2
.IP \[bu] 3
asterisk 3
.LP
Asterisks loose:
.IP \[bu] 3
asterisk 1
.IP \[bu] 3
asterisk 2
.IP \[bu] 3
asterisk 3
.LP
Pluses tight:
.IP \[bu] 3
Plus 1
.IP \[bu] 3
Plus 2
.IP \[bu] 3
Plus 3
.LP
Pluses loose:
.IP \[bu] 3
Plus 1
.IP \[bu] 3
Plus 2
.IP \[bu] 3
Plus 3
.LP
Minuses tight:
.IP \[bu] 3
Minus 1
.IP \[bu] 3
Minus 2
.IP \[bu] 3
Minus 3
.LP
Minuses loose:
.IP \[bu] 3
Minus 1
.IP \[bu] 3
Minus 2
.IP \[bu] 3
Minus 3
.SH 2
Ordered
.pdfhref O 2 "Ordered"
.pdfhref M "ordered"
.LP
Tight:
.IP " 1." 4
First
.IP " 2." 4
Second
.IP " 3." 4
Third
.LP
and:
.IP " 1." 4
One
.IP " 2." 4
Two
.IP " 3." 4
Three
.LP
Loose using tabs:
.IP " 1." 4
First
.IP " 2." 4
Second
.IP " 3." 4
Third
.LP
and using spaces:
.IP " 1." 4
One
.IP " 2." 4
Two
.IP " 3." 4
Three
.LP
Multiple paragraphs:
.IP " 1." 4
Item 1, graf one.
.RS 4
.PP
Item 1.
graf two.
The quick brown fox jumped over the lazy dog\[cq]s back.
.RE
.IP " 2." 4
Item 2.
.IP " 3." 4
Item 3.
.SH 2
Nested
.pdfhref O 2 "Nested"
.pdfhref M "nested"
.IP \[bu] 3
Tab
.RS 3
.IP \[bu] 3
Tab
.RS 3
.IP \[bu] 3
Tab
.RE
.RE
.LP
Here\[cq]s another:
.IP " 1." 4
First
.IP " 2." 4
Second:
.RS 4
.IP \[bu] 3
Fee
.IP \[bu] 3
Fie
.IP \[bu] 3
Foe
.RE
.IP " 3." 4
Third
.LP
Same thing but with paragraphs:
.IP " 1." 4
First
.IP " 2." 4
Second:
.RS 4
.IP \[bu] 3
Fee
.IP \[bu] 3
Fie
.IP \[bu] 3
Foe
.RE
.IP " 3." 4
Third
.SH 2
Tabs and spaces
.pdfhref O 2 "Tabs and spaces"
.pdfhref M "tabs-and-spaces"
.IP \[bu] 3
this is a list item indented with tabs
.IP \[bu] 3
this is a list item indented with spaces
.RS 3
.IP \[bu] 3
this is an example list item indented with tabs
.IP \[bu] 3
this is an example list item indented with spaces
.RE
.SH 2
Fancy list markers
.pdfhref O 2 "Fancy list markers"
.pdfhref M "fancy-list-markers"
.IP " (2)" 5
begins with 2
.IP " (3)" 5
and now 3
.RS 5
.LP
with a continuation
.IP " iv." 5
sublist with roman numerals, starting with 4
.IP "  v." 5
more items
.RS 5
.IP " (A)" 5
a subsublist
.IP " (B)" 5
a subsublist
.RE
.RE
.LP
Nesting:
.IP " A." 4
Upper Alpha
.RS 4
.IP " I." 4
Upper Roman.
.RS 4
.IP " (6)" 5
Decimal start with 6
.RS 5
.IP " c)" 4
Lower alpha with paren
.RE
.RE
.RE
.LP
Autonumbering:
.IP " 1." 4
Autonumber.
.IP " 2." 4
More.
.RS 4
.IP " 1." 4
Nested.
.RE
.LP
Should not be a list item:
.PP
M.A.\ 2007
.PP
B.
Williams
.HLINE
.SH 1
Definition Lists
.pdfhref O 1 "Definition Lists"
.pdfhref M "definition-lists"
.LP
Tight using spaces:
.IP "\f[B]apple\f[R]" 3
red fruit
.RS 3
.RE
.IP "\f[B]orange\f[R]" 3
orange fruit
.RS 3
.RE
.IP "\f[B]banana\f[R]" 3
yellow fruit
.RS 3
.RE
.LP
Tight using tabs:
.IP "\f[B]apple\f[R]" 3
red fruit
.RS 3
.RE
.IP "\f[B]orange\f[R]" 3
orange fruit
.RS 3
.RE
.IP "\f[B]banana\f[R]" 3
yellow fruit
.RS 3
.RE
.LP
Loose:
.IP "\f[B]apple\f[R]" 3
red fruit
.RS 3
.RE
.IP "\f[B]orange\f[R]" 3
orange fruit
.RS 3
.RE
.IP "\f[B]banana\f[R]" 3
yellow fruit
.RS 3
.RE
.LP
Multiple blocks with italics:
.IP "\f[B]\f[BI]apple\f[B]\f[R]" 3
red fruit
.RS 3
.PP
contains seeds, crisp, pleasant to taste
.RE
.IP "\f[B]\f[BI]orange\f[B]\f[R]" 3
orange fruit
.RS 3
.IP
.nf
\f[C]
{ orange code block }
\f[]
.fi
.QS
.LP
orange block quote
.QE
.RE
.LP
Multiple definitions, tight:
.IP "\f[B]apple\f[R]" 3
red fruit
.RS 3
.RE
computer
.RS 3
.RE
.IP "\f[B]orange\f[R]" 3
orange fruit
.RS 3
.RE
bank
.RS 3
.RE
.LP
Multiple definitions, loose:
.IP "\f[B]apple\f[R]" 3
red fruit
.RS 3
.RE
computer
.RS 3
.RE
.IP "\f[B]orange\f[R]" 3
orange fruit
.RS 3
.RE
bank
.RS 3
.RE
.LP
Blank line after term, indented marker, alternate markers:
.IP "\f[B]apple\f[R]" 3
red fruit
.RS 3
.RE
computer
.RS 3
.RE
.IP "\f[B]orange\f[R]" 3
orange fruit
.RS 3
.IP " 1." 4
sublist
.IP " 2." 4
sublist
.RE
.SH 1
HTML Blocks
.pdfhref O 1 "HTML Blocks"
.pdfhref M "html-blocks"
.LP
Simple block on one line:
foo
.LP
And nested without indentation:
.LP
foo
bar
.LP
Interpreted markdown in a table:
This is \f[I]emphasized\f[R]
And this is \f[B]strong\f[R]
.PP
Here\[cq]s a simple block:
.LP
foo
.LP
This should be a code block, though:
.IP
.nf
\f[C]
<div>
    foo
</div>
\f[]
.fi
.LP
As should this:
.IP
.nf
\f[C]
<div>foo</div>
\f[]
.fi
.LP
Now, nested:
foo
.LP
This should just be an HTML comment:
.PP
Multiline:
.PP
Code block:
.IP
.nf
\f[C]
<!-- Comment -->
\f[]
.fi
.LP
Just plain comment, with trailing spaces on the line:
.PP
Code:
.IP
.nf
\f[C]
<hr />
\f[]
.fi
.LP
Hr\[cq]s:
.HLINE
.SH 1
Inline Markup
.pdfhref O 1 "Inline Markup"
.pdfhref M "inline-markup"
.LP
This is \f[I]emphasized\f[R], and so \f[I]is this\f[R].
.PP
This is \f[B]strong\f[R], and so \f[B]is this\f[R].
.PP
An \f[I]\c
.pdfhref W -D "/url" -A "\c" \
 -- "emphasized link"
\&\f[R].
.PP
\f[B]\f[BI]This is strong and em.\f[B]\f[R]
.PP
So is \f[B]\f[BI]this\f[B]\f[R] word.
.PP
\f[B]\f[BI]This is strong and em.\f[B]\f[R]
.PP
So is \f[B]\f[BI]this\f[B]\f[R] word.
.PP
This is code: \f[C]>\f[R], \f[C]$\f[R], \f[C]\[rs]\f[R], \f[C]\[rs]$\f[R],
\f[C]<html>\f[R].
.PP
\m[strikecolor]This is \f[I]strikeout\f[R].\m[]
.PP
Superscripts: a\*{bc\*}d a\*{\f[I]hello\f[R]\*} a\*{hello\ there\*}.
.PP
Subscripts: H\*<2\*>O, H\*<23\*>O, H\*<many\ of\ them\*>O.
.PP
These should not be superscripts or subscripts, because of the unescaped
spaces: a\[ha]b c\[ha]d, a\[ti]b c\[ti]d.
.HLINE
.SH 1
Smart quotes, ellipses, dashes
.pdfhref O 1 "Smart quotes, ellipses, dashes"
.pdfhref M "smart-quotes-ellipses-dashes"
.LP
\[lq]Hello,\[rq] said the spider.
\[lq]`Shelob' is my name.\[rq]
.PP
`A', `B', and `C' are letters.
.PP
`Oak,' `elm,' and `beech' are names of trees.
So is `pine.'
.PP
`He said, \[lq]I want to go.\[rq]' Were you alive in the 70\[cq]s?
.PP
Here is some quoted `\f[C]code\f[R]' and a \[lq]\c
.pdfhref W -D "http://example.com/?foo=1&bar=2" -A "\c" \
 -- "quoted link"
\&\[rq].
.PP
Some dashes: one\[em]two \[em] three\[em]four \[em] five.
.PP
Dashes between numbers: 5\[en]7, 255\[en]66, 1987\[en]1999.
.PP
Ellipses\&...and\&...and\&....
.HLINE
.SH 1
LaTeX
.pdfhref O 1 "LaTeX"
.pdfhref M "latex"
.IP \[bu] 3
.IP \[bu] 3
@2 + 2 = 4@
.IP \[bu] 3
@x \[u2208] y@
.IP \[bu] 3
@alpha \[u2227] omega@
.IP \[bu] 3
@223@
.IP \[bu] 3
@p@-Tree
.IP \[bu] 3
Here\[cq]s some display math:
.EQ
d over {d x} f ( x ) = lim sub {h -> 0} {f ( x + h ) \[u2212] f ( x )} over h
.EN
.IP \[bu] 3
Here\[cq]s one that has a line break in it: @alpha + omega times x sup 2@.
.LP
These shouldn\[cq]t be math:
.IP \[bu] 3
To get the famous equation, write \f[C]$e = mc\[ha]2$\f[R].
.IP \[bu] 3
$22,000 is a \f[I]lot\f[R] of money.
So is $34,000.
(It worked if \[lq]lot\[rq] is emphasized.)
.IP \[bu] 3
Shoes ($20) and socks ($5).
.IP \[bu] 3
Escaped \f[C]$\f[R]: $73 \f[I]this should be emphasized\f[R] 23$.
.LP
Here\[cq]s a LaTeX table:
.HLINE
.SH 1
Special Characters
.pdfhref O 1 "Special Characters"
.pdfhref M "special-characters"
.LP
Here is some unicode:
.IP \[bu] 3
I hat: Î
.IP \[bu] 3
o umlaut: ö
.IP \[bu] 3
section: §
.IP \[bu] 3
set membership: ∈
.IP \[bu] 3
copyright: ©
.LP
AT&T has an ampersand in their name.
.PP
AT&T is another way to write it.
.PP
This & that.
.PP
4 < 5.
.PP
6 > 5.
.PP
Backslash: \[rs]
.PP
Backtick: \[ga]
.PP
Asterisk: *
.PP
Underscore: _
.PP
Left brace: {
.PP
Right brace: }
.PP
Left bracket: [
.PP
Right bracket: ]
.PP
Left paren: (
.PP
Right paren: )
.PP
Greater-than: >
.PP
Hash: #
.PP
Period: .
.PP
Bang: !
.PP
Plus: +
.PP
Minus: -
.HLINE
.SH 1
Links
.pdfhref O 1 "Links"
.pdfhref M "links"
.SH 2
Explicit
.pdfhref O 2 "Explicit"
.pdfhref M "explicit"
.LP
Just a \c
.pdfhref W -D "/url/" -A "\c" \
 -- "URL"
\&.
.PP
\c
.pdfhref W -D "/url/" -A "\c" \
 -- "URL and title"
\&.
.PP
\c
.pdfhref W -D "/url/" -A "\c" \
 -- "URL and title"
\&.
.PP
\c
.pdfhref W -D "/url/" -A "\c" \
 -- "URL and title"
\&.
.PP
\c
.pdfhref W -D "/url/" -A "\c" \
 -- "URL and title"
\&
.PP
\c
.pdfhref W -D "/url/" -A "\c" \
 -- "URL and title"
\&
.PP
\c
.pdfhref W -D "/url/with_underscore" -A "\c" \
 -- "with_underscore"
\&
.PP
\c
.pdfhref W -D "mailto:nobody%40nowhere.net" -A "\c" \
 -- "Email link"
\&
.PP
\c
.pdfhref W -D "" -A "\c" \
 -- "Empty"
\&.
.SH 2
Reference
.pdfhref O 2 "Reference"
.pdfhref M "reference"
.LP
Foo \c
.pdfhref W -D "/url/" -A "\c" \
 -- "bar"
\&.
.PP
With \c
.pdfhref W -D "/url/" -A "\c" \
 -- "embedded [brackets]"
\&.
.PP
\c
.pdfhref W -D "/url/" -A "\c" \
 -- "b"
\& by itself should be a link.
.PP
Indented \c
.pdfhref W -D "/url" -A "\c" \
 -- "once"
\&.
.PP
Indented \c
.pdfhref W -D "/url" -A "\c" \
 -- "twice"
\&.
.PP
Indented \c
.pdfhref W -D "/url" -A "\c" \
 -- "thrice"
\&.
.PP
This should [not][] be a link.
.IP
.nf
\f[C]
[not]: /url
\f[]
.fi
.LP
Foo \c
.pdfhref W -D "/url/" -A "\c" \
 -- "bar"
\&.
.PP
Foo \c
.pdfhref W -D "/url/" -A "\c" \
 -- "biz"
\&.
.SH 2
With ampersands
.pdfhref O 2 "With ampersands"
.pdfhref M "with-ampersands"
.LP
Here\[cq]s a \c
.pdfhref W -D "http://example.com/?foo=1&bar=2" -A "\c" \
 -- "link with an ampersand in the URL"
\&.
.PP
Here\[cq]s a link with an amersand in the link text: \c
.pdfhref W -D "http://att.com/" -A "\c" \
 -- "AT&T"
\&.
.PP
Here\[cq]s an \c
.pdfhref W -D "/script?foo=1&bar=2" -A "\c" \
 -- "inline link"
\&.
.PP
Here\[cq]s an \c
.pdfhref W -D "/script?foo=1&bar=2" -A "\c" \
 -- "inline link in pointy braces"
\&.
.SH 2
Autolinks
.pdfhref O 2 "Autolinks"
.pdfhref M "autolinks"
.LP
With an ampersand: \c
.pdfhref W -D "http://example.com/?foo=1&bar=2" -A "\c" \
 -- "http://example.com/?foo=1&bar=2"
\&
.IP \[bu] 3
In a list?
.IP \[bu] 3
\c
.pdfhref W -D "http://example.com/" -A "\c" \
 -- "http://example.com/"
\&
.IP \[bu] 3
It should.
.LP
An e-mail address: \c
.pdfhref W -D "mailto:nobody%40nowhere.net" -A "\c" \
 -- "nobody\[at]nowhere.net"
\&
.QS
.LP
Blockquoted: \c
.pdfhref W -D "http://example.com/" -A "\c" \
 -- "http://example.com/"
\&
.QE
.LP
Auto-links should not occur here: \f[C]<http://example.com/>\f[R]
.IP
.nf
\f[C]
or here: <http://example.com/>
\f[]
.fi
.HLINE
.SH 1
Images
.pdfhref O 1 "Images"
.pdfhref M "images"
.LP
From \[lq]Voyage dans la Lune\[rq] by Georges Melies (1902):
.PP
[IMAGE: lalune]
.PP
Here is a movie [IMAGE: movie] icon.
.HLINE
.SH 1
Footnotes
.pdfhref O 1 "Footnotes"
.pdfhref M "footnotes"
.LP
Here is a footnote reference,\**
.FS
Here is the footnote.
It can go anywhere after the footnote reference.
It need not be placed at the end of the document.
.FE
and another.\**
.FS
Here\[cq]s the long note.
This one contains multiple blocks.
.PP
Subsequent blocks are indented to show that they belong to the footnote (as
with list items).
.IP
.nf
\f[C]
  { <code> }
\f[]
.fi
.LP
If you want, you can indent every line, but you can also be lazy and just
indent the first line of each block.
.FE
This should \f[I]not\f[R] be a footnote reference, because it contains a
space.[\[ha]my note] Here is an inline note.\**
.FS
This is \f[I]easier\f[R] to type.
Inline notes may contain \c
.pdfhref W -D "http://google.com" -A "\c" \
 -- "links"
\& and \f[C]]\f[R] verbatim characters, as well as [bracketed text].
.FE
.QS
.LP
Notes can go in quotes.\**
.FS
In quote.
.FE
.QE
.IP " 1." 4
And in list items.\**
.FS
In list.
.FE
.LP
This paragraph should not be part of the note, as it is not indented.
.pdfsync
