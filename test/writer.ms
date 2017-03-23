.hy
.EQ
delim ||
.EN
\" From https://lists.gnu.org/archive/html/groff/2012-07/msg00046.html
\" Superscripts (ex tmac.e) (current versions without extra line space)
.\" (reinstate commented versions to restore extra line space)
.\" .ds { \v'-0.3m'\x'-0.2m'\\s[\\n[.s]*8u/10u]
.ds { \v'-0.3m'\\s[\\n[.s]*9u/12u]
.ds } \s0\v'0.3m'
.\" Subscripts
.\" .ds < \v'0.3m'\x'0.2m'\s[\\n[.s]*8u/10u]
.ds < \v'0.3m'\s[\\n[.s]*9u/12u]
.ds > \s0\v'-0.3m'
.TL
Pandoc Test Suite
.AU
John MacFarlane
.AU
Anonymous
.LP
This is a set of tests for pandoc.
Most of them are adapted from
John Gruber's markdown test suite.
.PP
   *   *   *   *   *
.SH 1
Headers
.SH 2
Level 2 with an embedded link\**
.FS
/url
.FE
.SH 3
Level 3 with \f[I]emphasis\f[]
.SH 4
Level 4
.SH 5
Level 5
.SH 1
Level 1
.SH 2
Level 2 with \f[I]emphasis\f[]
.SH 3
Level 3
.LP
with no blank line
.SH 2
Level 2
.LP
with no blank line
.PP
   *   *   *   *   *
.SH 1
Paragraphs
.LP
Here's a regular paragraph.
.LP
In Markdown 1.0.0 and earlier.
Version
8.
This line turns into a list item.
Because a hard\-wrapped line in the
middle of a paragraph looked like a
list item.
.LP
Here's one with a bullet.
* criminey.
.LP
There should be a hard line break
.br
here.
.PP
   *   *   *   *   *
.SH 1
Block Quotes
.LP
E\-mail style:
.RS
.LP
This is a block quote.
It is pretty short.
.RE
.RS
.LP
Code in a block quote:
.IP
.nf
\f[C]
sub\ status\ {
\ \ \ \ print\ \"working\";
}
\f[]
.fi
.LP
A list:
.IP "1." 3
item one
.IP "2." 3
item two
.LP
Nested block quotes:
.RS
.LP
nested
.RE
.RS
.LP
nested
.RE
.RE
.LP
This should not be a block quote: 2
> 1.
.LP
And a following paragraph.
.PP
   *   *   *   *   *
.SH 1
Code Blocks
.LP
Code:
.IP
.nf
\f[C]
\-\-\-\-\ (should\ be\ four\ hyphens)

sub\ status\ {
\ \ \ \ print\ \"working\";
}

this\ code\ block\ is\ indented\ by\ one\ tab
\f[]
.fi
.LP
And:
.IP
.nf
\f[C]
\ \ \ \ this\ code\ block\ is\ indented\ by\ two\ tabs

These\ should\ not\ be\ escaped:\ \ \\$\ \\\\\ \\>\ \\[\ \\{
\f[]
.fi
.PP
   *   *   *   *   *
.SH 1
Lists
.SH 2
Unordered
.LP
Asterisks tight:
.IP \[bu] 2
asterisk 1
.IP \[bu] 2
asterisk 2
.IP \[bu] 2
asterisk 3
.LP
Asterisks loose:
.IP \[bu] 2
asterisk 1
.IP \[bu] 2
asterisk 2
.IP \[bu] 2
asterisk 3
.LP
Pluses tight:
.IP \[bu] 2
Plus 1
.IP \[bu] 2
Plus 2
.IP \[bu] 2
Plus 3
.LP
Pluses loose:
.IP \[bu] 2
Plus 1
.IP \[bu] 2
Plus 2
.IP \[bu] 2
Plus 3
.LP
Minuses tight:
.IP \[bu] 2
Minus 1
.IP \[bu] 2
Minus 2
.IP \[bu] 2
Minus 3
.LP
Minuses loose:
.IP \[bu] 2
Minus 1
.IP \[bu] 2
Minus 2
.IP \[bu] 2
Minus 3
.SH 2
Ordered
.LP
Tight:
.IP "1." 3
First
.IP "2." 3
Second
.IP "3." 3
Third
.LP
and:
.IP "1." 3
One
.IP "2." 3
Two
.IP "3." 3
Three
.LP
Loose using tabs:
.IP "1." 3
First
.IP "2." 3
Second
.IP "3." 3
Third
.LP
and using spaces:
.IP "1." 3
One
.IP "2." 3
Two
.IP "3." 3
Three
.LP
Multiple paragraphs:
.IP "1." 3
Item 1, graf one.
.RS 4
.LP
Item 1.
graf two.
The quick brown fox jumped over the lazy dog's
back.
.RE
.IP "2." 3
Item 2.
.IP "3." 3
Item 3.
.SH 2
Nested
.IP \[bu] 2
Tab
.RS 2
.IP \[bu] 2
Tab
.RS 2
.IP \[bu] 2
Tab
.RE
.RE
.LP
Here's another:
.IP "1." 3
First
.IP "2." 3
Second:
.RS 4
.IP \[bu] 2
Fee
.IP \[bu] 2
Fie
.IP \[bu] 2
Foe
.RE
.IP "3." 3
Third
.LP
Same thing but with paragraphs:
.IP "1." 3
First
.IP "2." 3
Second:
.RS 4
.IP \[bu] 2
Fee
.IP \[bu] 2
Fie
.IP \[bu] 2
Foe
.RE
.IP "3." 3
Third
.SH 2
Tabs and spaces
.IP \[bu] 2
this is a list item
indented with tabs
.IP \[bu] 2
this is a list item
indented with spaces
.RS 2
.IP \[bu] 2
this is an example list item
indented with tabs
.IP \[bu] 2
this is an example list item
indented with spaces
.RE
.SH 2
Fancy list markers
.IP "(2)" 4
begins with 2
.IP "(3)" 4
and now 3
.RS 4
.LP
with a continuation
.IP "iv." 4
sublist with roman numerals,
starting with 4
.IP " v." 4
more items
.RS 4
.IP "(A)" 4
a subsublist
.IP "(B)" 4
a subsublist
.RE
.RE
.LP
Nesting:
.IP "A." 3
Upper Alpha
.RS 4
.IP "I." 3
Upper Roman.
.RS 4
.IP "(6)" 4
Decimal start with 6
.RS 4
.IP "c)" 3
Lower alpha with paren
.RE
.RE
.RE
.LP
Autonumbering:
.IP "1." 3
Autonumber.
.IP "2." 3
More.
.RS 4
.IP "1." 3
Nested.
.RE
.LP
Should not be a list item:
.LP
M.A.\ 2007
.LP
B.
Williams
.PP
   *   *   *   *   *
.SH 1
Definition Lists
.LP
Tight using spaces:
.IP "apple"
red fruit
.RS
.RE
.IP "orange"
orange fruit
.RS
.RE
.IP "banana"
yellow fruit
.RS
.RE
.LP
Tight using tabs:
.IP "apple"
red fruit
.RS
.RE
.IP "orange"
orange fruit
.RS
.RE
.IP "banana"
yellow fruit
.RS
.RE
.LP
Loose:
.IP "apple"
red fruit
.RS
.RE
.IP "orange"
orange fruit
.RS
.RE
.IP "banana"
yellow fruit
.RS
.RE
.LP
Multiple blocks with italics:
.IP "\f[I]apple\f[]"
red fruit
.RS
.LP
contains seeds,
crisp, pleasant to taste
.RE
.IP "\f[I]orange\f[]"
orange fruit
.RS
.IP
.nf
\f[C]
{\ orange\ code\ block\ }
\f[]
.fi
.RS
.LP
orange block quote
.RE
.RE
.LP
Multiple definitions, tight:
.IP "apple"
red fruit
.RS
.RE
computer
.RS
.RE
.IP "orange"
orange fruit
.RS
.RE
bank
.RS
.RE
.LP
Multiple definitions, loose:
.IP "apple"
red fruit
.RS
.RE
computer
.RS
.RE
.IP "orange"
orange fruit
.RS
.RE
bank
.RS
.RE
.LP
Blank line after term, indented marker, alternate markers:
.IP "apple"
red fruit
.RS
.RE
computer
.RS
.RE
.IP "orange"
orange fruit
.RS
.IP "1." 3
sublist
.IP "2." 3
sublist
.RE
.SH 1
HTML Blocks
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
This is \f[I]emphasized\f[]
And this is \f[B]strong\f[]
.LP
Here's a simple block:
.LP
foo
.LP
This should be a code block, though:
.IP
.nf
\f[C]
<div>
\ \ \ \ foo
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
.LP
Multiline:
.LP
Code block:
.IP
.nf
\f[C]
<!\-\-\ Comment\ \-\->
\f[]
.fi
.LP
Just plain comment, with trailing spaces on the line:
.LP
Code:
.IP
.nf
\f[C]
<hr\ />
\f[]
.fi
.LP
Hr's:
.PP
   *   *   *   *   *
.SH 1
Inline Markup
.LP
This is \f[I]emphasized\f[], and so \f[I]is this\f[].
.LP
This is \f[B]strong\f[], and so \f[B]is this\f[].
.LP
An \f[I]emphasized link\**\f[].
.FS
/url
.FE
.LP
\f[B]\f[BI]This is strong and em.\f[B]\f[]
.LP
So is \f[B]\f[BI]this\f[B]\f[] word.
.LP
\f[B]\f[BI]This is strong and em.\f[B]\f[]
.LP
So is \f[B]\f[BI]this\f[B]\f[] word.
.LP
This is code: \f[C]>\f[], \f[C]$\f[], \f[C]\\\f[], \f[C]\\$\f[],
\f[C]<html>\f[].
.LP
[STRIKEOUT:This is \f[I]strikeout\f[].]
.LP
Superscripts: a\*{bc\*}d a\*{\f[I]hello\f[]\*} a\*{hello\ there\*}.
.LP
Subscripts: H\*<2\*>O, H\*<23\*>O, H\*<many\ of\ them\*>O.
.LP
These should not be superscripts or subscripts,
because of the unescaped spaces: a^b c^d, a~b c~d.
.PP
   *   *   *   *   *
.SH 1
Smart quotes, ellipses, dashes
.LP
\[lq]Hello,\[rq] said the spider.
\[lq]`Shelob' is my name.\[rq]
.LP
`A', `B', and `C' are letters.
.LP
`Oak,' `elm,' and `beech' are names of trees.
So is `pine.'
.LP
`He said, \[lq]I want to go.\[rq]' Were you alive in the
70's?
.LP
Here is some quoted `\f[C]code\f[]' and a \[lq]quoted link\**\[rq].
.FS
http://example.com/?foo=1&bar=2
.FE
.LP
Some dashes: one\[em]two \[em] three\[em]four \[em] five.
.LP
Dashes between numbers: 5\[en]7, 255\[en]66, 1987\[en]1999.
.LP
Ellipses\&...and\&...and\&....
.PP
   *   *   *   *   *
.SH 1
LaTeX
.IP \[bu] 2
.IP \[bu] 2
|2 + 2 = 4|
.IP \[bu] 2
|x \[u2208] y|
.IP \[bu] 2
|alpha \[u2227] omega|
.IP \[bu] 2
|223|
.IP \[bu] 2
|p|\-Tree
.IP \[bu] 2
Here's some display math:
.EQ
d over {d x} f ( x ) = lim sub {h -> 0} {f ( x + h ) \[u2212] f ( x )} over h
.EN
.IP \[bu] 2
Here's one that has a line break in it: |alpha + omega times x sup 2|.
.LP
These shouldn't be math:
.IP \[bu] 2
To get the famous equation, write \f[C]$e\ =\ mc^2$\f[].
.IP \[bu] 2
$22,000 is a \f[I]lot\f[] of money.
So is $34,000.
(It worked if \[lq]lot\[rq] is emphasized.)
.IP \[bu] 2
Shoes ($20) and socks ($5).
.IP \[bu] 2
Escaped \f[C]$\f[]: $73 \f[I]this should be emphasized\f[] 23$.
.LP
Here's a LaTeX table:
.PP
   *   *   *   *   *
.SH 1
Special Characters
.LP
Here is some unicode:
.IP \[bu] 2
I hat: Î
.IP \[bu] 2
o umlaut: ö
.IP \[bu] 2
section: §
.IP \[bu] 2
set membership: ∈
.IP \[bu] 2
copyright: ©
.LP
AT&T has an ampersand in their name.
.LP
AT&T is another way to write it.
.LP
This & that.
.LP
4 < 5.
.LP
6 > 5.
.LP
Backslash: \\
.LP
Backtick: `
.LP
Asterisk: *
.LP
Underscore: _
.LP
Left brace: {
.LP
Right brace: }
.LP
Left bracket: [
.LP
Right bracket: ]
.LP
Left paren: (
.LP
Right paren: )
.LP
Greater\-than: >
.LP
Hash: #
.LP
Period: .
.LP
Bang: !
.LP
Plus: +
.LP
Minus: \-
.PP
   *   *   *   *   *
.SH 1
Links
.SH 2
Explicit
.LP
Just a URL\**.
.FS
/url/
.FE
.LP
URL and title\**.
.FS
/url/
.FE
.LP
URL and title\**.
.FS
/url/
.FE
.LP
URL and title\**.
.FS
/url/
.FE
.LP
URL and title\**
.FS
/url/
.FE
.LP
URL and title\**
.FS
/url/
.FE
.LP
with_underscore\**
.FS
/url/with_underscore
.FE
.LP
Email link\**
.FS
mailto:nobody\@nowhere.net
.FE
.LP
Empty\**.
.FS
.FE
.SH 2
Reference
.LP
Foo bar\**.
.FS
/url/
.FE
.LP
Foo bar\**.
.FS
/url/
.FE
.LP
Foo bar\**.
.FS
/url/
.FE
.LP
With embedded [brackets]\**.
.FS
/url/
.FE
.LP
b\**
.FS
/url/
.FE
by itself should be a link.
.LP
Indented once\**.
.FS
/url
.FE
.LP
Indented twice\**.
.FS
/url
.FE
.LP
Indented thrice\**.
.FS
/url
.FE
.LP
This should [not][] be a link.
.IP
.nf
\f[C]
[not]:\ /url
\f[]
.fi
.LP
Foo bar\**.
.FS
/url/
.FE
.LP
Foo biz\**.
.FS
/url/
.FE
.SH 2
With ampersands
.LP
Here's a link with an ampersand in the URL\**.
.FS
http://example.com/?foo=1&bar=2
.FE
.LP
Here's a link with an amersand in the link text: AT&T\**.
.FS
http://att.com/
.FE
.LP
Here's an inline link\**.
.FS
/script?foo=1&bar=2
.FE
.LP
Here's an inline link in pointy braces\**.
.FS
/script?foo=1&bar=2
.FE
.SH 2
Autolinks
.LP
With an ampersand: http://example.com/?foo=1&bar=2
.IP \[bu] 2
In a list?
.IP \[bu] 2
http://example.com/
.IP \[bu] 2
It should.
.LP
An e\-mail address: nobody\@nowhere.net
.RS
.LP
Blockquoted: http://example.com/
.RE
.LP
Auto\-links should not occur here: \f[C]<http://example.com/>\f[]
.IP
.nf
\f[C]
or\ here:\ <http://example.com/>
\f[]
.fi
.PP
   *   *   *   *   *
.SH 1
Images
.LP
From \[lq]Voyage dans la Lune\[rq] by Georges Melies (1902):
.LP
[IMAGE: lalune\**]
.FS
lalune.jpg
.FE
.LP
Here is a movie [IMAGE: movie\**]
.FS
movie.jpg
.FE
icon.
.PP
   *   *   *   *   *
.SH 1
Footnotes
.LP
Here is a footnote reference,\**
.FS
Here is the footnote.
It can go anywhere after the footnote
reference.
It need not be placed at the end of the document.
.FE
and another.\**
.FS
Here's the long note.
This one contains multiple
blocks.
.LP
Subsequent blocks are indented to show that they belong to the
footnote (as with list items).
.IP
.nf
\f[C]
\ \ {\ <code>\ }
\f[]
.fi
.LP
If you want, you can indent every line, but you can also be
lazy and just indent the first line of each block.
.FE
This should \f[I]not\f[] be a footnote reference, because it
contains a space.[^my note] Here is an inline note.\**
.FS
This
is \f[I]easier\f[] to type.
Inline notes may contain
links (http://google.com) and \f[C]]\f[] verbatim characters,
as well as [bracketed text].
.FE
.RS
.LP
Notes can go in quotes.\**
.FS
In quote.
.FE
.RE
.IP "1." 3
And in list items.\**
.FS
In list.
.FE
.LP
This paragraph should not be part of the note, as it is not indented.
