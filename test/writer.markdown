---
author:
- John MacFarlane
- Anonymous
date: 'July 17, 2006'
title: Pandoc Test Suite
---

This is a set of tests for pandoc. Most of them are adapted from John Gruber's
markdown test suite.

------------------------------------------------------------------------------

Headers
=======

Level 2 with an [embedded link](/url)
-------------------------------------

### Level 3 with *emphasis*

#### Level 4

##### Level 5

Level 1
=======

Level 2 with *emphasis*
-----------------------

### Level 3

with no blank line

Level 2
-------

with no blank line

------------------------------------------------------------------------------

Paragraphs
==========

Here's a regular paragraph.

In Markdown 1.0.0 and earlier. Version 8. This line turns into a list item.
Because a hard-wrapped line in the middle of a paragraph looked like a list
item.

Here's one with a bullet. \* criminey.

There should be a hard line break\
here.

------------------------------------------------------------------------------

Block Quotes
============

E-mail style:

> This is a block quote. It is pretty short.

> Code in a block quote:
>
>     sub status {
>         print "working";
>     }
>
> A list:
>
> 1.  item one
> 2.  item two
>
> Nested block quotes:
>
> > nested
>
> > nested

This should not be a block quote: 2 \> 1.

And a following paragraph.

------------------------------------------------------------------------------

Code Blocks
===========

Code:

    ---- (should be four hyphens)

    sub status {
        print "working";
    }

    this code block is indented by one tab

And:

        this code block is indented by two tabs

    These should not be escaped:  \$ \\ \> \[ \{

------------------------------------------------------------------------------

Lists
=====

Unordered
---------

Asterisks tight:

-   asterisk 1
-   asterisk 2
-   asterisk 3

Asterisks loose:

-   asterisk 1

-   asterisk 2

-   asterisk 3

Pluses tight:

-   Plus 1
-   Plus 2
-   Plus 3

Pluses loose:

-   Plus 1

-   Plus 2

-   Plus 3

Minuses tight:

-   Minus 1
-   Minus 2
-   Minus 3

Minuses loose:

-   Minus 1

-   Minus 2

-   Minus 3

Ordered
-------

Tight:

1.  First
2.  Second
3.  Third

and:

1.  One
2.  Two
3.  Three

Loose using tabs:

1.  First

2.  Second

3.  Third

and using spaces:

1.  One

2.  Two

3.  Three

Multiple paragraphs:

1.  Item 1, graf one.

    Item 1. graf two. The quick brown fox jumped over the lazy dog's back.

2.  Item 2.

3.  Item 3.

Nested
------

-   Tab
    -   Tab
        -   Tab

Here's another:

1.  First
2.  Second:
    -   Fee
    -   Fie
    -   Foe
3.  Third

Same thing but with paragraphs:

1.  First

2.  Second:

    -   Fee
    -   Fie
    -   Foe

3.  Third

Tabs and spaces
---------------

-   this is a list item indented with tabs

-   this is a list item indented with spaces

    -   this is an example list item indented with tabs

    -   this is an example list item indented with spaces

Fancy list markers
------------------

(2) begins with 2

(3) and now 3

    with a continuation

    iv. sublist with roman numerals, starting with 4
    v.  more items
        (A) a subsublist
        (B) a subsublist

Nesting:

A.  Upper Alpha
    I.  Upper Roman.
        (6) Decimal start with 6
            c)  Lower alpha with paren

Autonumbering:

1.  Autonumber.
2.  More.
    1.  Nested.

Should not be a list item:

M.A. 2007

B. Williams

------------------------------------------------------------------------------

Definition Lists
================

Tight using spaces:

apple
:   red fruit

orange
:   orange fruit

banana
:   yellow fruit

Tight using tabs:

apple
:   red fruit

orange
:   orange fruit

banana
:   yellow fruit

Loose:

apple

:   red fruit

orange

:   orange fruit

banana

:   yellow fruit

Multiple blocks with italics:

*apple*

:   red fruit

    contains seeds, crisp, pleasant to taste

*orange*

:   orange fruit

        { orange code block }

    > orange block quote

Multiple definitions, tight:

apple
:   red fruit
:   computer

orange
:   orange fruit
:   bank

Multiple definitions, loose:

apple

:   red fruit

:   computer

orange

:   orange fruit

:   bank

Blank line after term, indented marker, alternate markers:

apple

:   red fruit

:   computer

orange

:   orange fruit

    1.  sublist
    2.  sublist

HTML Blocks
===========

Simple block on one line:

<div>

foo

</div>

And nested without indentation:

<div>

<div>

<div>

foo

</div>

</div>

<div>

bar

</div>

</div>

Interpreted markdown in a table:

```{=html}
<table>
```
```{=html}
<tr>
```
```{=html}
<td>
```
This is *emphasized*
```{=html}
</td>
```
```{=html}
<td>
```
And this is **strong**
```{=html}
</td>
```
```{=html}
</tr>
```
```{=html}
</table>
```
```{=html}
<script type="text/javascript">document.write('This *should not* be interpreted as markdown');</script>
```
Here's a simple block:

<div>

foo

</div>

This should be a code block, though:

    <div>
        foo
    </div>

As should this:

    <div>foo</div>

Now, nested:

<div>

<div>

<div>

foo

</div>

</div>

</div>

This should just be an HTML comment:

```{=html}
<!-- Comment -->
```
Multiline:

```{=html}
<!--
Blah
Blah
-->
```
```{=html}
<!--
    This is another comment.
-->
```
Code block:

    <!-- Comment -->

Just plain comment, with trailing spaces on the line:

```{=html}
<!-- foo -->
```
Code:

    <hr />

Hr's:

```{=html}
<hr>
```
```{=html}
<hr />
```
```{=html}
<hr />
```
```{=html}
<hr>
```
```{=html}
<hr />
```
```{=html}
<hr />
```
```{=html}
<hr class="foo" id="bar" />
```
```{=html}
<hr class="foo" id="bar" />
```
```{=html}
<hr class="foo" id="bar">
```

------------------------------------------------------------------------------

Inline Markup
=============

This is *emphasized*, and so *is this*.

This is **strong**, and so **is this**.

An *[emphasized link](/url)*.

***This is strong and em.***

So is ***this*** word.

***This is strong and em.***

So is ***this*** word.

This is code: `>`, `$`, `\`, `\$`, `<html>`.

~~This is *strikeout*.~~

Superscripts: a^bc^d a^*hello*^ a^hello there^.

Subscripts: H~2~O, H~23~O, H~many of them~O.

These should not be superscripts or subscripts, because of the unescaped
spaces: a\^b c\^d, a\~b c\~d.

------------------------------------------------------------------------------

Smart quotes, ellipses, dashes
==============================

"Hello," said the spider. "'Shelob' is my name."

'A', 'B', and 'C' are letters.

'Oak,' 'elm,' and 'beech' are names of trees. So is 'pine.'

'He said, "I want to go."' Were you alive in the 70's?

Here is some quoted '`code`' and a "[quoted
link](http://example.com/?foo=1&bar=2)".

Some dashes: one---two --- three---four --- five.

Dashes between numbers: 5--7, 255--66, 1987--1999.

Ellipses...and...and....

------------------------------------------------------------------------------

LaTeX
=====

-   `\cite[22-23]{smith.1899}`{=tex}
-   $2+2=4$
-   $x \in y$
-   $\alpha \wedge \omega$
-   $223$
-   $p$-Tree
-   Here's some display math:
    $$\frac{d}{dx}f(x)=\lim_{h\to 0}\frac{f(x+h)-f(x)}{h}$$
-   Here's one that has a line break in it: $\alpha + \omega \times x^2$.

These shouldn't be math:

-   To get the famous equation, write `$e = mc^2$`.
-   \$22,000 is a *lot* of money. So is \$34,000. (It worked if "lot" is
    emphasized.)
-   Shoes (\$20) and socks (\$5).
-   Escaped `$`: \$73 *this should be emphasized* 23\$.

Here's a LaTeX table:

```{=tex}
\begin{tabular}{|l|l|}\hline
Animal & Number \\ \hline
Dog    & 2      \\
Cat    & 1      \\ \hline
\end{tabular}
```

------------------------------------------------------------------------------

Special Characters
==================

Here is some unicode:

-   I hat: Î
-   o umlaut: ö
-   section: §
-   set membership: ∈
-   copyright: ©

AT&T has an ampersand in their name.

AT&T is another way to write it.

This & that.

4 \< 5.

6 \> 5.

Backslash: \\

Backtick: \`

Asterisk: \*

Underscore: \_

Left brace: {

Right brace: }

Left bracket: \[

Right bracket: \]

Left paren: (

Right paren: )

Greater-than: \>

Hash: \#

Period: .

Bang: !

Plus: +

Minus: -

------------------------------------------------------------------------------

Links
=====

Explicit
--------

Just a [URL](/url/).

[URL and title](/url/ "title").

[URL and title](/url/ "title preceded by two spaces").

[URL and title](/url/ "title preceded by a tab").

[URL and title](/url/ "title with "quotes" in it")

[URL and title](/url/ "title with single quotes")

[with_underscore](/url/with_underscore)

[Email link](mailto:nobody@nowhere.net)

[Empty]().

Reference
---------

Foo [bar](/url/).

With [embedded \[brackets\]](/url/).

[b](/url/) by itself should be a link.

Indented [once](/url).

Indented [twice](/url).

Indented [thrice](/url).

This should \[not\]\[\] be a link.

    [not]: /url

Foo [bar](/url/ "Title with "quotes" inside").

Foo [biz](/url/ "Title with "quote" inside").

With ampersands
---------------

Here's a [link with an ampersand in the URL](http://example.com/?foo=1&bar=2).

Here's a link with an amersand in the link text:
[AT&T](http://att.com/ "AT&T").

Here's an [inline link](/script?foo=1&bar=2).

Here's an [inline link in pointy braces](/script?foo=1&bar=2).

Autolinks
---------

With an ampersand: <http://example.com/?foo=1&bar=2>

-   In a list?
-   <http://example.com/>
-   It should.

An e-mail address: <nobody@nowhere.net>

> Blockquoted: <http://example.com/>

Auto-links should not occur here: `<http://example.com/>`

    or here: <http://example.com/>

------------------------------------------------------------------------------

Images
======

From "Voyage dans la Lune" by Georges Melies (1902):

![lalune](lalune.jpg "Voyage dans la Lune")

Here is a movie ![movie](movie.jpg) icon.

------------------------------------------------------------------------------

Footnotes
=========

Here is a footnote reference,[^1] and another.[^2] This should *not* be a
footnote reference, because it contains a space.\[\^my note\] Here is an
inline note.[^3]

> Notes can go in quotes.[^4]

1.  And in list items.[^5]

This paragraph should not be part of the note, as it is not indented.

[^1]: Here is the footnote. It can go anywhere after the footnote reference.
    It need not be placed at the end of the document.

[^2]: Here's the long note. This one contains multiple blocks.

    Subsequent blocks are indented to show that they belong to the footnote
    (as with list items).

          { <code> }

    If you want, you can indent every line, but you can also be lazy and just
    indent the first line of each block.

[^3]: This is *easier* to type. Inline notes may contain
    [links](http://google.com) and `]` verbatim characters, as well as
    \[bracketed text\].

[^4]: In quote.

[^5]: In list.
