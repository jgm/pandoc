=================
Pandoc Test Suite
=================

:Author: John MacFarlane
:Author: Anonymous
:Date: July 17, 2006

This is a set of tests for pandoc. Most of them are adapted from
John Gruber's markdown test suite.

--------------

Headers
=======

Level 2 with an `embedded link`_
--------------------------------

Level 3 with *emphasis*
~~~~~~~~~~~~~~~~~~~~~~~

Level 4
^^^^^^^

Level 5
'''''''

Level 1
=======

Level 2 with *emphasis*
-----------------------

Level 3
~~~~~~~

with no blank line

Level 2
-------

with no blank line

--------------

Paragraphs
==========

Here's a regular paragraph.

In Markdown 1.0.0 and earlier. Version 8. This line turns into a
list item. Because a hard-wrapped line in the middle of a paragraph
looked like a list item.

Here's one with a bullet. \* criminey.

There should be a hard line break here.

--------------

Block Quotes
============

E-mail style:

    This is a block quote. It is pretty short.


    Code in a block quote:

    ::
 
        sub status {
            print "working";
        }
        
    A list:

    1.  item one
    2.  item two

    Nested block quotes:

        nested


        nested



This should not be a block quote: 2 > 1.

Box-style:

    Example:

    ::
 
        sub status {
            print "working";
        }
        

    1.  do laundry
    2.  take out the trash


Here's a nested one:

    Joe said:

        Don't quote me.



And a following paragraph.

--------------

Code Blocks
===========

Code:

::
 
    ---- (should be four hyphens)
    
    sub status {
        print "working";
    }
    
    this code block is indented by one tab
    
And:

::
 
        this code block is indented by two tabs
    
    These should not be escaped:  \$ \\ \> \[ \{
    
--------------

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

    Item 1. graf two. The quick brown fox jumped over the lazy dog's
    back.

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



--------------

HTML Blocks
===========

Simple block on one line:


.. raw:: html

   <div>
   
foo

.. raw:: html

   </div>
   
And nested without indentation:


.. raw:: html

   <div>
   <div>
   <div>
   
foo

.. raw:: html

   </div>
   </div>
   <div>
   
bar

.. raw:: html

   </div>
   </div>
   
Interpreted markdown in a table:


.. raw:: html

   <table>
   <tr>
   <td>
   
This is *emphasized*

.. raw:: html

   </td>
   <td>
   
And this is **strong**

.. raw:: html

   </td>
   </tr>
   </table>
   
   <script type="text/javascript">document.write('This *should not* be interpreted as markdown');</script>
   
Here's a simple block:


.. raw:: html

   <div>
       
   
foo

.. raw:: html

   </div>
   
This should be a code block, though:

::
 
    <div>
        foo
    </div>
    
As should this:

::
 
    <div>foo</div>
    
Now, nested:


.. raw:: html

   <div>
       <div>
           <div>
               
   
foo

.. raw:: html

   </div>
       </div>
   </div>
   
This should just be an HTML comment:


.. raw:: html

   <!-- Comment -->
   
Multiline:


.. raw:: html

   <!--
   Blah
   Blah
   -->
   
   <!--
       This is another comment.
   -->
   
Code block:

::
 
    <!-- Comment -->
    
Just plain comment, with trailing spaces on the line:


.. raw:: html

   <!-- foo -->   
   
Code:

::
 
    <hr />
    
Hr's:


.. raw:: html

   <hr>
   
   <hr />
   
   <hr />
   
   <hr>   
   
   <hr />  
   
   <hr /> 
   
   <hr class="foo" id="bar" />
   
   <hr class="foo" id="bar" />
   
   <hr class="foo" id="bar">
   
--------------

Inline Markup
=============

This is *emphasized*, and so *is this*.

This is **strong**, and so **is this**.

An *`emphasized link`_*.

***This is strong and em.***

So is ***this*** word.

***This is strong and em.***

So is ***this*** word.

This is code: ``>``, ``$``, ``\``, ``\$``, ``<html>``.

--------------

Smart quotes, ellipses, dashes
==============================

"Hello," said the spider. "'Shelob' is my name."

'A', 'B', and 'C' are letters.

'Oak,' 'elm,' and 'beech' are names of trees. So is 'pine.'

'He said, "I want to go."' Were you alive in the 70's?

Here is some quoted '``code``' and a "`quoted link`_".

Some dashes: one---two --- three--four -- five.

Dashes between numbers: 5-7, 255-66, 1987-1999.

Ellipses...and. . .and . . . .

--------------

LaTeX
=====

-   \cite[22-23]{smith.1899}
-   \doublespacing
-   $2+2=4$
-   $x \in y$
-   $\alpha \wedge \omega$
-   $223$
-   $p$-Tree
-   $\frac{d}{dx}f(x)=\lim_{h\to 0}\frac{f(x+h)-f(x)}{h}$
-   Here's one that has a line break in it:
    $\alpha + \omega \times x^2$.

These shouldn't be math:

-   To get the famous equation, write ``$e = mc^2$``.
-   $22,000 is a *lot* of money. So is $34,000. (It worked if "lot"
    is emphasized.)
-   Escaped ``$``: $73 *this should be emphasized* 23$.

Here's a LaTeX table:


.. raw:: latex

   \begin{tabular}{|l|l|}\hline
   Animal & Number \\ \hline
   Dog    & 2      \\
   Cat    & 1      \\ \hline
   \end{tabular}
   
--------------

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

4 < 5.

6 > 5.

Backslash: \\

Backtick: \`

Asterisk: \*

Underscore: \_

Left brace: {

Right brace: }

Left bracket: [

Right bracket: ]

Left paren: (

Right paren: )

Greater-than: >

Hash: #

Period: .

Bang: !

Plus: +

Minus: -

--------------

Links
=====

Explicit
--------

Just a `URL`_.

`URL and title`_.

`URL and title`_.

`URL and title`_.

`URL and title`_

`URL and title`_

`Email link`_

`Empty`_.

Reference
---------

Foo `bar`_.

Foo `bar`_.

Foo `bar`_.

With `embedded [brackets]`_.

`b`_ by itself should be a link.

Indented `once`_.

Indented `twice`_.

Indented `thrice`_.

This should [not] be a link.

::
 
    [not]: /url
    
Foo `bar`_.

Foo `biz`_.

With ampersands
---------------

Here's a `link with an ampersand in the URL`_.

Here's a link with an amersand in the link text: `AT&T`_.

Here's an `inline link`_.

Here's an `inline link in pointy braces`_.

Autolinks
---------

With an ampersand: `http://example.com/?foo=1&bar=2`_

-   In a list?
-   `http://example.com/`_
-   It should.

An e-mail address: `nobody@nowhere.net`_

    Blockquoted: `http://example.com/`_


Auto-links should not occur here: ``<http://example.com/>``

::
 
    or here: <http://example.com/>
    
--------------

Images
======

From "Voyage dans la Lune" by Georges Melies (1902):

|lalune|

Here is a movie |movie| icon.

--------------

Footnotes
=========

Here is a footnote reference [1]_, and another [longnote]_. This
should *not* be a footnote reference, because it contains a
space^(my note).

.. [1] 
   Here is the footnote. It can go anywhere in the document, not just
   at the end.

.. [longnote] 
   Here's the other note. This one contains multiple blocks.

   Caret characters are used to indicate that the blocks all belong to
   a single footnote (as with block quotes).

   ::
 
         { <code> }
       
   If you want, you can use a caret at the beginning of every line, as
   with blockquotes, but all that you need is a caret at the beginning
   of the first line of the block and any preceding blank lines.


.. _embedded link: /url
.. _emphasized link: /url
.. _quoted link: http://example.com/?foo=1&bar=2
.. _URL: /url/
.. _URL and title: /url/
.. _Email link: mailto:nobody@nowhere.net
.. _Empty: 
.. _bar: /url/
.. _embedded [brackets]: /url/
.. _b: /url/
.. _once: /url
.. _twice: /url
.. _thrice: /url
.. _biz: /url/
.. _link with an ampersand in the URL: http://example.com/?foo=1&bar=2
.. _AT&T: http://att.com/
.. _inline link: /script?foo=1&bar=2
.. _inline link in pointy braces: /script?foo=1&bar=2
.. _`http://example.com/?foo=1&bar=2`: http://example.com/?foo=1&bar=2
.. _`http://example.com/`: http://example.com/
.. _nobody@nowhere.net: mailto:nobody@nowhere.net
.. |lalune| image:: lalune.jpg
.. |movie| image:: movie.jpg
