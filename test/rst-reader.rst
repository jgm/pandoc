Pandoc Test Suite
#################
Subtitle
^^^^^^^^

:Authors: John MacFarlane; Anonymous
:Date: July 17, 2006
:Revision: 3

Level one header
================

This is a set of tests for pandoc.  Most of them are adapted from
John Gruber's markdown test suite.

Level two header
----------------

Level three
+++++++++++

Level four with *emphasis*
~~~~~~~~~~~~~~~~~~~~~~~~~~

Level five
''''''''''

Paragraphs
==========

Here's a regular paragraph.

In Markdown 1.0.0 and earlier. Version
8. This line turns into a list item.
Because a hard-wrapped line in the
middle of a paragraph looked like a
list item.

Here's one with a bullet.
* criminey.

Horizontal rule:

-----

Another:

****

Block Quotes
============

Here's a block quote:

  This is a block quote.
  It is pretty short.

Here's another, differently indented:

    This is a block quote.
    It's indented with a tab.

    Code in a block quote:: 

        sub status {
            print "working";
        }

    List in a block quote:

    1. item one
    2. item two

    Nested block quotes:

        nested

            nested

Code Blocks
===========

Code:

::

    ---- (should be four hyphens)

    sub status {
        print "working";
    }

::

	this code block is indented by one tab

And::

		this block is indented by two tabs

        These should not be escaped:  \$ \\ \> \[ \{

And:

.. code-block:: python

   def my_function(x):
       return x + 1

If we use the highlight directive, we can specify a default language
for literate blocks.

.. highlight:: haskell

::

  -- this code is in haskell
  data Tree = Leaf | Node Tree Tree

::

  -- this code is in haskell too
  data Nat = Zero | Succ Nat

.. highlight:: javascript

::

  -- this code is in javascript
  let f = (x, y) => x + y

.. highlight::

Lists
=====

Unordered
---------

Asterisks tight:

*	asterisk 1
*	asterisk 2
*	asterisk 3

Asterisks loose:

*	asterisk 1

*	asterisk 2

*	asterisk 3

Pluses tight:

+	Plus 1
+	Plus 2
+	Plus 3

Pluses loose:

+	Plus 1

+	Plus 2

+	Plus 3

Minuses tight:

-	Minus 1
-	Minus 2
-	Minus 3

Minuses loose:

-	Minus 1

-	Minus 2

-	Minus 3

Ordered
-------

Tight:

1.	First
2.	Second
3.	Third

and:

1. One
2. Two
3. Three

Loose using tabs:

1.	First

2.	Second

3.	Third

and using spaces:

1. One

2. Two

3. Three

Multiple paragraphs:

1.	Item 1, graf one.

	Item 1. graf two. The quick brown fox jumped over the lazy dog's
	back.

2.	Item 2.

3.	Item 3.

Nested:

*	Tab

	*	Tab

		*	Tab

Here's another:

1. First

2. Second:

	* Fee
	* Fie
	* Foe

3. Third 

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
            
            c) Lower alpha with paren

Autonumbering:

#. Autonumber.
#.  More.
    
    #. Nested.

Autonumbering with explicit start:

(d)  item 1
(#)  item 2

Definition
----------

term 1
    Definition 1.

term 2
    Definition 2, paragraph 1.

    Definition 2, paragraph 2.

term with *emphasis* 
    Definition 3.

Field Lists
===========

 :address:  61 Main St.
 :city:  *Nowhere*, MA,
    USA
 :phone: 123-4567

:address:  61 Main St.
:city:  *Nowhere*, MA,
    USA
:phone:
  123-4567

HTML Blocks
===========

Simple block on one line:

.. raw:: html

    <div>foo</div>

Now, nested:

.. raw:: html

    <div>
	    <div>
		    <div>
			    foo
		    </div>
	    </div>
    </div>

LaTeX Block
===========

.. raw:: latex

   \begin{tabular}{|l|l|}\hline
   Animal & Number \\ \hline
   Dog    & 2      \\
   Cat    & 1      \\ \hline
   \end{tabular}

Inline Markup
=============

This is *emphasized*. This is **strong**.

This is code: ``>``, ``$``, ``\``, ``\$``, ``<html>``.

This is\ :sub:`subscripted` and this is :sup:`superscripted`\ .

Special Characters
==================

Here is some unicode:

- I hat: Î
- o umlaut: ö
- section: § 
- set membership: ∈
- copyright: ©

AT&T has an ampersand in their name.

This & that.

4 < 5.

6 > 5.

Backslash: \\

Backtick: \`

Asterisk: \*

Underscore: \_

Left brace: \{

Right brace: \}

Left bracket: \[

Right bracket: \]

Left paren: \(

Right paren: \)

Greater-than: \>

Hash: \#

Period: \.

Bang: \!

Plus: \+

Minus: \-

Links
=====

Explicit:  a `URL </url/>`_.

Explicit with no label:  `<foo>`_.

Two anonymous links:  `the first`__ and `the second`__

__ /url1/
__ /url2/

Reference links:  `link1`_ and `link2`_ and link1_ again.

.. _link1: /url1/
.. _`link2`: /url2/

Another `style of reference link <link1_>`_.

Here's a `link with an ampersand in the URL`_.

Here's a link with an amersand in the link text: `AT&T </url/>`_.

.. _link with an ampersand in the URL: http://example.com/?foo=1&bar=2

Autolinks: http://example.com/?foo=1&bar=2 and nobody@nowhere.net.

But not here::

    http://example.com/

Images
======

From "Voyage dans la Lune" by Georges Melies (1902):

.. image:: lalune.jpg

.. image:: lalune.jpg
   :height: 2343
   :alt: Voyage dans la Lune

Here is a movie |movie| icon.

.. |movie| image:: movie.jpg

And an |image with a link|.

.. |image with a link| image:: movie.jpg
   :alt:  A movie
   :target: /url

Comments
========

First paragraph

.. comment

..
    Comment block, should not appear in output
    as defined by reStructuredText

Another paragraph

..
    Another comment block.

    This one spans several
    text elements.

    It doesn't end until
    indentation is restored to the
    preceding level.

A third paragraph

Line blocks
===========

| But can a bee be said to be
|     or not to be an entire bee,
|         when half the bee is not a bee,
|             due to some ancient injury?
|
| Continuation
 line
|   and
       another

Simple Tables
=============

==================  ===========  ==========
col 1               col 2        col 3 
==================  ===========  ==========
r1 a                b            c
r2 d                e            f
==================  ===========  ==========

Headless

==================  ===========  ==========
r1 a                b            c
r2 d                e            f
==================  ===========  ==========


Grid Tables
===========

+------------------+-----------+------------+
| col 1            | col 2     | col 3      |
+==================+===========+============+
| r1 a             | b         | c          |
| r1 bis           | b 2       | c 2        |
+------------------+-----------+------------+
| r2 d             | e         | f          |
+------------------+-----------+------------+

Headless

+------------------+-----------+------------+
| r1 a             | b         | c          |
| r1 bis           | b 2       | c 2        |
+------------------+-----------+------------+
| r2 d             | e         | f          |
+------------------+-----------+------------+

Spaces at ends of lines

+------------------+-----------+------------+  
| r1 a             | b         | c          |
| r1 bis           | b 2       | c 2        | 
+------------------+-----------+------------+
| r2 d             | e         | f          |
+------------------+-----------+------------+

Multiple blocks in a cell

+------------------+-----------+------------+  
| r1 a             | - b       | c          |
|                  | - b 2     | c 2        | 
| r1 bis           | - b 2     | c 2        | 
+------------------+-----------+------------+

Footnotes
=========

[1]_

[#]_

[#]_

[*]_

.. [1] Note with one line.

.. [#] Note with
  continuation line.

.. [#] Note with

  continuation block.

.. [*] Note with
   continuation line

   and a second para.

Not in note.

Math
====

Some inline math :math:`E=mc^2`\ .  Now some
display math:

.. math:: E=mc^2

.. math::

   E = mc^2

.. math::

   E = mc^2

   \alpha = \beta

.. math::
   :label: hithere
   :nowrap:

   E &= mc^2\\
   F &= \pi E

   F &= \gamma \alpha^2

All done.

Default-Role
============

Try changing the default role to a few different things.

.. default-role:: math

Doesn't Break Title Parsing
---------------------------

Inline math: `E=mc^2` or :math:`E=mc^2` or `E=mc^2`:math:.
Other roles: :sup:`super`, `sub`:sub:.

.. math::
    \alpha = beta

    E = mc^2

.. default-role:: sup

Some `of` these :sup:`words` are in `superscript`:sup:.

Reset default-role to the default default.

.. default-role::

And now `some-invalid-string-3231231` is nonsense.

.. role:: html(raw)
   :format: html

And now with :html:`<b>inline</b> <span id="test">HTML</span>`.

.. role:: haskell(code)
   :language: haskell

And some inline haskell :haskell:`fmap id [1,2..10]`.

.. role:: indirect(code)

.. role:: py(indirect)
   :language: python

Indirect python role :py:`[x*x for x in [1,2,3,4,5]]`.

.. role:: different-indirect(code)
   :language: c

.. role:: c(different-indirect)

Different indirect C :c:`int x = 15;`.

Literal symbols
---------------

2*2 = 4*1
