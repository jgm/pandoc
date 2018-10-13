% Lua filter types and objects
% Albert Krewinkel
% Oct 6, 2018

## Pandoc

blocks
:   document content ([list] of [blocks])

meta
:   document meta information ([Meta] object)


## Meta

Meta information on a document; string-indexed collection of [meta
values](#metavalue). This is represented as a string-indexed table containing
[meta values](#MetaValue).


## MetaValue

Document meta information items.

### MetaBlocks

blocks
:   a list of blocks usable as meta value ([list] of [blocks])


## Block

### BlockQuote

A block quote element

content:
:   block content ([list] of [blocks])

tag, t
:   the literal `BlockQuote` (string)

### BulletList

A bullet list

content
:   list of items ([list] of [blocks])

tag, t
:   the literal `BlockQuote` (string)

### CodeBlock

Block of code.

text
:   code string (string)

attr
:   element attributes (Attr)

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `CodeBlock` (string)

### DefinitionList

Definition list, containing terms and their explanation.

content
:   list of items

tag, t
:   the literal `DefinitionList` (string)

### Div

Generic block container with attributes

content
:   block content ([list] of [blocks])

attr
:   element attributes (Attr)

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `Div` (string)

### Header

Creates a header element.

level
:   header level (integer)

content
:   inline content ([list] of [inlines])

attr
:   element attributes (Attr)

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `Header` (string)


### HorizontalRule

A horizontal rule.

tag, t
:   the literal `HorizontalRule` (string)

### LineBlock

A line block, i.e. a list of lines, each separated from the next
by a newline.

content
:   inline content

tag, t
:   the literal `LineBlock` (string)

### Null

A null element; this element never produces any output in the
target format.

tag, t
:   the literal `Null` (string)

### OrderedList

An ordered list.

Parameters:

items
:   list items ([list] of [blocks])

listAttributes
:   list parameters (ListAttributes)

start
:   alias for `listAttributes.start` (integer)

style
:   alias for `listAttributes.style` (string)

delimiter
:   alias for `listAttributes.delimiter` (string)

tag, t
:   the literal `OrderedList` (string)

### Para

A paragraph

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Para` (string)

### Plain

Plain text, not a paragraph

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Plain` (string)

### RawBlock

Raw content of a specified format.

format
:   format of content (string)

text
:   raw content (string)

tag, t
:   the literal `RawBlock` (string)

### Table

A table.

caption:
:   table caption

aligns:
:   alignments

widths:
:   column widths

headers
:   header row

rows:
:   table rows

tag, t
:   the literal `Table` (string)

## Inline

### Cite
Citation

content
:   ([list] of [inlines])

citations
:   citation entries ([list] of [citations])

tag, t
:   the literal `Cite` (string)

### Code
Inline code

text
:   code string (string)

attr
:   attributes ([Attr])

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `Code` (string)

### Emph
Emphasized text

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Emph` (string)

### Image
Image:  alt text (list of inlines), target

attr
:   attributes ([Attr])

caption
:   text used to describe the image ([list] of [inlines])

src
:   path to the image file (string)

title
:   brief image description

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `Image` (string)

### LineBreak
Hard line break

tag, t
:   the literal `LineBreak` (string)

### Link
Hyperlink: alt text (list of inlines), target

attr
:   attributes ([Attr])

content
:   text for this link ([list] of [inlines])

target
:   the link target (string)

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `Link` (string)

### Math
TeX math (literal)

mathype
:   specifier determining whether the math content should be
    shown inline (`InlineMath`) or on a separate line
    (`DisplayMath`) (string)

text
:   math content (string)

tag, t
:   the literal `Math` (string)

### Note
Footnote or endnote

content
:   ([list] of [blocks])

tag, t
:   the literal `Note` (string)

### Quoted
Quoted text

quotetype
:   type of quotes to be used; one of `SingleQuote` or
    `DoubleQuote` (string)

content
:   quoted text ([list] of [inlines])

tag, t
:   the literal `Quoted` (string)

### RawInline
Raw inline

format
:   the format of the content (string)

text
:   raw content (string)

tag, t
:   the literal `RawInline` (string)

### SmallCaps
Small caps text

content
:   ([list] of [inlines])

tag, t
:   the literal `SmallCaps` (string)

### SoftBreak
Soft line break

tag, t
:   the literal `SoftBreak` (string)

### Space
Inter-word space

tag, t
:   the literal `Space` (string)

### Span
Generic inline container with attributes

attr
:   attributes ([Attr])

content
:   wrapped content ([list] of [inlines])

identifier
:   alias for `attr.identifier` (string)

classes
:   alias for `attr.classes` ([list] of strings)

attributes
:   alias for `attr.attributes` ([attributes])

tag, t
:   the literal `Span` (string)

### Str
Text

text
:   content (string)

tag, t
:   the literal `Str` (string)

### Strikeout
Strikeout text

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Strikeout` (string)

### Strong
Strongly emphasized text

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Strong` (string)

### Subscript
Subscripted text

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Subscript` (string)

### Superscript
Superscripted text

content
:   inline content ([list] of [inlines])

tag, t
:   the literal `Superscript` (string)

## Attributes

List of key/value pairs. Values can be accessed by using keys as
indices to the list table.

## Citation

Single citation entry

id
:   citation identifier, e.g., a bibtex key (string)

mode
:   citation mode, one of `AuthorInText`, `SuppressAuthor`, or
    `NormalCitation` (string)

prefix
:   citation prefix ([list] of [inlines])

suffix
:   citation suffix ([list] of [inlines])

note_num
:   note number (integer)

hash
:   hash (integer)

## ListAttributes
List attributes

start
:   number of the first list item (integer)

style
:   style used for list numbers; possible values are `DefaultStyle`,
    `Example`, `Decimal`, `LowerRoman`, `UpperRoman`,
    `LowerAlpha`, and `UpperAlpha` (string)

delimiter
:   delimiter of list numbers; one of `DeaultDelim`, `Period`,
    `OneParen`, and `TwoParens` (string)

## Hierarchical Element {#Element}

Hierarchical elements can be either *Sec* (sections) or *Blk*
(blocks). *Blk* elements are treated like [block]s.

### Sec

Section elements used to provide hierarchical information on
document contents.

**Objects of this type are read-only.***

level
:   header level (integer)

numbering
:   section numbering ([list] of integers)

attr
:   header attributes ([attributes])

label
:   header content ([list] of [inlines])

contents
:   list of contents in this section ([list] of hierarchical elements)

t, tag
:   constant `Sec` (string)


[block]: #block
[blocks]: #block
[list]: #list
[meta value]: #metavalue
[inline]: #inline
[inlines]: #inline
[Attr]: #attr
[attributes]: #attributes
[citaions]: #citation
