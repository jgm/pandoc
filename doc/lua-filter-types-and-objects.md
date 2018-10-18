% Lua filter types and objects
% Albert Krewinkel
% Oct 6, 2018

## Pandoc

Pandoc document

`blocks`
:   document content ([list] of [Block]s)

`meta`
:   document meta information ([Meta] object)


## Meta

Meta information on a document; string-indexed collection of
[MetaValue]s.

## MetaValue

Document meta information items.

### MetaBlocks

A list of blocks usable as meta value ([list] of [Block]s)

Fields:

`tag`, `t`
:   the literal `MetaBlocks` (string)

### MetaBool

Plain Lua boolean value (boolean)

### MetaInlines

List of inlines used in metadata ([list] of [Inline]s)

Fields:

`tag`, `t`
:   the literal `MetaInlines` (string)

### MetaList

A list of other [MetaValue]s. ([list])

Fields:

`tag`, `t`
:   the literal `MetaList` (string)

### MetaMap

A string-indexed map of meta-values. (table)

Fields:

`tag`, `t`
:   the literal `MetaMap` (string)

*Note*: The fields will be shadowed if the map contains a field
with the same name as those listed.

### MetaString

Plain Lua string value (string)


## Block

### BlockQuote

A block quote element

content:
:   block content ([list] of [Block]s)

`tag`, `t`
:   the literal `BlockQuote` (string)

### BulletList

A bullet list

`content`
:   list of items ([list] of [Block]s)

`tag`, `t`
:   the literal `BulletList` (string)

### CodeBlock

Block of code.

`text`
:   code string (string)

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `CodeBlock` (string)

### DefinitionList

Definition list, containing terms and their explanation.

`content`
:   list of items

`tag`, `t`
:   the literal `DefinitionList` (string)

### Div

Generic block container with attributes

`content`
:   block content ([list] of [Block]s)

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Div` (string)

### Header

Creates a header element.

`level`
:   header level (integer)

`content`
:   inline content ([list] of [Inline]s)

`attr`
:   element attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Header` (string)


### HorizontalRule

A horizontal rule.

`tag`, `t`
:   the literal `HorizontalRule` (string)

### LineBlock

A line block, i.e. a list of lines, each separated from the next
by a newline.

`content`
:   inline content

`tag`, `t`
:   the literal `LineBlock` (string)

### Null

A null element; this element never produces any output in the
target format.

`tag`, `t`
:   the literal `Null` (string)

### OrderedList

An ordered list.

Parameters:

`items`
:   list items ([list] of [Block]s)

`listAttributes`
:   list parameters ([ListAttributes])

`start`
:   alias for `listAttributes.start` (integer)

`style`
:   alias for `listAttributes.style` (string)

`delimiter`
:   alias for `listAttributes.delimiter` (string)

`tag`, `t`
:   the literal `OrderedList` (string)

### Para

A paragraph

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Para` (string)

### Plain

Plain text, not a paragraph

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Plain` (string)

### RawBlock

Raw content of a specified format.

`format`
:   format of content (string)

`text`
:   raw content (string)

`tag`, `t`
:   the literal `RawBlock` (string)

### Table

A table.

`caption`
:   table caption ([list] of [Inline]s)

`aligns`
:   column alignments ([list] of [Alignment]s)

`widths`
:   column widths (number)

`headers`
:   header row ([list] of [table cells])

`rows`
:   table rows ([list] of [list]s of [table cells])

`tag`, `t`
:   the literal `Table` (string)

A [table cell]{#table-cell} is a list of blocks.

*[Alignment]{#Alignment}* is a string value indicating the
horizontal alignment of a table column. `AlignLeft`,
`AlignRight`, and `AlignCenter` leads cell content tob be
left-aligned, right-aligned, and centered, respectively. The
default alignment is `AlignDefault` (often equivalent to
centered).

[Alignment]: #Alignment
[table cells]: #table-cell

## Inline

### Cite
Citation

`content`
:   ([list] of [Inline]s)

`citations`
:   citation entries ([list] of [citations])

`tag`, `t`
:   the literal `Cite` (string)

### Code
Inline code

`text`
:   code string (string)

`attr`
:   attributes ([Attr])

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Code` (string)

### Emph
Emphasized text

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Emph` (string)

### Image
Image:  alt text (list of inlines), target

`attr`
:   attributes ([Attr])

`caption`
:   text used to describe the image ([list] of [Inline]s)

`src`
:   path to the image file (string)

`title`
:   brief image description

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Image` (string)

### LineBreak
Hard line break

`tag`, `t`
:   the literal `LineBreak` (string)

### Link
Hyperlink: alt text (list of inlines), target

`attr`
:   attributes ([Attr])

`content`
:   text for this link ([list] of [Inline]s)

`target`
:   the link target (string)

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Link` (string)

### Math
TeX math (literal)

`mathype`
:   specifier determining whether the math content should be
    shown inline (`InlineMath`) or on a separate line
    (`DisplayMath`) (string)

`text`
:   math content (string)

`tag`, `t`
:   the literal `Math` (string)

### Note
Footnote or endnote

`content`
:   ([list] of [Block]s)

`tag`, `t`
:   the literal `Note` (string)

### Quoted
Quoted text

`quotetype`
:   type of quotes to be used; one of `SingleQuote` or
    `DoubleQuote` (string)

`content`
:   quoted text ([list] of [Inline]s)

`tag`, `t`
:   the literal `Quoted` (string)

### RawInline
Raw inline

`format`
:   the format of the content (string)

`text`
:   raw content (string)

`tag`, `t`
:   the literal `RawInline` (string)

### SmallCaps
Small caps text

`content`
:   ([list] of [Inline]s)

`tag`, `t`
:   the literal `SmallCaps` (string)

### SoftBreak
Soft line break

`tag`, `t`
:   the literal `SoftBreak` (string)

### Space
Inter-word space

`tag`, `t`
:   the literal `Space` (string)

### Span
Generic inline container with attributes

`attr`
:   attributes ([Attr])

`content`
:   wrapped content ([list] of [Inline]s)

`identifier`
:   alias for `attr.identifier` (string)

`classes`
:   alias for `attr.classes` ([list] of strings)

`attributes`
:   alias for `attr.attributes` ([Attributes])

`tag`, `t`
:   the literal `Span` (string)

### Str
Text

`text`
:   content (string)

`tag`, `t`
:   the literal `Str` (string)

### Strikeout
Strikeout text

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Strikeout` (string)

### Strong
Strongly emphasized text

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Strong` (string)

### Subscript
Subscripted text

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Subscript` (string)

### Superscript
Superscripted text

`content`
:   inline content ([list] of [Inline]s)

`tag`, `t`
:   the literal `Superscript` (string)

## Element components

### Attr

A set of element attributes

`identifier`
:   element identifier (string)

`classes`
:   element classes ([list] of strings)

`attributes`
:   collection of key/value pairs ([Attributes])

### Attributes

List of key/value pairs. Values can be accessed by using keys as
indices to the list table.

### Citation

Single citation entry

`id`
:   citation identifier, e.g., a bibtex key (string)

`mode`
:   citation mode, one of `AuthorInText`, `SuppressAuthor`, or
    `NormalCitation` (string)

`prefix`
:   citation prefix ([list] of [Inline]s)

`suffix`
:   citation suffix ([list] of [Inline]s)

`note_num`
:   note number (integer)

`hash`
:   hash (integer)

### ListAttributes
List attributes

`start`
:   number of the first list item (integer)

`style`
:   style used for list numbers; possible values are `DefaultStyle`,
    `Example`, `Decimal`, `LowerRoman`, `UpperRoman`,
    `LowerAlpha`, and `UpperAlpha` (string)

`delimiter`
:   delimiter of list numbers; one of `DefaultDelim`, `Period`,
    `OneParen`, and `TwoParens` (string)

## Hierarchical Element {#Element}

Hierarchical elements can be either *Sec* (sections) or *Blk*
(blocks). *Blk* elements are treated like [Block]s.

### Sec

Section elements used to provide hierarchical information on
document contents.

**Objects of this type are read-only.**

`level`
:   header level (integer)

`numbering`
:   section numbering ([list] of integers)

`attr`
:   header attributes ([Attr])

`label`
:   header content ([list] of [Inline]s)

`contents`
:   list of contents in this section ([list] of [hierarchical element]s)

`tag`, `t`
:   constant `Sec` (string)

[hierarchical element]: #Element

## ReaderOptions

Pandoc reader options

`abbreviations`
:   set of known abbreviations (set of strings)

`columns`
:   number of columns in terminal (integer)

`default_image_extension`
:   default extension for images (string)

`extensions`
:   string representation of the syntax extensions bit field
    (string)

`indented_code_classes`
:   default classes for indented code blocks (list of strings)

`standalone`
:   whether the input was a standalone document with header
    (boolean)

`strip_comments`
:   HTML comments are stripped instead of parsed as raw HTML
    (boolean)

`tab_stop`
:   width (i.e. equivalent number of spaces) of tab stops
    (integer)

`track_changes`
:   track changes setting for docx; one of `AcceptChanges`,
    `RejectChanges`, and `AllChanges` (string)

[Block]: #Block
[list]: #List
[MetaValue]: #MetaValue
[Inline]: #Inline
[Attr]: #attr
[Attributes]: #attributes
[citations]: #citation
