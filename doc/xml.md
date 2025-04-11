---
title: XML
author: massifrg@gmail.com
---

# Pandoc XML format

This document describes Pandoc's `xml` format, a 1:1 equivalent
of the `native` and `json` formats.

Here's the xml version of the beginning of this document,
to give you a glimpse of the format:

```xml
<?xml version='1.0' ?>
<Pandoc api-version="1,23,1">
<meta>
  <entry text="author">
    <MetaInlines>massifrg@gmail.com</MetaInlines>
  </entry>
  <entry text="title">
    <MetaInlines>XML</MetaInlines>
  </entry>
</meta>
<blocks>
  <Header id="pandoc-xml-format" level="1">Pandoc XML format</Header>
  <Para>This document describes Pandoc’s <Code>xml</Code> format, a 1:1 equivalent<SoftBreak />of the <Code>native</Code> and <Code>json</Code> formats.</Para>
  ...
</blocks>
</Pandoc>
```

## The tags

If you know [Pandoc types](https://hackage.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html), the XML conversion is fairly straightforward.

These are the main rules:

- every `Block` and `Inline` becomes an element with the same name and the same capitalization:
  a `Para` Block becomes a `<Para>` element, an `Emph` Inline becomes an `<Emph>` element,
  and so on;

- the root element is `<Pandoc>` and it has a `api-version` attribute, whose value
  is a string of comma-separated integer numbers; it matches the `pandoc-api-version`
  field of the `json` format;

- the root `<Pandoc>` element has only two children: `<meta>` and `<blocks>`
  (lowercase, as in `json` format);

- blocks and inlines with an `Attr` are HTM-like, and they have:

    - the `id` attribute for the identifier
    
    - the `class` attribute, a string of space-separated classes

    - the other attributes of `Attr`, without any prefix (so no `data-` prefix, instead of HTML)

- attributes are in lower (kebab) case:

    - `level` in Header

    - `start`, `number-style`, `number-delim` in OrderedList

    - `format` in `RawBlock` and RawInline

    - `quote-type` in Quoted

    - `math-type` in Math

    - `title` and `src` in Image (about `src`, see below)

    - `title` and `href` in Link (about `href`, see below)

    - `alignment` and `col-width` in ColSpec (about `col-width` values, see below)

    - `alignment`, `rowspan` and `colspan` in Cell

    - `row-head-columns` in TableBody

    - `id`, `mode`, `note-num` and `hash` for Citation (about Cite elements, see below)

The classes of items with an `Attr` are put in a `class` attribute,
so that you can style the XML with CSS.

## Added tags

Some other elements have been introduced to better structure the resulting XML.

Since they are not Pandoc Blocks or Inlines, or they have no constructor or type
in Pandoc's haskell code, they are kept lowercased.

### BulletList and OrderedList items

Items of those lists are embedded in `<item>` elements.

### DefinitionList items

Definition lists have `<item>` elements.

Each `<item>` term has only one `<term>` child element,
and one or more `<def>` children elements.

### Figure and Table captions

Figures and tables have a `<Caption>` child element,
which in turn may optionally have a `<ShortCaption>` child element.

### Tables

A `<Table>` element has:

- a `<Caption>` child element;

- a `<colspecs>` child element, whose children are empty
  `<ColSpec alignment="..." col-width="..." />` elements;

- a `<TableHead>` child element;

- one or more `<TableBody>` children elements, that in turn
  have two children: `<header>` and `<body>`, whose children
  are `<Row>` elements;

- a `<TableFoot>` child element.

This specification is debatable; I have these doubts:

- is it necessary to enclose the `<ColSpec>` elements in a `<colspecs>` element?

- to discriminate between header and data cells in table bodies,
  there are the `row-head-columns` attribute, and the `<header>` and `<body>` children 
  of the `<TableBody>` element, but there's only one type of cell:
  every cell is a `<Cell>` element

- the specs are a tradeoff between consistency with pandoc types and CSS compatibility;
  this way bodies' header rows are easily stylable with CSS, while header columns are not

### Metadata and MetaMap entries

Metadata entries are meta values (`MetaBool`, `MetaString`, `MetaInlines`, `MetaBlocks`,
`MetaList` and `MetaMap` elements) inside `<entry>` elements.

### Cite elements