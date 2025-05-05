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
  <entry key="author">
    <MetaInlines>massifrg@gmail.com</MetaInlines>
  </entry>
  <entry key="title">
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

- `Str` inlines are usually converted to plain, UTF-8 text (see below for exceptions)

- `Space` inlines are usually converted to " " chars (see below for exceptions)

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

  - `start`, `number-style`, `number-delim` in OrderedList;
    style and delimiter values are capitalized exactly as in `Text.Pandoc.Definition`;

  - `format` in `RawBlock` and RawInline

  - `quote-type` in Quoted (values are `SingleQuote` and `DoubleQuote`)

  - `math-type` in Math (values are `InlineMath` and `DisplayMath`)

  - `title` and `src` in Image target

  - `title` and `href` in Link target

  - `alignment` and `col-width` in ColSpec (about `col-width` values, see below);
    (alignment values are capitalized as in `Text.Pandoc.Definition`)

  - `alignment`, `row-span` and `col-span` in Cell

  - `row-head-columns` in TableBody

  - `id`, `mode`, `note-num` and `hash` for Citation (about Cite elements, see below);
    (`mode` values are capitalized as in `Text.Pandoc.Definition`)

The classes of items with an `Attr` are put in a `class` attribute,
so that you can style the XML with CSS.

## Str and Space elements

`Str` and `Space` usually result in text and normal " " spaces, but there are exceptions:

- `Str ""`, an empty string, is not suppressed; instead it is converted into a `<Str />` element;

- `Str "foo bar"`, a string containing a space, is converted as `<Str content="foo bar" />`;

- consecutive `Str` inlines, as in `[ ..., Str "foo", Str "bar", ... ]`,
  are encoded as `foo<Str content="bar" />` to keep their individuality;

- consecutive `Space` inlines, as in `[ ..., Space, Space, ... ]`,
  are encoded as `<Space count="2" />`

- `Space` inlines at the start or at the end of their container element
  are always encoded with a `<Space />` element, instead of just a " "

These encodings are necessary to ensure 1:1 equivalence of the `xml` format with the AST,
or the `native` and `json` formats.

Since the ones above are corner cases, usually you should not see those `<Str />` and `<Space />`
elements in your documents.

## Added tags

Some other elements have been introduced to better structure the resulting XML.

Since they are not Pandoc Blocks or Inlines, or they have no constructor or type
in Pandoc's haskell code, they are kept lowercased.

### BulletList and OrderedList items

Items of those lists are embedded in `<item>` elements.

These snippets are from the `xml` version of `test/testsuite.native`:

```xml
<BulletList>
  <item>
    <Plain>asterisk 1</Plain>
  </item>
  <item>
    <Plain>asterisk 2</Plain>
  </item>
  <item>
    <Plain>asterisk 3</Plain>
  </item>
</BulletList>
...
<OrderedList start="1" number-style="Decimal" number-delim="Period">
  <item>
    <Plain>First</Plain>
  </item>
  <item>
    <Plain>Second</Plain>
  </item>
  <item>
    <Plain>Third</Plain>
  </item>
</OrderedList>
```

### DefinitionList items

Definition lists have `<item>` elements.

Each `<item>` term has only one `<term>` child element,
and one or more `<def>` children elements.

This snippet is from the `xml` version of `test/testsuite.native`:

```xml
<DefinitionList>
  <item>
    <term>apple</term>
    <def>
      <Plain>red fruit</Plain>
    </def>
  </item>
  <item>
    <term>orange</term>
    <def>
      <Plain>orange fruit</Plain>
    </def>
  </item>
  <item>
    <term>banana</term>
    <def>
      <Plain>yellow fruit</Plain>
    </def>
  </item>
</DefinitionList>
```

### Figure and Table captions

Figures and tables have a `<Caption>` child element,
which in turn may optionally have a `<ShortCaption>` child element.

This snippet is from the `xml` version of `test/testsuite.native`:

```xml
<Figure>
  <Caption>
    <Plain>lalune</Plain>
  </Caption>
  <Plain><Image src="lalune.jpg" title="Voyage dans la Lune">lalune</Image></Plain>
</Figure>
```

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

The `ColWidthDefault` value becomes a "0" value for the attribute `col-width`;
this way it's type-consistent with non-zero values, but I'm still doubtful whether to
leave its value as a "ColWidthDefault" string.

Here's an example from the `xml` version of `test/tables/planets.native`:

```xml
<Table>
  <Caption>
    <Para>Data about the planets of our solar system.</Para>
  </Caption>
  <colspecs>
    <ColSpec col-width="0" alignment="AlignCenter" />
    <ColSpec col-width="0" alignment="AlignCenter" />
    <ColSpec col-width="0" alignment="AlignDefault" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignRight" />
    <ColSpec col-width="0" alignment="AlignDefault" />
  </colspecs>
  <TableHead>
    <Row>
      <Cell col-span="2" row-span="1" alignment="AlignDefault" />
      <Cell col-span="1" row-span="1" alignment="AlignDefault">
        <Plain>Name</Plain>
      </Cell>
      <Cell col-span="1" row-span="1" alignment="AlignDefault">
        <Plain>Mass (10^24kg)</Plain>
      </Cell>
      ...
    </Row>
  </TableHead>
  <TableBody row-head-columns="3">
    <header />
    <body>
      <Row>
        <Cell col-span="2" row-span="4" alignment="AlignDefault">
          <Plain>Terrestrial planets</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>Mercury</Plain>
        </Cell>
          <Cell alignment="AlignDefault">
        <Plain>0.330</Plain>
          </Cell>
        <Cell alignment="AlignDefault">
          <Plain>4,879</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>5427</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>3.7</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>4222.6</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>57.9</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>167</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>0</Plain>
        </Cell>
        <Cell alignment="AlignDefault">
          <Plain>Closest to the Sun</Plain>
        </Cell>
      </Row>
      ...
    </body>
  </TableBody>
  <TableFoot />
</Table>
```

### Metadata and MetaMap entries

Metadata entries are meta values (`MetaBool`, `MetaString`, `MetaInlines`, `MetaBlocks`,
`MetaList` and `MetaMap` elements) inside `<entry>` elements.

The `<meta>` and the `<MetaMap>` elements have the same children elements (`<entry>`),
which have a `key` attribute.

`<MetaInlines>`, `<MetaBlocks>`, `<MetaList>` and `<MetaMap>` elements
all have children elements.

`<MetaString>` elements have only text.

`<MetaBool>` elements are empty, they can be either `<MetaBool value="true" />`
or `<MetaBool value="false" />`.

This snippet is from the `xml` version of `test/testsuite.native`:

```xml
<meta>
  <entry key="author">
    <MetaList>
      <MetaInlines>John MacFarlane</MetaInlines>
      <MetaInlines>Anonymous</MetaInlines>
    </MetaList>
  </entry>
  <entry key="date">
    <MetaInlines>July 17, 2006</MetaInlines>
  </entry>
  <entry key="title">
    <MetaInlines>Pandoc Test Suite</MetaInlines>
  </entry>
</meta>
```

### Cite elements

`Cite` inlines are modeled with `<Cite>` elements, whose first child
is a `<citations>` element, that have only `<Citation>` children elements.

`<Citation>` elements are empty, unless they have a prefix and/or a suffix.

Here's an example from the `xml` version of `test/markdown-citations.native`:

```xml
<Para><Cite><citations>
  <Citation note-num="3" mode="AuthorInText" id="item1" hash="0" />
</citations>@item1</Cite> says blah.</Para>
<Para><Cite><citations>
  <Citation note-num="4" mode="AuthorInText" id="item1" hash="0">
    <suffix>p. 30</suffix>
  </Citation>
</citations>@item1 [p. 30]</Cite> says blah.</Para>
<Para>A citation group <Cite><citations>
  <Citation note-num="8" mode="NormalCitation" id="item1" hash="0">
    <prefix>see</prefix>
    <suffix> chap. 3</suffix>
  </Citation>
  <Citation note-num="8" mode="NormalCitation" id="пункт3" hash="0">
    <prefix>also</prefix>
    <suffix> p. 34-35</suffix>
  </Citation>
</citations>[see @item1 chap. 3; also @пункт3 p. 34-35]</Cite>.</Para>
```
