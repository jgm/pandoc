The `abstract` metadata field holds blocks, so it must be rendered
with the block writer: previously it was flattened to inlines, which
produced bare character data (not valid as a child of `office:text`)
and dropped the `Abstract` style.

```
% pandoc -f markdown -t opendocument --template command/opendocument-abstract.opendocument
---
abstract: A one-line abstract.
---

Body.
^D
<text:p text:style-name="Abstract">A one-line abstract.</text:p>
```

A multi-paragraph abstract stays multi-paragraph:

```
% pandoc -f markdown -t opendocument --wrap=none --template command/opendocument-abstract.opendocument
---
abstract: |
  First para.

  Second para.
---

Body.
^D
<text:p text:style-name="Abstract">First para.</text:p><text:p text:style-name="Abstract">Second para.</text:p>
```

A requested paragraph style must also be applied to `Plain` blocks, not
just to `Para`. The body of a caption-less figure is a `Plain`, so it
should still get the `Figure` style:

```
% pandoc -f native -t opendocument --template command/opendocument-body.opendocument
[Figure ("fig1",[],[]) (Caption Nothing []) [Plain [Str "placeholder"]]]
^D
<text:p text:style-name="Figure">placeholder</text:p>
```

Column style names must remain valid NCNames past the 26th column;
incrementing a character code would run into `[`, `\`, etc.

```
% pandoc -f native -t opendocument --template command/opendocument-body.opendocument
[Table ("",[],[]) (Caption Nothing [])
 [(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)
 ,(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault),(AlignDefault,ColWidthDefault)]
 (TableHead ("",[],[]) [])
 [(TableBody ("",[],[]) (RowHeadColumns 0) [] [])]
 (TableFoot ("",[],[]) [])]
^D
<table:table table:name="Table1" table:style-name="Table1">
  <table:table-column table:style-name="Table1.A" />
  <table:table-column table:style-name="Table1.B" />
  <table:table-column table:style-name="Table1.C" />
  <table:table-column table:style-name="Table1.D" />
  <table:table-column table:style-name="Table1.E" />
  <table:table-column table:style-name="Table1.F" />
  <table:table-column table:style-name="Table1.G" />
  <table:table-column table:style-name="Table1.H" />
  <table:table-column table:style-name="Table1.I" />
  <table:table-column table:style-name="Table1.J" />
  <table:table-column table:style-name="Table1.K" />
  <table:table-column table:style-name="Table1.L" />
  <table:table-column table:style-name="Table1.M" />
  <table:table-column table:style-name="Table1.N" />
  <table:table-column table:style-name="Table1.O" />
  <table:table-column table:style-name="Table1.P" />
  <table:table-column table:style-name="Table1.Q" />
  <table:table-column table:style-name="Table1.R" />
  <table:table-column table:style-name="Table1.S" />
  <table:table-column table:style-name="Table1.T" />
  <table:table-column table:style-name="Table1.U" />
  <table:table-column table:style-name="Table1.V" />
  <table:table-column table:style-name="Table1.W" />
  <table:table-column table:style-name="Table1.X" />
  <table:table-column table:style-name="Table1.Y" />
  <table:table-column table:style-name="Table1.Z" />
  <table:table-column table:style-name="Table1.AA" />
</table:table>
```

A note nested inside another note must not reuse the outer note's
`text:id`:

```
% pandoc -f markdown -t opendocument --wrap=none --template command/opendocument-body.opendocument
Text^[outer^[inner]]^[second]
^D
<text:p text:style-name="Text_20_body">Text<text:note text:id="ftn0" text:note-class="footnote"><text:note-citation>1</text:note-citation><text:note-body><text:p text:style-name="Footnote">outer<text:note text:id="ftn1" text:note-class="footnote"><text:note-citation>2</text:note-citation><text:note-body><text:p text:style-name="Footnote">inner</text:p></text:note-body></text:note></text:p></text:note-body></text:note><text:note text:id="ftn2" text:note-class="footnote"><text:note-citation>3</text:note-citation><text:note-body><text:p text:style-name="Footnote">second</text:p></text:note-body></text:note></text:p>
```

Automatic text styles are declared in numerical order: sorting them by
name would put `T10` between `T1` and `T2`.

```
% pandoc -f native -t opendocument --wrap=none --template command/11301-styles.opendocument
[Para [SmallCaps [Str "a"]
      ,SmallCaps [Strikeout [Str "b"]]
      ,SmallCaps [Superscript [Str "c"]]
      ,SmallCaps [Subscript [Str "d"]]
      ,SmallCaps [Underline [Str "e"]]
      ,Strikeout [Superscript [Str "f"]]
      ,Strikeout [Subscript [Str "g"]]
      ,Strikeout [Underline [Str "h"]]
      ,Underline [Superscript [Str "i"]]
      ,Underline [Subscript [Str "j"]]]]
^D
<style:style style:name="T1" style:family="text"><style:text-properties fo:font-variant="small-caps" /></style:style>
<style:style style:name="T2" style:family="text"><style:text-properties fo:font-variant="small-caps" style:text-line-through-style="solid" /></style:style>
<style:style style:name="T3" style:family="text"><style:text-properties fo:font-variant="small-caps" style:text-position="super 58%" /></style:style>
<style:style style:name="T4" style:family="text"><style:text-properties fo:font-variant="small-caps" style:text-position="sub 58%" /></style:style>
<style:style style:name="T5" style:family="text"><style:text-properties fo:font-variant="small-caps" style:text-underline-color="font-color" style:text-underline-style="solid" style:text-underline-width="auto" /></style:style>
<style:style style:name="T6" style:family="text"><style:text-properties style:text-line-through-style="solid" style:text-position="super 58%" /></style:style>
<style:style style:name="T7" style:family="text"><style:text-properties style:text-line-through-style="solid" style:text-position="sub 58%" /></style:style>
<style:style style:name="T8" style:family="text"><style:text-properties style:text-line-through-style="solid" style:text-underline-color="font-color" style:text-underline-style="solid" style:text-underline-width="auto" /></style:style>
<style:style style:name="T9" style:family="text"><style:text-properties style:text-position="super 58%" style:text-underline-color="font-color" style:text-underline-style="solid" style:text-underline-width="auto" /></style:style>
<style:style style:name="T10" style:family="text"><style:text-properties style:text-position="sub 58%" style:text-underline-color="font-color" style:text-underline-style="solid" style:text-underline-width="auto" /></style:style>
<style:style style:name="fr2" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" style:horizontal-pos="center" style:horizontal-rel="paragraph-content" style:wrap="none" /></style:style>
<style:style style:name="fr1" style:family="graphic" style:parent-style-name="Formula"><style:graphic-properties style:vertical-pos="middle" style:vertical-rel="text" /></style:style>
<text:p text:style-name="Text_20_body"><text:span text:style-name="T1">a</text:span><text:span text:style-name="T2">b</text:span><text:span text:style-name="T3">c</text:span><text:span text:style-name="T4">d</text:span><text:span text:style-name="T5">e</text:span><text:span text:style-name="T6">f</text:span><text:span text:style-name="T7">g</text:span><text:span text:style-name="T8">h</text:span><text:span text:style-name="T9">i</text:span><text:span text:style-name="T10">j</text:span></text:p>
```
