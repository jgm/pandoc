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
