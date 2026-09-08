Identifiers are preserved on tables without a figure wrapper.

```
% pandoc -f html -t typst
<table id="table-id" class="typst:no-figure">
  <tr><td>A</td><td>B</td></tr>
</table>
^D
#table(
  columns: 2,
  align: (auto,auto,),
  [A], [B],
)
<table-id>
```

An explicit Typst label takes precedence over the identifier.

```
% pandoc -f html -t typst
<table id="table-id" class="typst:no-figure" typst-label="table-label">
  <tr><td>A</td><td>B</td></tr>
</table>
^D
#table(
  columns: 2,
  align: (auto,auto,),
  [A], [B],
)
<table-label>
```

Labels follow scoped text properties.

```
% pandoc -f html -t typst
<table class="typst:no-figure" typst-label="table label"
       typst:text:size="3em">
  <tr><td>A</td><td>B</td></tr>
</table>
<p>Paragraph after.</p>
^D
#{set text(size: 3em);table(
  columns: 2,
  align: (auto,auto,),
  [A], [B],
)}
#label("table label")
Paragraph after.
```
