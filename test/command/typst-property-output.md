```
% pandoc -f html -t typst
<span typst:text:fill="orange">foo</span>
^D
#text(fill: orange)[foo]
```

```
% pandoc -f html -t typst
<span>foo</span>
^D
foo
```

```
% pandoc -f html -t typst
<div typst:inset="10pt">foo</div>
^D
#block(inset: 10pt)[
foo
]
```

```
% pandoc -f html -t typst
<div typst:text:fill="purple">foo</div>
^D
#block[
#set text(fill: purple); foo
]
```

```
% pandoc -f html -t typst
<div>foo</div>
^D
#block[
foo
]
```

```
% pandoc -f html -t typst
<table typst:fill="blue">
  <tr><td>A</td><td>B</td></tr>
</table>
^D
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    fill: blue,
    [A], [B],
  )]
  , kind: table
  )
```

```
% pandoc -f html -t typst
<table typst:text:fill="orange">
  <tr><td>A</td><td>B</td></tr>
</table>
^D
#figure(
  align(center)[#set text(fill: orange); #table(
    columns: 2,
    align: (auto,auto,),
    [A], [B],
  )]
  , kind: table
  )
```

```
% pandoc -f html -t typst
<table>
  <tr><td>A</td><td typst:fill="green">B</td></tr>
</table>
^D
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    [A], table.cell(fill: green)[B],
  )]
  , kind: table
  )
```

```
% pandoc -f html -t typst
<table>
  <tr><td>A</td><td typst:text:fill="fuchsia">B</td></tr>
</table>
^D
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    [A], [#set text(fill: fuchsia); B],
  )]
  , kind: table
  )
```

```
% pandoc -f html -t typst
<table>
  <tr><td>A</td><td typst:text:fill="maroon" align="center">B</td></tr>
</table>
^D
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,center,),
    [A], table.cell(align: center)[#set text(fill: maroon); B],
  )]
  , kind: table
  )
```

```
% pandoc -f html -t typst
<table>
  <tr><td>A</td><td typst:align="horizon" align="center">B</td></tr>
</table>
^D
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,center,),
    [A], table.cell(align: horizon + center)[B],
  )]
  , kind: table
  )
```


```
% pandoc -f html -t typst
<table>
  <tr><td>A</td><td>B</td></tr>
</table>
^D
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    [A], [B],
  )]
  , kind: table
  )
```

```
% pandoc -f html -t typst
<p>Paragraph before.</p>

<table class="typst:no-figure" typst:text:size="3em">
  <tr>
    <td>A</td>
    <td>B</td>
    <td>C</td>
  </tr>
</table>

<p>Paragraph after.</p>
^D
Paragraph before.

#{set text(size: 3em); table(
  columns: 3,
  align: (auto,auto,auto,),
  [A], [B], [C],
)}
Paragraph after.
```


```
% pandoc -f html -t typst
<p>Paragraph before.</p>

<table typst:text:size="3em">
  <tr>
    <td>A</td>
    <td>B</td>
    <td>C</td>
  </tr>
</table>

<p>Paragraph after.</p>
^D
Paragraph before.

#figure(
  align(center)[#set text(size: 3em); #table(
    columns: 3,
    align: (auto,auto,auto,),
    [A], [B], [C],
  )]
  , kind: table
  )

Paragraph after.
```


```
% pandoc -f html -t typst
<p>Paragraph before.</p>

<table class="typst:no-figure">
  <tr>
    <td>A</td>
    <td>B</td>
    <td>C</td>
  </tr>
</table>

<p>Paragraph after.</p>
^D
Paragraph before.

#table(
  columns: 3,
  align: (auto,auto,auto,),
  [A], [B], [C],
)
Paragraph after.
```