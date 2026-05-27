Like other block-level constructs, grid tables may be indented by up to
three spaces and are still recognized as tables.

```
% pandoc -f markdown -t html
   +---+---+
   | a | b |
   +===+===+
   | 1 | 2 |
   +---+---+
^D
<table style="width:11%;">
<colgroup>
<col style="width: 5%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr>
<th>a</th>
<th>b</th>
</tr>
</thead>
<tbody>
<tr>
<td>1</td>
<td>2</td>
</tr>
</tbody>
</table>
```

A headerless indented grid table is recognized too.

```
% pandoc -f markdown -t html
  +------+------+
  | foo  | bar  |
  +------+------+
^D
<table style="width:19%;">
<colgroup>
<col style="width: 9%" />
<col style="width: 9%" />
</colgroup>
<tbody>
<tr>
<td>foo</td>
<td>bar</td>
</tr>
</tbody>
</table>
```

A grid table indented four spaces is a code block, not a table.

```
% pandoc -f markdown -t html
    +---+---+
    | a | b |
    +---+---+
^D
<pre><code>+---+---+
| a | b |
+---+---+</code></pre>
```
