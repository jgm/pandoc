```
% pandoc -f man -t html
.SH HEADING
.TS
tab(;);
l l.
a;b
c;d
^D
<h1 id="heading">HEADING</h1>
<table>
<tbody>
<tr>
<td style="text-align: left;">a</td>
<td style="text-align: left;">b</td>
</tr>
<tr>
<td style="text-align: left;">c</td>
<td style="text-align: left;">d</td>
</tr>
</tbody>
</table>
```

```
% pandoc -f man -t html
.TS
l.
^D
<p>TABLE</p>
```
