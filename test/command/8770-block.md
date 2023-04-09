```
% pandoc -t html5 --reference-location=block
# Section 1

hello[^1]

: Sample table.[^2]

-----------
 Fruit[^3]
-----------
 Bans[^4]
-----------

# Section 2

dolly[^5]

[^1]: doc footnote
[^2]: caption footnote
[^3]: header footnote
[^4]: table cell footnote
[^5]: doc footnote
^D
<h1 id="section-1">Section 1</h1>
<p>hello<a href="#fn1" class="footnote-ref" id="fnref1"
role="doc-noteref"><sup>1</sup></a></p>
<aside id="footnotes" class="footnotes footnotes-end-of-block"
role="doc-footnote">
<ol>
<li id="fn1"><p>doc footnote<a href="#fnref1" class="footnote-back"
role="doc-backlink">↩︎</a></p></li>
</ol>
</aside>
<table style="width:17%;">
<caption>Sample table.<a href="#fn2" class="footnote-ref" id="fnref2"
role="doc-noteref"><sup>2</sup></a></caption>
<colgroup>
<col style="width: 16%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: center;">Fruit<a href="#fn3" class="footnote-ref"
id="fnref3" role="doc-noteref"><sup>3</sup></a></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: center;">Bans<a href="#fn4" class="footnote-ref"
id="fnref4" role="doc-noteref"><sup>4</sup></a></td>
</tr>
</tbody>
</table>
<aside id="footnotes-2" class="footnotes footnotes-end-of-block"
role="doc-footnote">
<ol start="2">
<li id="fn2"><p>caption footnote<a href="#fnref2" class="footnote-back"
role="doc-backlink">↩︎</a></p></li>
<li id="fn3"><p>header footnote<a href="#fnref3" class="footnote-back"
role="doc-backlink">↩︎</a></p></li>
<li id="fn4"><p>table cell footnote<a href="#fnref4"
class="footnote-back" role="doc-backlink">↩︎</a></p></li>
</ol>
</aside>
<h1 id="section-2">Section 2</h1>
<p>dolly<a href="#fn5" class="footnote-ref" id="fnref5"
role="doc-noteref"><sup>5</sup></a></p>
<aside id="footnotes-3" class="footnotes footnotes-end-of-block"
role="doc-footnote">
<ol start="5">
<li id="fn5"><p>doc footnote<a href="#fnref5" class="footnote-back"
role="doc-backlink">↩︎</a></p></li>
</ol>
</aside>
```
