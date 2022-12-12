Figure float with caption at the figure level.

```
% pandoc -f native -t mediawiki
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
<div id="fig-id" class="figure">

[[File:foo.png|thumb|none]]


</div>
```
