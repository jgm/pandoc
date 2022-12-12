Figure float with caption at the figure level.

```
% pandoc -f native -t markdown
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
<figure id="fig-id">
<p><img src="foo.png" title="fig:" /></p>
<figcaption><p>Caption</p></figcaption>
</figure>
```
