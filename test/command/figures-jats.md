Figure float with caption at the figure level.

```
% pandoc -f native -t jats
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Str "Text"],
Para [Image ("fig-id-2",[],[]) [] ("foo.png", "fig:")]]]

^D
<fig id="fig-id">
  <caption><p>Caption</p></caption>
  <p>Text</p>
  <graphic id="fig-id-2" mimetype="image" mime-subtype="png" xlink:href="foo.png" xlink:title="fig:" />
</fig>
```
