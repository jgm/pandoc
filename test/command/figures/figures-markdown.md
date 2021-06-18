Figure float with caption at the figure level.

```
% pandoc -f native -t markdown
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
::: {#fig-id .figure}
![](foo.png)
:::
```
