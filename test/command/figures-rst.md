Figure float with caption at the figure level.

```
% pandoc -f native -t rst
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
.. figure:: foo.png
   :alt: fig:
```
