```
% pandoc -f native -t textile
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "")]]]

^D
<figure id="fig-id">

<figcaption>

Caption

</figcaption>

!foo.png!


</figure>
```

```
% pandoc -f native -t textile
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Image ("",[],[]) [] ("foo.png", "")]]]

^D
<figure id="fig-id">

!foo.png!


</figure>
```
