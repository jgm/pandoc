# Writer

HTML5 figure with caption and content.

```
% pandoc -f native -t html5
[Figure ("fig-id",[],[]) (Caption Nothing [Plain [Str "caption"]]) [Para [Str "content"]]]

^D
<figure id="fig-id">
<p>content</p>
<figcaption>caption</figcaption>
</figure>
```

HTML5 figure with NO caption and content.

```
% pandoc -f native -t html5
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Str "content"]]]

^D
<figure id="fig-id">
<p>content</p>
</figure>
```

HTML4 figure with caption and content.

```
% pandoc -f native -t html4
[Figure ("fig-id",[],[]) (Caption Nothing [Plain [Str "caption"]]) [Para [Str "content"]]]

^D
<div class="float" id="fig-id">
<p>content</p>
<div class="figcaption">caption</div>
</div>
```

HTML4 figure with NO caption and content.

```
% pandoc -f native -t html4
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Str "content"]]]

^D
<div class="float" id="fig-id">
<p>content</p>
</div>
```

# Reader

Figure with caption and multiple elements.

```
% pandoc -f html -t native
<figure class="important">
  <img src="../media/rId25.jpg" />
  <ul> <li> ITEM </li> </ul>
  <figcaption> CAP2 </figcaption>
</figure>
^D
[ Figure
    ( "" , [ "important" ] , [] )
    (Caption Nothing [ Plain [ Str "CAP2" ] ])
    [ Plain
        [ Image ( "" , [] , [] ) [] ( "../media/rId25.jpg" , "" ) ]
    , BulletList [ [ Plain [ Str "ITEM" ] ] ]
    ]
]
```

Figure without caption.

```
% pandoc -f html -t native
<figure class="important">
  <img src="../media/rId25.jpg" />
  <ul> <li> ITEM </li> </ul>
</figure>
^D
[ Figure
    ( "" , [ "important" ] , [] )
    (Caption Nothing [])
    [ Plain
        [ Image ( "" , [] , [] ) [] ( "../media/rId25.jpg" , "" ) ]
    , BulletList [ [ Plain [ Str "ITEM" ] ] ]
    ]
]
```
