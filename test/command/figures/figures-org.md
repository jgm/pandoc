```
% pandoc -f native -t org
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Str "content"]]]

^D
<<fig-id>>
content
```

```
% pandoc -f native -t org
[Figure ("",[],[]) (Caption Nothing []) [Para [Str "content"]]]

^D
content
```
