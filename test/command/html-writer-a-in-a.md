a is not allowed inside a in HTML, so we remove links in link text:
```
% pandoc -f native -t html
[ Link ("",[],[]) [Link ("",[],[]) [Str "Lo"] ("url2","") ] ("url","") ]
^D
<a href="url"><span>Lo</span></a>
```
