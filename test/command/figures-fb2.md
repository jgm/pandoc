```
% pandoc -f native -t fb2
[Figure ("fig-id",[],[]) (Caption Nothing []) [Para [Str "content"]]]

^D
<?xml version="1.0" encoding="UTF-8"?>
<FictionBook xmlns="http://www.gribuser.ru/xml/fictionbook/2.0" xmlns:l="http://www.w3.org/1999/xlink"><description><title-info><genre>unrecognised</genre></title-info><document-info><program-used>pandoc</program-used></document-info></description><body><title><p /></title><section><p>content</p></section></body></FictionBook>
```
