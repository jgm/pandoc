```
% pandoc -f native -t docx -o - | pandoc -f docx -t native
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
[Para [Str "hi"]
,Para [Str "lo"]]
```

```
% pandoc -f native -t docx+empty_paragraphs -o - | pandoc -f docx -t native
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
[Para [Str "hi"]
,Para [Str "lo"]]
```

```
% pandoc -f native -t docx -o - | pandoc -f docx+empty_paragraphs -t native
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
[Para [Str "hi"]
,Para [Str "lo"]]
```

```
% pandoc -f native -t docx+empty_paragraphs -o - | pandoc -f docx+empty_paragraphs -t native
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
[Para [Str "hi"]
,Para []
,Para []
,Para [Str "lo"]]
```

```
% pandoc -f native -t html5
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
<p>hi</p>
<p>lo</p>
```

```
% pandoc -f native -t html5+empty_paragraphs
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
<p>hi</p>
<p></p>
<p></p>
<p>lo</p>
```

```
% pandoc -f html+empty_paragraphs -t native
<p>hi</p>
<p></p>
<p></p>
<p>lo</p>
^D
[Para [Str "hi"]
,Para []
,Para []
,Para [Str "lo"]]
```

```
% pandoc -f html -t native
<p>hi</p>
<p></p>
<p></p>
<p>lo</p>
^D
[Para [Str "hi"]
,Para [Str "lo"]]
```

```
% pandoc -f native -t opendocument+empty_paragraphs
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
<text:p text:style-name="Text_20_body">hi</text:p>
<text:p text:style-name="Text_20_body"></text:p>
<text:p text:style-name="Text_20_body"></text:p>
<text:p text:style-name="Text_20_body">lo</text:p>
```

```
% pandoc -f native -t opendocument
[Para [Str "hi"], Para [], Para [], Para [Str "lo"]]
^D
<text:p text:style-name="Text_20_body">hi</text:p>
<text:p text:style-name="Text_20_body">lo</text:p>
```
