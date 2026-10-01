A note nested inside another note must not reuse the outer note's
`text:id`:

```
% pandoc -f markdown -t opendocument --wrap=none --template command/opendocument-body.opendocument
Text^[outer^[inner]]^[second]
^D
<text:p text:style-name="Text_20_body">Text<text:note text:id="ftn0" text:note-class="footnote"><text:note-citation>1</text:note-citation><text:note-body><text:p text:style-name="Footnote">outer<text:note text:id="ftn1" text:note-class="footnote"><text:note-citation>2</text:note-citation><text:note-body><text:p text:style-name="Footnote">inner</text:p></text:note-body></text:note></text:p></text:note-body></text:note><text:note text:id="ftn2" text:note-class="footnote"><text:note-citation>3</text:note-citation><text:note-body><text:p text:style-name="Footnote">second</text:p></text:note-body></text:note></text:p>
```
