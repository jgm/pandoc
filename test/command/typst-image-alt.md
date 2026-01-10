Typst writer: image alt text support

```
% pandoc -f markdown -t typst
![A cat sleeping on a couch](cat.png)
^D
#figure(image("cat.png", alt: "A cat sleeping on a couch"),
  caption: [
    A cat sleeping on a couch
  ]
)

```

Inline image with alt text:

```
% pandoc -f markdown -t typst
Here is an icon ![small logo](icon.png) in the text.
^D
Here is an icon #box(image("icon.png", alt: "small logo")) in the text.

```

Image with explicit alt attribute (different from caption):

```
% pandoc -f markdown -t typst
![Figure caption](diagram.png){alt="Detailed description for accessibility"}
^D
#figure(image("diagram.png", alt: "Detailed description for accessibility"),
  caption: [
    Figure caption
  ]
)

```

Image with no alt text (should omit alt parameter):

```
% pandoc -f markdown -t typst
![](empty.png)
^D
#box(image("empty.png"))

```

Inline image with dimensions should preserve alt:

```
% pandoc -f markdown -t typst
Here is ![small icon](icon.png){width=20px height=20px} inline.
^D
Here is
#box(image("icon.png", height: 0.20833in, width: 0.20833in, alt: "small icon"))
inline.

```

Alt text with special characters (quotes and backslashes):

```
% pandoc -f markdown -t typst
![Caption](test.png){alt="A \"quoted\" phrase and C:\\path\\file"}
^D
#figure(image("test.png", alt: "A \"quoted\" phrase and C:\\path\\file"),
  caption: [
    Caption
  ]
)

```

Data URI image with alt text:

```
% pandoc -f html -t typst
<img src="data:image/png;base64,iVBORw0KGgo=" alt="A small red dot">
^D
#box(image.decode("<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><image xlink:href=\"data:image/png;base64,iVBORw0KGgo=\" /></svg>", alt: "A small red dot"))

```
