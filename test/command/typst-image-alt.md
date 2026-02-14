```
% pandoc -f markdown-implicit_figures -t typst
![Alt text from inlines](image.png)
^D
#box(image("image.png", alt: "Alt text from inlines"))
```

```
% pandoc -f markdown-implicit_figures -t typst
![](image.png){alt="Explicit alt attribute"}
^D
#box(image("image.png", alt: "Explicit alt attribute"))
```

```
% pandoc -f markdown-implicit_figures -t typst
![Inlines ignored](image.png){alt="Explicit wins"}
^D
#box(image("image.png", alt: "Explicit wins"))
```

```
% pandoc -f markdown-implicit_figures -t typst
![](image.png)
^D
#box(image("image.png"))
```

```
% pandoc -f markdown-implicit_figures -t typst
![Inlines ignored for decorative](image.png){alt=""}
^D
#box(image("image.png"))
```

```
% pandoc -t typst
![Caption text](image.png){alt="Alt text describing the image"}

^D
#figure(image("image.png", alt: "Alt text describing the image"),
  caption: [
    Caption text
  ]
)
```

```
% pandoc -t typst
![Caption only](image.png)

^D
#figure(image("image.png", alt: "Caption only"),
  caption: [
    Caption only
  ]
)
```

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

```
% pandoc -f html -t typst
<img src="data:image/png;base64,iVBORw0KGgo=" alt="A small red dot">
^D
#box(image(bytes((137,80,78,71,13,10,26,10)), alt: "A small red dot"))

```
