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
