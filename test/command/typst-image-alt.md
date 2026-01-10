```
% pandoc -t typst
![Alt text from inlines](image.png)
^D
#box(image("image.png", alt: "Alt text from inlines"))
```

```
% pandoc -t typst
![](image.png){alt="Explicit alt attribute"}
^D
#box(image("image.png", alt: "Explicit alt attribute"))
```

```
% pandoc -t typst
![Inlines ignored](image.png){alt="Explicit wins"}
^D
#box(image("image.png", alt: "Explicit wins"))
```

```
% pandoc -t typst
![](image.png)
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
