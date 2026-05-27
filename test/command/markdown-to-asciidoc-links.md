converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/links.feature>

Render an implicit inline link

```
% pandoc -t asciidoc
Use [http://example.com](http://example.com) for sample links in documentation.
^D
Use http://example.com for sample links in documentation.
```


Render an inline link

```
% pandoc -t asciidoc
This is [an example](http://example.com/) inline link.
^D
This is http://example.com/[an example] inline link.
```


Render linked text with comma

```
% pandoc -t asciidoc
This is [a very, very cool](http://example.com/) inline link.
^D
This is http://example.com/[a very, very cool] inline link.
```


Render a reference style link with link definition

```
% pandoc -t asciidoc
The [syntax page][s] provides complete, detailed documentation for

[s]: /projects/markdown/syntax  "Markdown Syntax"
^D
The link:/projects/markdown/syntax[syntax page] provides complete,
detailed documentation for
```


Render a reference style link with link text

```
% pandoc -t asciidoc
The [syntax page] provides documentation

[syntax page]: http://www.syntaxpage.com
^D
The http://www.syntaxpage.com[syntax page] provides documentation
```


Render an internal link

```
% pandoc -t asciidoc
Refer to [Quick start](#quick-start) to learn how to get started.
^D
Refer to link:#quick-start[Quick start] to learn how to get started.
```


Render an reference style image

```
% pandoc -t asciidoc
![Alt text][logo]

[logo]: images/icons/home.png
^D
image:images/icons/home.png[Alt text]
```


Render an inline image with parameters

```
% pandoc -t asciidoc
![Alt text](images/icons/home.png)

![Alt text](images/icons/home.png?width=100)
^D
image:images/icons/home.png[Alt text]

image:images/icons/home.png?width=100[Alt text]
```


Render an inline image with comma in alt text

```
% pandoc -t asciidoc
![Alt,text](images/icons/home.png)
^D
image:images/icons/home.png["Alt,text"]
```


Render a hyperlinked inline image with alt text

```
% pandoc -t asciidoc
[![Build Status](https://travis-ci.org/asciidoctor/asciidoctor.png)](https://travis-ci.org/asciidoctor/asciidoctor)
^D
image:https://travis-ci.org/asciidoctor/asciidoctor.png[Build Status,link=https://travis-ci.org/asciidoctor/asciidoctor]
```


Render a hyperlinked inline image with no alt text

```
% pandoc -t asciidoc
[![](https://travis-ci.org/asciidoctor/asciidoctor.png)](https://travis-ci.org/asciidoctor/asciidoctor)
^D
image:https://travis-ci.org/asciidoctor/asciidoctor.png[link=https://travis-ci.org/asciidoctor/asciidoctor]
```


