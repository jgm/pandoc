converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/headings.feature>

Render a level 1 heading

```
% pandoc -t asciidoc
# Title #
^D
= Title
```


Render a level 1 underscored heading

```
% pandoc -t asciidoc
Title
=====
^D
= Title
```


Render a level 2 heading

```
% pandoc -t asciidoc
## Title
^D
== Title
```


Render a level 2 underscored heading

```
% pandoc -t asciidoc
Title
-----
^D
== Title
```


Render a level 3 heading

```
% pandoc -t asciidoc
## Title
^D
== Title
```


Render a level 4 heading

```
% pandoc -t asciidoc
#### Title
^D
==== Title
```


Render a level 5 heading

```
% pandoc -t asciidoc
##### Title
^D
===== Title
```


Render a level 6 heading

```
% pandoc -t asciidoc
###### Title
^D
====== Title
```


Render a heading with different styling

```
% pandoc -t asciidoc
# Title ####
^D
= Title
```


