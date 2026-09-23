converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/paragraphs.feature>

Render a paragraph with a single line

```
% pandoc -t asciidoc
A paragraph with a single line.
^D
A paragraph with a single line.
```


Render multiple paragraphs seperated by a blank line

```
% pandoc -t asciidoc
First paragraph.

Second paragraph.
^D
First paragraph.

Second paragraph.
```


