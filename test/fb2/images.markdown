This example test if Pandoc correctly embeds images into FictionBook.

Small inline image: ![alt text a small PNG image][inline-image].

Paragraph image:

![alt text of a big JPEG image](fb2/test.jpg "image title text")

![alt text of a big missing image](missing.jpg)

A missing image inline: ![alt text of missing image](missing.jpg).

[inline-image]: fb2/test-small.png
