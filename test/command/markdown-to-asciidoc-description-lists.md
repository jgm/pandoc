converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/descriptionlists.feature>

Render a description list

```
% pandoc -t asciidoc
Apple
:   Pomaceous fruit of plants of the genus Malus in
the family Rosaceae.
^D
Apple::
  Pomaceous fruit of plants of the genus Malus in the family Rosaceae.
```


Render a multiple description lists

```
% pandoc -t asciidoc
Apple
:   Pomaceous fruit of plants of the genus Malus in
the family Rosaceae.

Orange
:   The fruit of an evergreen tree of the genus Citrus.
^D
Apple::
  Pomaceous fruit of plants of the genus Malus in the family Rosaceae.
Orange::
  The fruit of an evergreen tree of the genus Citrus.
```


