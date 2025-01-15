```
% pandoc --citeproc --csl command/chicago-fullnote-bibliography.csl -t plain
---
references:
- id: foo
  type: book
  author: John Doe
  title: A Book
...

See @foo [p. 21], as well.
^D
See John Doe,[1] as well.

John Doe. A Book, n.d.

[1] A Book, n.d., 21.
```

