```
% pandoc --citeproc -t plain --csl command/chicago-fullnote-bibliography.csl
---
suppress-bibliography: true
references:
- id: foo
  name: John doe
  title: A Book
  type: book
  publisher: Oxford University Press
  issued: 2010
...

# Chapter one

Blah [@foo, p. 7].

Blah [@foo, p. 8].

# Chapter two {.reset-citation-positions}

Blah [@foo, p. 57].
^D
Chapter one

Blah.[1]

Blah.[2]

Chapter two

Blah.[3]

[1] A Book (Oxford University Press, 2010), 7.

[2] A Book, 8.

[3] A Book (Oxford University Press, 2010), 57.

```

