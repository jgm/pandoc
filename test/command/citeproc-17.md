```
% pandoc -t csljson
---
references:
- id: foo
  type: book
  title: "Hi & Low"
...
^D
[
  {
    "id": "foo",
    "title": "Hi & Low",
    "type": "book"
  }
]
```
