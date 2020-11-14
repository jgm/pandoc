```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
csl: command/bioethics.csl
references:
  - id: test
    title: Test
    author:
      family: Doe
      given: John
---

Irrelevant.^[note]

Test [@test, 12].

Test.^[asdfasdf]

Test [@test, 12].
^D
Irrelevant.[^1]

Test.[^2]

Test.[^3]

Test.[^4]

[^1]: note

[^2]: J. Doe. ***Test***: 12.

[^3]: asdfasdf

[^4]: Doe (cited n. 2) : 12.
```
