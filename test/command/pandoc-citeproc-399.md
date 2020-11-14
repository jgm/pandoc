```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
csl: command/style399.csl
references:
- author:
  - family: One
  id: one
- author:
  - family: Two
  id: two
- author:
  - family: Three
  id: three
- author:
  - family: Four
  id: four
- author:
  - family: Five
  id: five
- author:
  - family: Six
  id: six
- author:
  - family: Seven
  id: seven
- author:
  - family: Eight
  id: eight
- author:
  - family: Nine
  id: nine
- author:
  - family: Ten
  id: ten
- author:
  - family: Eleven
  id: eleven
---

Inline citation
[@one; @two; @three; @four; @five; @six; @seven; @eight; @nine; @ten; @eleven].
^D
Inline citation \[1,2,3,4,5,6,7,8,9,10,11\].
```
