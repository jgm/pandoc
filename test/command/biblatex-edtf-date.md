```
% pandoc -f biblatex -t markdown -s
Note that current CSL doesn't give us a way
to distinguish between /open and /unknown,
so item3-3 and item3-4 get parsed similarly.
That should change in CSL 1.1, and then this
test should be revised.

@article{item3-3, date={1998/unknown}}
@article{item3-4, date={1999/open}}
@article{item3-10, date={2004-04-05T14:34:00}}
@article{item5-1, date={0000}}
@article{item5-2, date={-0876}}
@article{item5-3, date={-0877/-0866}}
@article{item5-5, date={-0343-02}}
@article{item5-8, date={1723~}}
@article{item5-9, date={1723?}}
@article{item5-10, date={1723?~}}
@article{item5-11, date={2004-22}}
@article{item5-12, date={2004-24}}
@article{item5-13, date={20uu}}
@article{item5-14, date={y-123456789}}

^D
---
nocite: "[@*]"
references:
- id: item3-3
  issued: 1998/
  type: article-journal
- id: item3-4
  issued: 1999/
  type: article-journal
- id: item3-10
  issued: 2004-04-05
  type: article-journal
- id: item5-1
  type: article-journal
- id: item5-2
  issued: "-0876"
  type: article-journal
- id: item5-3
  issued: "-0877/-0866"
  type: article-journal
- id: item5-5
  issued: "-0343-02"
  type: article-journal
- id: item5-8
  issued: 1723\~
  type: article-journal
- id: item5-9
  issued: 1723
  type: article-journal
- id: item5-10
  issued: 1723\~
  type: article-journal
- id: item5-11
  issued: 2004-22
  type: article-journal
- id: item5-12
  issued: 2004-24
  type: article-journal
- id: item5-13
  issued: 2000/2099
  type: article-journal
- id: item5-14
  issued: y-123456789
  type: article-journal
---


```
