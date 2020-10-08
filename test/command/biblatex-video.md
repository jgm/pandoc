```
% pandoc -f biblatex -t markdown -s
@video{x1,title={blah}}
@movie{x2,title={blah}}

^D
---
nocite: "[@*]"
references:
- id: x1
  title: Blah
  type: motion_picture
- id: x2
  title: Blah
  type: motion_picture
---


```
