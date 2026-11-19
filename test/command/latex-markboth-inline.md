\markboth may occur in a heading.

```
% pandoc -f latex -t native
\chapter*{Summary\markboth{Summary}{Summary}}
Text.
^D
[ Header
    1 ( "summary" , [ "unnumbered" ] , [] ) [ Str "Summary" ]
, Para [ Str "Text." ]
]
```
