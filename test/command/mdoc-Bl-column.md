```
% pandoc -f mdoc -t djot
.Bl -column a b
.It a Ta
.Sy b
b
.Ta c
.Pp
.It c Ta d Ta \&
.It Em e
.Ta f
.Ta g
.It h Ta Ta j
.It Pq a Ta b Ta c
.El
^D
| a   | *b* b | c |
| c   | d     |   |
| _e_ | f     | g |
| h   |       | j |
| (a) | b     | c |
```
