```
% pandoc -f man -t plain
.de test
ok
..
.test
.br
\A'test'
.br
\A'xyz'
^D
ok
1
0
```
