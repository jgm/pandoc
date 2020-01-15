```
% pandoc -t latex --biblatex
[e.g. @a1;@a2;@a3; but also @b1;@b2;@b3]
^D
\autocites[e.g.~][]{a1,a2,a3}[but also][]{b1,b2,b3}
```
```
% pandoc -t latex --biblatex
[e.g. @a1; e.g. @a2;@a3; but also @b1;@b2;but also @b3]
^D
\autocites[e.g.~][]{a1}[e.g.~][]{a2,a3}[but also][]{b1,b2}[but
also][]{b3}
```


