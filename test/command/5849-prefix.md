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
```
% pandoc -t latex --biblatex
[e.g. @a1, ch.3 and elsewhere;@a2;@a3; but also @a4;@a5]
^D
\autocites[e.g.~][ch.3 and elsewhere]{a1}{a2,a3}[but also][]{a4,a5}
```
```
% pandoc -t latex --biblatex
[e.g. @a1;@a2, ch.3 and elsewhere;@a3; but also @a4;@a5]
^D
\autocites[e.g.~][ch.3 and elsewhere]{a1,a2}{a3}[but also][]{a4,a5}
```
```
% pandoc -t latex --biblatex
[e.g. @a1, blah;@a2, ch.3 and elsewhere;@a3; but also @b4;@b5]
^D
\autocites[e.g.~][blah]{a1}[ch.3 and elsewhere]{a2}{a3}[but
also][]{b4,b5}
```
