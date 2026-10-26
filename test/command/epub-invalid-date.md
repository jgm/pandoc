Dates that cannot be normalized are skipped, so that `dc:date`
is not empty.

```
% pandoc -t epub3 -o - | pandoc -f epub -t plain -s
---
title: T
date: [10 septembre 2026, 2026-09-10]
---
^D
T
2026-09-10

T
```
