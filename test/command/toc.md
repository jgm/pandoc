```
% pandoc -s --toc -t markdown
# A
## b
# B
## b

::: interior
# C
## cc
# D
:::

::: blue
# E
## e
:::
^D
-   [A](#a)
    -   [b](#b)
-   [B](#b-1)
    -   [b](#b-2)
-   [E](#e)
    -   [e](#e-1)

A
=

b
-

B
=

b
-

::: {.interior}
C
=

cc
--

D
=
:::

::: {.blue}
E
=

e
-
:::
```
