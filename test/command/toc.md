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
-   [A](#a){#toc-a}
    -   [b](#b){#toc-b}
-   [B](#b-1){#toc-b-1}
    -   [b](#b-2){#toc-b-2}
-   [E](#e){#toc-e}
    -   [e](#e-1){#toc-e-1}

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
```
