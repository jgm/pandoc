```
% pandoc -f native -t markdown
[Para [Str "a", Note [Para [Str "outer", Note [Para [Str "inner"]]]], Str "b", Note [Para [Str "second"]]]]
^D
a[^1]b[^2]

[^1]: outer[^3]

[^2]: second

[^3]: inner
```

```
% pandoc -f native -t rst
[Para [Str "a", Note [Para [Str "outer", Note [Para [Str "inner"]]]], Str "b", Note [Para [Str "second"]]]]
^D
a [1]_b [2]_

.. [1]
   outer [3]_

.. [2]
   second

.. [3]
   inner
```

```
% pandoc -f native -t haddock
[Para [Str "a", Note [Para [Str "outer", Note [Para [Str "inner"]]]], Str "b", Note [Para [Str "second"]]]]
^D
a<#notes [1]>b<#notes [2]>

#notes#

1.  outer<#notes [3]>

2.  second

3.  inner
```
