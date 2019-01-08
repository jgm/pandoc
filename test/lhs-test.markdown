lhs test
========

`unsplit` is an arrow that takes a pair of values and combines them to
return a single value:

``` {.haskell .literate}
unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry
          -- arr (\op (x,y) -> x `op` y)
```

`(***)` combines two arrows into a new arrow by running the two arrows on a
pair of values (one arrow on the first item of the pair and one arrow on the
second item of the pair).

    f *** g = first f >>> second g

Block quote:

> foo bar
