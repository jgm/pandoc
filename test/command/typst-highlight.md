Highlighted text should be read as a `mark` span, round-tripping with
the Typst writer.

```
% pandoc -f typst -t native
#highlight[hello]
^D
[ Para [ Span ( "" , [ "mark" ] , [] ) [ Str "hello" ] ] ]
```

```
% pandoc -f markdown -t typst
[hello]{.mark}
^D
#highlight[hello]
```

```
% pandoc -f typst -t typst
#highlight[hello]
^D
#highlight[hello]
```
