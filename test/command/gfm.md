gfm tests:

```
% pandoc -f gfm -t native
| Fruit | Price |
| ----- | ----: |
| apple | 0.13  |
| orange|1.12|
^D
[Table [] [AlignDefault,AlignRight] [0.0,0.0]
 [[Plain [Str "Fruit"]]
 ,[Plain [Str "Price"]]]
 [[[Plain [Str "apple"]]
  ,[Plain [Str "0.13"]]]
 ,[[Plain [Str "orange"]]
  ,[Plain [Str "1.12"]]]]]
```

```
% pandoc -f gfm -t native
~~stricken out~~
^D
[Para [Strikeout [Str "stricken",Space,Str "out"]]]
```

```
% pandoc -f gfm -t native
# Header
## Header
# -foo-bar_baz
^D
[Header 1 ("header",[],[]) [Str "Header"]
,Header 2 ("header-1",[],[]) [Str "Header"]
,Header 1 ("-foo-bar_baz",[],[]) [Str "-foo-bar_baz"]]
```

```
% pandoc -f gfm -t native
My:thumbsup:emoji:heart:
^D
[Para [Str "My",Span ("",["emoji"],[("data-emoji","thumbsup")]) [Str "\128077"],Str "emoji",Span ("",["emoji"],[("data-emoji","heart")]) [Str "\10084\65039"]]]
```

```
% pandoc -f gfm -t native
"hi"
^D
[Para [Str "\"hi\""]]
```

```
% pandoc -f gfm+smart -t native
"hi"
^D
[Para [Str "\8220hi\8221"]]
```

```
% pandoc -t gfm -f native
[Table [Str "The",Space,Str "caption."] [AlignDefault,AlignRight] [0.0,0.0]
 [[Plain [Str "Fruit"]]
 ,[Plain [Str "Price"]]]
 [[[Plain [Str "apple"]]
  ,[Plain [Str "0.13"]]]
 ,[[Plain [Str "orange"]]
  ,[Plain [Str "1.12"]]]]]
^D
| Fruit  | Price |
| ------ | ----: |
| apple  |  0.13 |
| orange |  1.12 |

The caption.
```

```
% pandoc -f gfm-smart -t gfm+smart
“hi”
^D
"hi"
```

```
% pandoc -f gfm+smart -t gfm-smart
"hi"
^D
“hi”
```

```
% pandoc -f gfm+smart -t gfm+smart
"hi"
^D
"hi"
```

```
% pandoc -f gfm+hard_line_breaks -t native
hi
hi
^D
[Para [Str "hi",LineBreak,Str "hi"]]
```
