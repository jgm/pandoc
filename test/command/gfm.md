gfm tests:

```
% pandoc -f gfm -t native
| Fruit | Price |
| ----- | ----: |
| apple | 0.13  |
| orange|1.12|
^D
[Table ("",[],[]) (Caption Nothing
 [])
 [(AlignDefault,ColWidthDefault)
 ,(AlignRight,ColWidthDefault)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Fruit"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Price"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "apple"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "0.13"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "orange"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "1.12"]]]])]
 (TableFoot ("",[],[])
 [])]
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
[Para [Quoted DoubleQuote [Str "hi"]]]
```

```
% pandoc -t gfm -f native
[Table ("",[],[]) (Caption Nothing
 [Plain [Str "The",Space,Str "caption."]])
 [(AlignDefault,ColWidthDefault)
 ,(AlignRight,ColWidthDefault)]
 (TableHead ("",[],[])
 [Row ("",[],[])
  [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Fruit"]]
  ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
   [Plain [Str "Price"]]]])
 [(TableBody ("",[],[]) (RowHeadColumns 0)
  []
  [Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "apple"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "0.13"]]]
  ,Row ("",[],[])
   [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "orange"]]
   ,Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1)
    [Plain [Str "1.12"]]]])]
 (TableFoot ("",[],[])
 [])]
^D
| Fruit  | Price |
|--------|------:|
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

```
% pandoc -f gfm -t native
- [ ] foo
- [x] bar
^D
[BulletList
 [[Plain [Str "\9744",Space,Str "foo"]]
 ,[Plain [Str "\9746",Space,Str "bar"]]]]
```

```
% pandoc -f gfm-task_lists -t native
- [ ] foo
- [x] bar
^D
[BulletList
 [[Plain [Str "[",Space,Str "]",Space,Str "foo"]]
 ,[Plain [Str "[x]",Space,Str "bar"]]]]
```

```
% pandoc -f gfm -t gfm
- [ ] foo
- [x] bar
^D
-   [ ] foo
-   [x] bar
```
