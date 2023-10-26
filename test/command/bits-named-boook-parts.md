```
% pandoc -f jats -t native
<dedication>
	<named-book-part-body>
		<p>This is the dedication text.</p>
	</named-book-part-body>
</dedication>
^D
[ Header 1 ( "" , [] , [] ) [ Str "Dedication" ]
, Para
    [ Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "dedication"
    , Space
    , Str "text."
    ]
]
```

```
% pandoc -f jats -t native
<foreword>
	<named-book-part-body>
		<p>This is the foreword text.</p>
	</named-book-part-body>
</foreword>
^D
[ Header 1 ( "" , [] , [] ) [ Str "Foreword" ]
, Para
    [ Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "foreword"
    , Space
    , Str "text."
    ]
]
```

```
% pandoc -f jats -t native
<preface>
	<named-book-part-body>
		<p>This is the preface text.</p>
	</named-book-part-body>
</preface>
^D
[ Header 1 ( "" , [] , [] ) [ Str "Preface" ]
, Para
    [ Str "This"
    , Space
    , Str "is"
    , Space
    , Str "the"
    , Space
    , Str "preface"
    , Space
    , Str "text."
    ]
]
```