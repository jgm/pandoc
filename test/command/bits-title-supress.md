```
% pandoc -f jats -t native
<sec>
	<title>The European Parliament</title>
	<p>Members of the European Parliament (MEPs) are directly elected by EU citizens.</p>
</sec>
^D
[ Header
    1
    ( "" , [] , [] )
    [ Str "The"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    ]
, Para
    [ Str "Members"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    , Space
    , Str "(MEPs)"
    , Space
    , Str "are"
    , Space
    , Str "directly"
    , Space
    , Str "elected"
    , Space
    , Str "by"
    , Space
    , Str "EU"
    , Space
    , Str "citizens."
    ]
]
```

```
% pandoc -f jats -t native
<sec>
	<title supress="no">The European Parliament</title>
	<p>Members of the European Parliament (MEPs) are directly elected by EU citizens.</p>
</sec>
^D
[ Header
    1
    ( "" , [] , [] )
    [ Str "The"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    ]
, Para
    [ Str "Members"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    , Space
    , Str "(MEPs)"
    , Space
    , Str "are"
    , Space
    , Str "directly"
    , Space
    , Str "elected"
    , Space
    , Str "by"
    , Space
    , Str "EU"
    , Space
    , Str "citizens."
    ]
]
```

```
% pandoc -f jats -t native
<sec>
	<title supress="yes">The European Parliament</title>
	<p>Members of the European Parliament (MEPs) are directly elected by EU citizens.</p>
</sec>
^D
[ Para
    [ Str "Members"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    , Space
    , Str "(MEPs)"
    , Space
    , Str "are"
    , Space
    , Str "directly"
    , Space
    , Str "elected"
    , Space
    , Str "by"
    , Space
    , Str "EU"
    , Space
    , Str "citizens."
    ]
]
```