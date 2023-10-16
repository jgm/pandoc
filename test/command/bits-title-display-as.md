```
% pandoc -f jats -t native
<sec>
	<title>THE EUROPEAN UNION  EXPLAINED</title>
</sec>
^D
[ Header
    1
    ( "" , [] , [] )
    [ Str "THE"
    , Space
    , Str "EUROPEAN"
    , Space
    , Str "UNION"
    , Space
    , Str "EXPLAINED"
    ]
]
```

```
% pandoc -f jats -t native
<sec>
	<title display-as="3">THE EUROPEAN UNION  EXPLAINED</title>
</sec>
^D
[ Header
    3
    ( "" , [] , [] )
    [ Str "THE"
    , Space
    , Str "EUROPEAN"
    , Space
    , Str "UNION"
    , Space
    , Str "EXPLAINED"
    ]
]
```

```
% pandoc -f jats -t native
<body>
	<sec>
		<title>The European Parliament</title>
		<p>Members of the European Parliament (MEPs) are directly elected by EU citizens.</p>
		<sec>
		    <title display-as="3">Composition of the European Parliament</title>
			<p>The seats in the European Parliament are allocated among the Member States.</p>
		</sec>
        <sec>
            <title>Composition of the European Parliament - II </title>
            <p>Most MEPs are associated with a national political party in their home country.</p>
		</sec>
    </sec>
</body>
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
, Header
    3
    ( "" , [] , [] )
    [ Str "Composition"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    ]
, Para
    [ Str "The"
    , Space
    , Str "seats"
    , Space
    , Str "in"
    , Space
    , Str "the"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    , Space
    , Str "are"
    , Space
    , Str "allocated"
    , Space
    , Str "among"
    , Space
    , Str "the"
    , Space
    , Str "Member"
    , Space
    , Str "States."
    ]
, Header
    2
    ( "" , [] , [] )
    [ Str "Composition"
    , Space
    , Str "of"
    , Space
    , Str "the"
    , Space
    , Str "European"
    , Space
    , Str "Parliament"
    , Space
    , Str "-"
    , Space
    , Str "II"
    ]
, Para
    [ Str "Most"
    , Space
    , Str "MEPs"
    , Space
    , Str "are"
    , Space
    , Str "associated"
    , Space
    , Str "with"
    , Space
    , Str "a"
    , Space
    , Str "national"
    , Space
    , Str "political"
    , Space
    , Str "party"
    , Space
    , Str "in"
    , Space
    , Str "their"
    , Space
    , Str "home"
    , Space
    , Str "country."
    ]
]
```