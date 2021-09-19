```
% pandoc -f latex -t native
electromagnetic\hyp{}endioscopy
^D
[ Para [ Str "electromagnetic-endioscopy" ] ]
```

```
% pandoc -f latex -t native
C\colonhyp\bshyp{}Windows\bshyp
^D
[ Para [ Str "C:\173\\\173Windows\\\173" ] ]
```

```
% pandoc -f latex -t native
\fshyp{}usr\fshyp{}share\fshyp
^D
[ Para [ Str "/\173usr/\173share/\173" ] ]
```

```
% pandoc -f latex -t native
\fshyp{}home\fshyp{}schrieveslaach\fshyp\dothyp{}m2
^D
[ Para [ Str "/\173home/\173schrieveslaach/\173.\173m2" ] ]
```

```
% pandoc -f latex -t native
\nohyphens{Pneumonoultramicroscopicsilicovolcanoconiosis}
^D
[ Para
    [ Str "Pneumonoultramicroscopicsilicovolcanoconiosis" ]
]
```

```
% pandoc -f latex -t native
\textnhtt{Pneumonoultramicroscopicsilicovolcanoconiosis}
^D
[ Para
    [ Code
        ( "" , [] , [] )
        "Pneumonoultramicroscopicsilicovolcanoconiosis"
    ]
]
```

```
% pandoc -f latex -t native
\nhttfamily{Pneumonoultramicroscopicsilicovolcanoconiosis}
^D
[ Para
    [ Code
        ( "" , [] , [] )
        "Pneumonoultramicroscopicsilicovolcanoconiosis"
    ]
]
```

