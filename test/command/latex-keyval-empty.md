Key-value options can have empty values and empty entries.

```
% pandoc -f latex -t native
\includegraphics[clip=,width=1cm]{a.png}
^D
[ Para
    [ Image
        ( "" , [] , [ ( "width" , "1cm" ) ] )
        [ Str "image" ]
        ( "a.png" , "" )
    ]
]
```

```
% pandoc -f latex -t native
\includegraphics[width=1cm,,clip,]{a.png}
^D
[ Para
    [ Image
        ( "" , [] , [ ( "width" , "1cm" ) ] )
        [ Str "image" ]
        ( "a.png" , "" )
    ]
]
```
