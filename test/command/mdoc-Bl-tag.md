```
% pandoc -f mdoc -t native
.Bl -tag
.It one
one
.It Xo
two
.Xc
two
.It Em three Xo
three
.Xc
three
.It Po one two
.Bq three
.Pc
four
.El
^D
[ DefinitionList
    [ ( [ Str "one" ] , [ [ Para [ Str "one" ] ] ] )
    , ( [ Str "two" ] , [ [ Para [ Str "two" ] ] ] )
    , ( [ Emph [ Str "three" ] , Space , Str "three" ]
      , [ [ Para [ Str "three" ] ] ]
      )
    , ( [ Str "(one"
        , Space
        , Str "two"
        , Space
        , Str "[three])"
        ]
      , [ [ Para [ Str "four" ] ] ]
      )
    ]
]
```
