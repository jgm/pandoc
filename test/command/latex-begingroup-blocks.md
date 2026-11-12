```
% pandoc -f latex -t native
{
\hypersetup{colorlinks=true}

\begingroup
\section{Inside}
\endgroup
}

\begingroup\bfseries bold\endgroup{} plain

\begingroup Unclosed.
^D
[ Header 1 ( "inside" , [] , [] ) [ Str "Inside" ]
, Para
    [ Span ( "" , [] , [] ) [ Strong [ Str "bold" ] ]
    , Space
    , Str "plain"
    ]
, Para [ Str "Unclosed." ]
]
```

```
% pandoc -f latex -t native
\begingroup
One.

\begingroup
Two.

\begingroup
Three.
^D
[ Para [ Str "One." ]
, Para [ Str "Two." ]
, Para [ Str "Three." ]
]
```
