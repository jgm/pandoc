```
% pandoc -f latex -t native
\section{Intro}\label
{sec:intro}
See Section~\ref
{sec:intro}.
^D
[ Header 1 ( "sec:intro" , [] , [] ) [ Str "Intro" ]
, Para
    [ Str "See"
    , Space
    , Str "Section\160"
    , Link
        ( ""
        , []
        , [ ( "reference-type" , "ref" )
          , ( "reference" , "sec:intro" )
          ]
        )
        [ Str "1" ]
        ( "#sec:intro" , "" )
    , Str "."
    ]
]
```
