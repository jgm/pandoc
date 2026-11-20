As in TeX, a command's arguments may be preceded by spaces and a
single newline:

```
% pandoc -f latex -t native
\section{Intro}\label
{sec:intro}
See Section~\ref
{sec:intro}, \href
{https://example.com}
{a link}, \textcolor{red} % comment
{red}, and \foreignlanguage
{french}{oui}.
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
    , Str ","
    , Space
    , Link
        ( "" , [] , [] )
        [ Str "a" , Space , Str "link" ]
        ( "https://example.com" , "" )
    , Str ","
    , Space
    , Span
        ( "" , [] , [ ( "style" , "color: red" ) ] ) [ Str "red" ]
    , Str ","
    , Space
    , Str "and"
    , Space
    , Span ( "" , [] , [ ( "lang" , "fr" ) ] ) [ Str "oui" ]
    , Str "."
    ]
]
```

```
% pandoc -f latex -t native
\newcommand
{\foo}[2][d]{<#1|#2>}
\foo
[a]{b} \foo
{c}
^D
[ Para [ Str "<a|b>" , Space , Str "<d|c>" ] ]
```

```
% pandoc -f latex -t native
\begin{tabular}{ll}
\multicolumn
{1}{r}
{a} & b \\
\end{tabular}
^D
[ Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignLeft , ColWidthDefault )
    , ( AlignLeft , ColWidthDefault )
    ]
    (TableHead ( "" , [] , [] ) [])
    [ TableBody
        ( "" , [] , [] )
        (RowHeadColumns 0)
        []
        [ Row
            ( "" , [] , [] )
            [ Cell
                ( "" , [] , [] )
                AlignRight
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "a" ] ]
            , Cell
                ( "" , [] , [] )
                AlignDefault
                (RowSpan 1)
                (ColSpan 1)
                [ Plain [ Str "b" ] ]
            ]
        ]
    ]
    (TableFoot ( "" , [] , [] ) [])
]
```

```
% pandoc -f latex -t native
\includegraphics
[width=1in]
{x.png}
^D
[ Para
    [ Image
        ( "" , [] , [ ( "width" , "1in" ) ] )
        [ Str "image" ]
        ( "x.png" , "" )
    ]
]
```

A blank line is not skipped; as in TeX, it yields a `\par` token,
which becomes the argument of a user-defined macro:

```
% pandoc -f latex -t native
\newcommand{\foo}[1]{<#1>}
\foo

{x} y
^D
[ Para [ Str "<" ]
, Para
    [ Str ">"
    , Span ( "" , [] , [] ) [ Str "x" ]
    , Space
    , Str "y"
    ]
]
```
