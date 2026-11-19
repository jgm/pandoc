\and in \address separates affiliations.

```
% pandoc -s -f latex -t native
\title{T}
\author{A}
\address{Mathematics \and Computer Science}
\begin{document}
\maketitle
\end{document}
^D
Pandoc
  Meta
    { unMeta =
        fromList
          [ ( "address"
            , MetaInlines
                [ Str "Mathematics"
                , LineBreak
                , Str "Computer"
                , Space
                , Str "Science"
                ]
            )
          , ( "author" , MetaList [ MetaInlines [ Str "A" ] ] )
          , ( "title" , MetaInlines [ Str "T" ] )
          ]
    }
  []
```
