# `\textcolor{}{}`

```
% pandoc -f latex -t native
Hello \textcolor{red}{World}
^D
[ Para
    [ Str "Hello "
    , Span
        ( "" , [] , [ ( "style" , "color: red" ) ] ) [ Str "World" ]
    ]
]
```

```
% pandoc -f latex -t native
\textcolor{red}{Hello} World
^D
[ Para
    [ Span
        ( "" , [] , [ ( "style" , "color: red" ) ] ) [ Str "Hello" ]
    , Str " World"
    ]
]
```

```
% pandoc -f latex -t native
Hello \textcolor{blue}{\textbf{World}}
^D
[ Para
    [ Str "Hello "
    , Span
        ( "" , [] , [ ( "style" , "color: blue" ) ] )
        [ Strong [ Str "World" ] ]
    ]
]
```


```
% pandoc -f latex -t native
Hello \textcolor{blue}{\textbf{World}}.
^D
[ Para
    [ Str "Hello "
    , Span
        ( "" , [] , [ ( "style" , "color: blue" ) ] )
        [ Strong [ Str "World" ] ]
    , Str "."
    ]
]
```

```
% pandoc -f latex -t native
\textcolor{orange}{
\begin{itemize}	
    \item Item 1
    \item Item 2
\end{itemize}
}
^D
[ Div
    ( "" , [] , [ ( "style" , "color: orange" ) ] )
    [ BulletList
        [ [ Para [ Str "Item 1" ] ] , [ Para [ Str "Item 2" ] ] ]
    ]
]
```

```
% pandoc -f latex -t native
\textcolor{blue}{
\begin{itemize}
    \item Item 1
    \item Item 2
\end{itemize}
} some more text
^D
[ Div
    ( "" , [] , [ ( "style" , "color: blue" ) ] )
    [ BulletList
        [ [ Para [ Str "Item 1" ] ] , [ Para [ Str "Item 2" ] ] ]
    ]
, Para [ Str "some more text" ]
]
```

# `\colorbox{}{}`


```
% pandoc -f latex -t native
Hello \colorbox{red}{World}
^D
[ Para
    [ Str "Hello "
    , Span
        ( "" , [] , [ ( "style" , "background-color: red" ) ] )
        [ Str "World" ]
    ]
]
```

```
% pandoc -f latex -t native
\colorbox{red}{Hello} World
^D
[ Para
    [ Span
        ( "" , [] , [ ( "style" , "background-color: red" ) ] )
        [ Str "Hello" ]
    , Str " World"
    ]
]
```

```
% pandoc -f latex -t native
Hello \colorbox{blue}{\textbf{World}}
^D
[ Para
    [ Str "Hello "
    , Span
        ( "" , [] , [ ( "style" , "background-color: blue" ) ] )
        [ Strong [ Str "World" ] ]
    ]
]
```

```
% pandoc -f latex -t native
Hello \colorbox{blue}{\textbf{World}}.
^D
[ Para
    [ Str "Hello "
    , Span
        ( "" , [] , [ ( "style" , "background-color: blue" ) ] )
        [ Strong [ Str "World" ] ]
    , Str "."
    ]
]
```

```
% pandoc -f latex -t native
\colorbox{orange}{
\begin{minipage}{\textwidth}
\begin{itemize}	
    \item Item 1
    \item Item 2
\end{itemize}
\end{minipage}
}
^D
[ Div
    ( "" , [] , [ ( "style" , "background-color: orange" ) ] )
    [ BulletList
        [ [ Para [ Str "Item 1" ] ] , [ Para [ Str "Item 2" ] ] ]
    ]
]
```

```
% pandoc -f latex -t native
\colorbox{blue}{
\begin{minipage}{\textwidth}
\begin{itemize}
    \item Item 1
    \item Item 2
\end{itemize}
\end{minipage}
} some more text
^D
[ Div
    ( "" , [] , [ ( "style" , "background-color: blue" ) ] )
    [ BulletList
        [ [ Para [ Str "Item 1" ] ] , [ Para [ Str "Item 2" ] ] ]
    ]
, Para [ Str "some more text" ]
]
```
