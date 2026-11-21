```
% pandoc -f latex -t native
\begin{lstlisting}[language=Python,caption={Main loop}\label{lst:main},numbers=left]
print(1)
\end{lstlisting}

\begin{lstlisting}[caption={A, B},label=lst:b]
print(2)
\end{lstlisting}
^D
[ CodeBlock
    ( ""
    , [ "numberLines" , "python" ]
    , [ ( "language" , "Python" )
      , ( "caption" , "{Main loop}\\label{lst:main}" )
      , ( "numbers" , "left" )
      ]
    )
    "print(1)"
, CodeBlock
    ( "lst:b"
    , []
    , [ ( "caption" , "A, B" ) , ( "label" , "lst:b" ) ]
    )
    "print(2)"
]
```
