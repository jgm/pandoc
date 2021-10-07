```
% pandoc -f latex -t native
\begin{lstlisting}[language=Java, caption={Java Example}, label=lst:Hello-World]
public class World {
    public static void main(String[] args) {
        System.out.println("Hello World");
    }
}
\end{lstlisting}
^D
[ CodeBlock
    ( "lst:Hello-World"
    , [ "java" ]
    , [ ( "language" , "Java" )
      , ( "caption" , "Java Example" )
      , ( "label" , "lst:Hello-World" )
      ]
    )
    "public class World {\n    public static void main(String[] args) {\n        System.out.println(\"Hello World\");\n    }\n}"
]
```

```
% pandoc -f latex -t native
\begin{lstlisting}[language=Java, escapechar=|, caption={Java Example}, label=lst:Hello-World]
public class World {
    public static void main(String[] args) {
        System.out.println("Hello World");
    }
}
\end{lstlisting}
^D
[ CodeBlock
    ( "lst:Hello-World"
    , [ "java" ]
    , [ ( "language" , "Java" )
      , ( "escapechar" , "|" )
      , ( "caption" , "Java Example" )
      , ( "label" , "lst:Hello-World" )
      ]
    )
    "public class World {\n    public static void main(String[] args) {\n        System.out.println(\"Hello World\");\n    }\n}"
]
```
