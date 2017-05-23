```
% pandoc -f latex -t native
Hello \textcolor{red}{World}
^D
[Para [Str "Hello",Space,Span ("",[],[("style","color: red")]) [Str "World"]]]
```

```
% pandoc -f latex -t native
\textcolor{red}{Hello} World
^D
[Para [Span ("",[],[("style","color: red")]) [Str "Hello"],Space,Str "World"]]
```

```
% pandoc -f latex -t native
Hello \textcolor{blue}{\textbf{World}}
^D
[Para [Str "Hello",Space,Span ("",[],[("style","color: blue")]) [Strong [Str "World"]]]]
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
[Div ("",[],[("style","color: orange")])
 [BulletList
  [[Para [Str "Item",Space,Str "1"]]
  ,[Para [Str "Item",Space,Str "2"]]]]]
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
[Div ("",[],[("style","color: blue")])
 [BulletList
  [[Para [Str "Item",Space,Str "1"]]
  ,[Para [Str "Item",Space,Str "2"]]]]
,Para [Str "some",Space,Str "more",Space,Str "text"]]
```

