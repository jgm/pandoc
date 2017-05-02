```
% pandoc -f latex -t native
\textcolor{red}{Hello World}
^D
[Para [Span ("",[],[("style","color: red")]) [Str "Hello",Space,Str "World"]]]
```

```
% pandoc -f latex -t native
\textcolor{blue}{Hello \textbf{World}}
^D
[Para [Span ("",[],[("style","color: blue")]) [Str "Hello",Space,Strong [Str "World"]]]]
```
