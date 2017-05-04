```
% pandoc -f latex -t native
Figure \ref{fig:1}
^D
[Para [Str "Figure",Space,Span ("",[],[("data-reference","fig:1")]) [Str "[fig:1]"]]]
```

```
% pandoc -f latex -t native
Figure \cref{fig:1}
^D
[Para [Str "Figure",Space,Span ("",[],[("data-reference","fig:1")]) [Str "[fig:1]"]]]
```

```
% pandoc -f latex -t native
Figure \vref{fig:1}
^D
[Para [Str "Figure",Space,Span ("",[],[("data-reference","fig:1")]) [Str "[fig:1]"]]]
```

