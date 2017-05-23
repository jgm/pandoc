```
% pandoc -f latex -t native
Figure \ref{fig:1}
^D
[Para [Str "Figure",Space,Span ("",[],[("data-reference-type","ref"),("data-reference","fig:1")]) [Str "[fig:1]"]]]
```

```
% pandoc -f latex -t native
Figure \cref{fig:1}
^D
[Para [Str "Figure",Space,Span ("",[],[("data-reference-type","ref"),("data-reference","fig:1")]) [Str "[fig:1]"]]]
```

```
% pandoc -f latex -t native
Figure \vref{fig:1}
^D
[Para [Str "Figure",Space,Span ("",[],[("data-reference-type","ref+page"),("data-reference","fig:1")]) [Str "[fig:1]"]]]
```

```
% pandoc -f latex -t native
Accuracy~\eqref{eq:Accuracy} is the proportion, measuring true results among all results.

\begin{equation}
  \label{eq:Accuracy}
  Accuracy = \frac{t_p + t_n}{t_p + f_p + f_n + t_n}
\end{equation}
^D
[Para [Str "Accuracy\160",Span ("",[],[("data-reference-type","eqref"),("data-reference","eq:Accuracy")]) [Str "[eq:Accuracy]"],Space,Str "is",Space,Str "the",Space,Str "proportion,",Space,Str "measuring",Space,Str "true",Space,Str "results",Space,Str "among",Space,Str "all",Space,Str "results."]
,Para [Math DisplayMath "\\label{eq:Accuracy}\n  Accuracy = \\frac{t_p + t_n}{t_p + f_p + f_n + t_n}"]]
```
