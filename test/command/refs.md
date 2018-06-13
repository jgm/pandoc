```
% pandoc -f latex -t native
Figure \ref{fig:1}
^D
[Para [Str "Figure",Space,Link ("",[],[("reference-type","ref"),("reference","fig:1")]) [Str "[fig:1]"] ("#fig:1","")]]
```

```
% pandoc -f latex -t native
Figure \cref{fig:1}
^D
[Para [Str "Figure",Space,Link ("",[],[("reference-type","ref"),("reference","fig:1")]) [Str "[fig:1]"] ("#fig:1","")]]
```

```
% pandoc -f latex -t native
Figure \vref{fig:1}
^D
[Para [Str "Figure",Space,Link ("",[],[("reference-type","ref+page"),("reference","fig:1")]) [Str "[fig:1]"] ("#fig:1","")]]
```

```
% pandoc -f latex -t native
Accuracy~\eqref{eq:Accuracy} is the proportion, measuring true results among all results.

\begin{equation}
  \label{eq:Accuracy}
  Accuracy = \frac{t_p + t_n}{t_p + f_p + f_n + t_n}
\end{equation}
^D
[Para [Str "Accuracy\160",Link ("",[],[("reference-type","eqref"),("reference","eq:Accuracy")]) [Str "[eq:Accuracy]"] ("#eq:Accuracy",""),Space,Str "is",Space,Str "the",Space,Str "proportion,",Space,Str "measuring",Space,Str "true",Space,Str "results",Space,Str "among",Space,Str "all",Space,Str "results."]
,Para [Math DisplayMath "\\label{eq:Accuracy}\n  Accuracy = \\frac{t_p + t_n}{t_p + f_p + f_n + t_n}"]]
```

```
% pandoc -f latex -t native
\begin{figure}
  \includegraphics{command/SVG_logo.svg}
  \caption{Logo}
  \label{fig:Logo}
\end{figure}

Figure \ref{fig:Logo} illustrated the SVG logo
^D
[Para [Image ("fig:Logo",[],[]) [Str "Logo",Span ("",[],[("label","fig:Logo")]) []] ("command/SVG_logo.svg","fig:")]
,Para [Str "Figure",Space,Link ("",[],[("reference-type","ref"),("reference","fig:Logo")]) [Str "[fig:Logo]"] ("#fig:Logo",""),Space,Str "illustrated",Space,Str "the",Space,Str "SVG",Space,Str "logo"]]
```

```
% pandoc -f latex -t native
\label{section} Section \ref{section}
^D
[Para [Span ("section",[],[("label","section")]) [Str "[section]"],Space,Str "Section",Space,Link ("",[],[("reference-type","ref"),("reference","section")]) [Str "[section]"] ("#section","")]]
```
