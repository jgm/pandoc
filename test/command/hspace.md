`\hspace` and `\vspace` should count as both block and inline.

Here they need to be inline:
```
% pandoc -f markdown+raw_tex -t native
\begin{figure}
\includegraphics{lalune.jpg}
\caption{lalune \hspace{2em} \vspace{1em} bloo}
\end{figure}
^D
[RawBlock (Format "tex") "\\begin{figure}\n\\includegraphics{lalune.jpg}\n\\caption{lalune \\hspace{2em} \\vspace{1em} bloo}\n\\end{figure}"]
```

Here block:
```
% pandoc -f markdown+raw_tex -t native
\begin{tabular}[t]{cc|c}
\(P\) & \(Q\) & \(P\wedge Q\)\\
\hline
T & T &\\
T & F &\\
F & T &\\
F & F &\\
\end{tabular}
\hspace{1em}
\begin{tabular}[t]{cc|c}
\(P\) & \(Q\) & \(P\vee Q\)\\
\hline
T & T &\\
T & F &\\
F & T &\\
F & F &\\
\end{tabular}
^D
[RawBlock (Format "tex") "\\begin{tabular}[t]{cc|c}\n\\(P\\) & \\(Q\\) & \\(P\\wedge Q\\)\\\\\n\\hline\nT & T &\\\\\nT & F &\\\\\nF & T &\\\\\nF & F &\\\\\n\\end{tabular}\n\\hspace{1em}\n\\begin{tabular}[t]{cc|c}\n\\(P\\) & \\(Q\\) & \\(P\\vee Q\\)\\\\\n\\hline\nT & T &\\\\\nT & F &\\\\\nF & T &\\\\\nF & F &\\\\\n\\end{tabular}"]
```

```
% pandoc -f markdown+raw_tex -t native
hi\hspace{1em}there
^D
[Para [Str "hi",RawInline (Format "tex") "\\hspace{1em}",Str "there"]]
```

```
% pandoc -f markdown+raw_tex -t native
hi

\hspace{1em}

there
^D
[Para [Str "hi"]
,RawBlock (Format "tex") "\\hspace{1em}"
,Para [Str "there"]]
```
