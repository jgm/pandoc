# Figure with one image, caption and label

```
pandoc -f latex+native_figures -t native
\begin{document}
  \begin{figure}
    \includegraphics{../../media/rId25.jpg}
    \caption{CAP}
    \label{LAB}
  \end{figure}
\end{document}
^D
[Figure ("LAB",[],[]) (Caption Nothing [Plain [Str "CAP"]]) [Plain [Image ("",[],[]) [] ("../../media/rId25.jpg","")]]]
```

# Nested figures

```
pandoc -f latex+native_figures -t native
\begin{figure}
  \begin{subfigure}[b]{0.5\textwidth}
    \begin{subfigure}[b]{0.5\textwidth}
      \centering
      \includegraphics{test/media/rId25.jpg}
      \caption{CAP1.1}
    \end{subfigure}
    \begin{subfigure}[b]{0.5\textwidth}
      \centering
      \includegraphics{test/media/rId25.jpg}
      \caption{CAP1.2}
    \end{subfigure}
    \caption{CAP1}
    \label{fig:inner1}
  \end{subfigure}
  \begin{subfigure}[b]{0.5\textwidth}
    \includegraphics{test/media/rId25.jpg}
    \caption{CAP2}
    \label{fig:inner2}
  \end{subfigure}
  \caption{CAP}
  \label{fig:outer}
\end{figure}
^D
[Figure ("fig:outer",[],[]) (Caption Nothing [Plain [Str "CAP"]]) [Figure ("fig:inner1",[],[]) (Caption Nothing [Plain [Str "CAP1"]]) [Figure ("",[],[]) (Caption Nothing [Plain [Str "CAP1.1"]]) [Plain [Image ("",[],[]) [] ("test/media/rId25.jpg","")]],Figure ("",[],[]) (Caption Nothing [Plain [Str "CAP1.2"]]) [Plain [Image ("",[],[]) [] ("test/media/rId25.jpg","")]]],Figure ("fig:inner2",[],[]) (Caption Nothing [Plain [Str "CAP2"]]) [Plain [Image ("",[],[]) [] ("test/media/rId25.jpg","")]]]]
```
