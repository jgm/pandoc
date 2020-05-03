```
pandoc -t beamer
:::: { .columns }                                
::: { .column align=center }
:::
::: { .column align=bottom }
:::
::::

:::: { .columns align=bottom .onlytextwidth }
::: { .column align=top }
:::
::: { .column align=top-baseline }
:::
::::

:::: { .columns totalwidth=7em } 
::::
^D
\begin{frame}
\begin{columns}[T]
\begin{column}[c]{0.48\textwidth}
\end{column}

\begin{column}[b]{0.48\textwidth}
\end{column}
\end{columns}

\begin{columns}[b,onlytextwidth]
\begin{column}[T]{0.48\textwidth}
\end{column}

\begin{column}[t]{0.48\textwidth}
\end{column}
\end{columns}

\begin{columns}[T,totalwidth=7em]
\end{columns}
\end{frame}
```
