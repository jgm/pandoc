```
% pandoc -f latex -t plain
\iftrue
should print
\iftrue
should print
\else
should not print A
\fi
\else
should not print B
\fi

\iffalse
should not print C
\else
\iftrue
should print
\else
should not print D
\fi
\fi

\newif\ifepub

\ifepub
should not print E
\fi

\epubtrue

\ifepub
should print
\else
should not print F
\fi

\epubfalse

\ifepub
should not print G
\else
should print
\fi
^D
should print

should print

should print

should print

should print
```
