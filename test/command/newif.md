```
% pandoc -f latex -t plain
\iftrue
should print
\iftrue
should print
\else
should not print
\fi
\else
should not print
\fi

\iffalse
should not print
\else
\iftrue
should print
\else
should not print
\fi
\fi

\newif\ifepub

\ifepub
should not print
\fi

\epubtrue

\ifepub
should print
\else
should not print
\fi

\epubfalse

\ifepub
should not print
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
