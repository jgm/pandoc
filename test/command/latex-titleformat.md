titlesec's \titleformat, in its starred form and with its optional last
argument.

```
% pandoc -f latex -t native
\titleformat{\section}
  {\normalfont\Large\bfseries}
  {\thesection}{0.6em}{}
\titleformat*{\subsection}{\large}
\titleformat{\chapter}[display]
{\normalfont\large}{Appendix \thechapter:}{1em}{}[\vspace{1ex}]
\section{Intro}
Text.
^D
[ Header 1 ( "intro" , [] , [] ) [ Str "Intro" ]
, Para [ Str "Text." ]
]
```
