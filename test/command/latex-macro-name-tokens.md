Tokens after a macro name in braces are read as LaTeX reads them: as
delimiters for \DeclareRobustCommand and \DeclareMathOperator, and [n]
as the number of arguments for \newcommand.

```
% pandoc -f latex -t native
\DeclareMathOperator{\Pol2}{Pol_2}
\DeclareMathOperator{\_div}{div}
\renewcommand{\div}{\_div}
$\Pol2(x) + \div v$
^D
[ Para
    [ Math
        InlineMath
        "\\mathop{\\mathrm{Pol_2}}(x) + \\mathop{\\mathrm{div}}v"
    ]
]
```

```
% pandoc -f latex -t native
\newcommand{\msp[1]}[1]{\mspace{#1mu}}
$a\msp[3]b\msp c$
^D
[ Para [ Math InlineMath "a\\mspace{3mu}b\\mspace{1mu}c" ] ]
```
