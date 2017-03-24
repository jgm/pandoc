.mso www.tmac
.\" **** Custom macro definitions ****
.\" * Super/subscript
.\" (https://lists.gnu.org/archive/html/groff/2012-07/msg00046.html)
.ds { \v'-0.3m'\\s[\\n[.s]*9u/12u]
.ds } \s0\v'0.3m'
.ds < \v'0.3m'\s[\\n[.s]*9u/12u]
.ds > \s0\v'-0.3m'
.\" * Horizontal line
.de HLINE
.LP
.ce
\l'20'
..
.\" **********************************
$if(adjusting)$
.ad $adjusting$
$endif$
$if(hyphenate)$
.hy
$else$
.nh \" Turn off hyphenation by default.
$endif$
$if(has-inline-math)$
.EQ
delim ||
.EN
$endif$
$if(title)$
.TL
$title$
$endif$
$for(author)$
.AU
$author$
$endfor$
$if(date)$
.ND "$date$"
$endif$
$if(abstract)$
.AB
$abstract$
.AE
$endif$
$for(header-includes)$
$header-includes$
$endfor$
$for(include-before)$
$include-before$
$endfor$
$body$
$if(toc)$
$toc$
.TC
$endif$
$for(include-after)$
$include-after$
$endfor$
