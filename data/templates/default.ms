.\" **** Custom macro definitions *********************************
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
.\" **** Settings *************************************************
.\" text width
.nr LL 5.5i
.\" left margin
.nr PO 1.25i
.\" top margin
.nr HM 1.25in
.\" bottom margin
.nr FM 1.25in
.\" header/footer width
.nr LT \n[LL]
.\" point size (pt x 1000)
.nr PS 10000
.\" line height (pt x 1000)
.nr VS 12000
.\" font family: A, BM, H, HN, N, P, T, ZCM
.fam T
.\" paragraph indent
.nr PI 2m
.\" interparagraph space
.nr PD 0.33v
.\" footnote width
.nr FL \n[LL]
.\" footnote point size
.nr FPS (\n[PS] - 2000)
.\" color used for strikeout
.defcolor strikecolor rgb 0.7 0.7 0.7
.\" color for links (rgb)
.ds PDFHREF.COLOUR   0.35 0.00 0.60
.\" border for links (default none)
.ds PDFHREF.BORDER   0 0 0
.\" ***************************************************************
.\" PDF metadata
.pdfinfo /Title "$title-meta$"
.pdfinfo /Author "$author-meta$"
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
.\" 1 column (use .2C for two column)
.1C
$body$
$if(toc)$
$toc$
.TC
$endif$
$for(include-after)$
$include-after$
$endfor$
.pdfsync
