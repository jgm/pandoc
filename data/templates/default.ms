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
\" From https://lists.gnu.org/archive/html/groff/2012-07/msg00046.html
\" Superscripts (ex tmac.e) (current versions without extra line space)
.\" (reinstate commented versions to restore extra line space)
.\" .ds { \v'-0.3m'\x'-0.2m'\\s[\\n[.s]*8u/10u]
.ds { \v'-0.3m'\\s[\\n[.s]*9u/12u]
.ds } \s0\v'0.3m'
.\" Subscripts
.\" .ds < \v'0.3m'\x'0.2m'\s[\\n[.s]*8u/10u]
.ds < \v'0.3m'\s[\\n[.s]*9u/12u]
.ds > \s0\v'-0.3m'
$if(title)$
.TL
$title$
$endif$
$for(author)$
.AU
$author$
$endfor$
$for(header-includes)$
$header-includes$
$endfor$
$for(include-before)$
$include-before$
$endfor$
$body$
$for(include-after)$
$include-after$
$endfor$
