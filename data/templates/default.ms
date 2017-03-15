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
