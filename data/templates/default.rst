$if(titleblock)$
$titleblock$

$endif$
$for(author)$
:Author: $author$
$endfor$
$if(date)$
:Date:   $date$
$endif$
$if(author)$

$else$
$if(date)$

$endif$
$endif$
$if(rawtex)$
.. role:: raw-latex(raw)
   :format: latex
..

$endif$
$for(include-before)$
$include-before$

$endfor$
$if(toc)$
.. contents::
   :depth: $toc-depth$
..

$endif$
$if(number-sections)$
.. section-numbering::

$endif$
$for(header-includes)$
$header-includes$

$endfor$
$body$
$for(include-after)$

$include-after$
$endfor$
