$if(titleblock)$
$titleblock$

$for(author)$
:Author: $^$$author$
$endfor$
$if(authors)$
:Authors:
   $author$
$endif$
$if(date)$
:Date: $^$$date$
$endif$
$if(address)$
:Addresss: $^$$address$
$endif$
$if(contact)$
:Contact: $^$$contact$
$endif$
$if(copyright)$
:Copyright: $^$$copyright$
$endif$
$if(dedication)$
:Dedication: $^$$dedication$
$endif$
$if(organization)$
:Organization: $^$$organization$
$endif$
$if(revision)$
:Revision: $^$$revision$
$endif$
$if(status)$
:Status: $^$$status$
$endif$
$if(version)$
:Version: $^$$version$
$endif$
$if(abstract)$
:Abstract:
   $abstract$
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
