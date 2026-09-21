#set page(width: 10cm, height: auto)
#set heading(numbering: "1.")

= Fibonacci sequence
The Fibonacci sequence is defined through the
recurrence relation $F_n = F_(n-1) + F_(n-2)$.
It can also be expressed in _closed form:_

$ F_n = round(1 / sqrt(5) phi.alt^n), quad
  phi.alt = (1 + sqrt(5)) / 2 $

#let count = 8
#let nums = range(1, count + 1)
#let fib(n) = (
  if n <= 2 { 1 }
  else { fib(n - 1) + fib(n - 2) }
)

The first #count numbers of the sequence are:

#align(center, table(
  columns: count,
  ..nums.map(n => $F_#n$),
  ..nums.map(n => str(fib(n))),
))

#include "undergradmath.typ"


= Citations

Normal: #cite(<brown01>) or @brown01.

Prose: #cite(<brown01>, form: "prose")

Year: #cite(<brown01>, form: "year")

Author: #cite(<brown01>, form: "author")

= Blocks

#block[An unfilled block stays transparent.]

#block(fill: none)[An explicit none fill stays transparent.]

#block(fill: yellow)[
  A filled block keeps its boundary, with the fill recorded as a
  background-color attribute.
]

#block(fill: rgb("#ffdddd"), radius: 3pt)[
  The color is preserved; other parameters are dropped.
]

#block(fill: luma(31%))[
  A grayscale fill converts to sRGB hex.
]

= Boxes

An #box[unfilled box] keeps its boundary with no attribute.

An #box(fill: yellow)[filled box] records the fill as a background-color
attribute.

An #box(fill: rgb("#ffdddd"), radius: 2pt)[colored box] preserves the
color; other parameters are dropped.

An #box(fill: rgb(255, 200, 100, 50%))[translucent box] appends
an alpha byte.
