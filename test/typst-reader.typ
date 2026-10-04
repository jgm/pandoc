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


= Inline elements across paragraphs

#emph[hi

there]

Hello #strong[again

and again] world.

#underline[Para one.

Para two.]

#smallcaps[#heading(level: 2)[smallcaps heading]]


= More inline elements across paragraphs

#link("https://example.com/typst")[link one

link two]

#emph[hi #block[struck middle] there]

#lower[Mixed Case

Also Here]

/ term: #emph[term one

  term two]

Edge case #emph[

split here] continues.


= Splitting edge cases <edge-label>

Trailing edge #emph[splits

here] continues.

Consecutive breaks #emph[a


b] end.

Vanishing #emph[

] body.

#emph[#underline[nested one

nested two]]

#emph[a #grid(columns: 1)[cell] c]

Ref supplement: @edge-label[see this section].
