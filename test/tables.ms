.LP
Simple table with caption:
.PP
Demonstration of simple table syntax.
.na
.TS
delim(@@) tab(	);
r l c l.
T{
Right
T}	T{
Left
T}	T{
Center
T}	T{
Default
T}
_
T{
12
T}	T{
12
T}	T{
12
T}	T{
12
T}
T{
123
T}	T{
123
T}	T{
123
T}	T{
123
T}
T{
1
T}	T{
1
T}	T{
1
T}	T{
1
T}
.TE
.ad
.LP
Simple table without caption:
.PP
.na
.TS
delim(@@) tab(	);
r l c l.
T{
Right
T}	T{
Left
T}	T{
Center
T}	T{
Default
T}
_
T{
12
T}	T{
12
T}	T{
12
T}	T{
12
T}
T{
123
T}	T{
123
T}	T{
123
T}	T{
123
T}
T{
1
T}	T{
1
T}	T{
1
T}	T{
1
T}
.TE
.ad
.LP
Simple table indented two spaces:
.PP
Demonstration of simple table syntax.
.na
.TS
delim(@@) tab(	);
r l c l.
T{
Right
T}	T{
Left
T}	T{
Center
T}	T{
Default
T}
_
T{
12
T}	T{
12
T}	T{
12
T}	T{
12
T}
T{
123
T}	T{
123
T}	T{
123
T}	T{
123
T}
T{
1
T}	T{
1
T}	T{
1
T}	T{
1
T}
.TE
.ad
.LP
Multiline table with caption:
.PP
Here\[cq]s the caption. It may span multiple lines.
.na
.nr LLold \n[LL]
.TS
delim(@@) tab(	);
cw(10.5n) lw(9.6n) rw(11.4n) lw(24.5n).
T{
Centered Header
T}	T{
Left Aligned
T}	T{
Right Aligned
T}	T{
Default aligned
T}
_
T{
.nr LL 10.5n
First
T}	T{
.nr LL 9.6n
row
T}	T{
.nr LL 11.4n
12.0
T}	T{
.nr LL 24.5n
Example of a row that spans multiple lines.
T}
T{
.nr LL 10.5n
Second
T}	T{
.nr LL 9.6n
row
T}	T{
.nr LL 11.4n
5.0
T}	T{
.nr LL 24.5n
Here\[cq]s another one.
Note the blank line between rows.
T}
.TE
.nr LL \n[LLold]
.ad
.LP
Multiline table without caption:
.PP
.na
.nr LLold \n[LL]
.TS
delim(@@) tab(	);
cw(10.5n) lw(9.6n) rw(11.4n) lw(24.5n).
T{
Centered Header
T}	T{
Left Aligned
T}	T{
Right Aligned
T}	T{
Default aligned
T}
_
T{
.nr LL 10.5n
First
T}	T{
.nr LL 9.6n
row
T}	T{
.nr LL 11.4n
12.0
T}	T{
.nr LL 24.5n
Example of a row that spans multiple lines.
T}
T{
.nr LL 10.5n
Second
T}	T{
.nr LL 9.6n
row
T}	T{
.nr LL 11.4n
5.0
T}	T{
.nr LL 24.5n
Here\[cq]s another one.
Note the blank line between rows.
T}
.TE
.nr LL \n[LLold]
.ad
.LP
Table without column headers:
.PP
.na
.TS
delim(@@) tab(	);
r l c r.
T{
12
T}	T{
12
T}	T{
12
T}	T{
12
T}
T{
123
T}	T{
123
T}	T{
123
T}	T{
123
T}
T{
1
T}	T{
1
T}	T{
1
T}	T{
1
T}
.TE
.ad
.LP
Multiline table without column headers:
.PP
.na
.nr LLold \n[LL]
.TS
delim(@@) tab(	);
cw(10.5n) lw(9.6n) rw(11.4n) lw(24.5n).
T{
.nr LL 10.5n
First
T}	T{
.nr LL 9.6n
row
T}	T{
.nr LL 11.4n
12.0
T}	T{
.nr LL 24.5n
Example of a row that spans multiple lines.
T}
T{
.nr LL 10.5n
Second
T}	T{
.nr LL 9.6n
row
T}	T{
.nr LL 11.4n
5.0
T}	T{
.nr LL 24.5n
Here\[cq]s another one.
Note the blank line between rows.
T}
.TE
.nr LL \n[LLold]
.ad
