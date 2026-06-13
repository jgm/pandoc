With `auto_identifiers` enabled, headings without an explicit label get
automatic identifiers, with collisions disambiguated:

```
% pandoc -f typst+auto_identifiers -t native
= Introduction

Text.

== A Sub Section

= Introduction
^D
[ Header
    1 ( "introduction" , [] , [] ) [ Str "Introduction" ]
, Para [ Str "Text." ]
, Header
    2
    ( "a-sub-section" , [] , [] )
    [ Str "A" , Space , Str "Sub" , Space , Str "Section" ]
, Header
    1 ( "introduction-1" , [] , [] ) [ Str "Introduction" ]
]
```

The extension is off by default, so headings have no identifiers:

```
% pandoc -f typst -t native
= Introduction

Text.
^D
[ Header 1 ( "" , [] , [] ) [ Str "Introduction" ]
, Para [ Str "Text." ]
]
```
