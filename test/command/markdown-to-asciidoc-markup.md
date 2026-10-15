converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/markup.feature>

Text, lists, text

```
% pandoc -t asciidoc
The support provides:

* Understanding of implicit browser methods (e.g. `to()`, `at()`) in test classes (e.g. `extends GebSpec`)
* Understanding of content defined via the Content DSL (within `Page` and `Module` classes only)
* Completion in `at {}` and `content {}` blocks

This effectively enables more authoring support with less explicit type information. The Geb development team would like to thank the good folks at JetBrains for adding this explicit support for Geb to IDEA.
^D
The support provides:

* Understanding of implicit browser methods (e.g. `+to()+`, `+at()+`) in
test classes (e.g. `+extends GebSpec+`)
* Understanding of content defined via the Content DSL (within `+Page+`
and `+Module+` classes only)
* Completion in `+at {}+` and `+content {}+` blocks

This effectively enables more authoring support with less explicit type
information. The Geb development team would like to thank the good folks
at JetBrains for adding this explicit support for Geb to IDEA.
```


Escaped characters

```
% pandoc -t asciidoc
\*this text is surrounded by literal asterisks\*
^D
+++*this text is surrounded by literal asterisks*+++
```


Make text bold

```
% pandoc -t asciidoc
**Bold text**
^D
*Bold text*
```


Make text italic

```
% pandoc -t asciidoc
*Italic text*
^D
_Italic text_
```


Make text mono

```
% pandoc -t asciidoc
`Mono text`
^D
`+Mono text+`
```


Make text bold and italic

```
% pandoc -t asciidoc
This is ***bold and italic*** text
^D
This is *_bold and italic_* text
```


Blockquotes

```
% pandoc -f markdown-smart -t asciidoc
> Blockquotes are very handy in email to emulate reply text.
> This line is part of the same quote.

Quote break.

> This is a very long line that will still be quoted properly when it wraps. Oh boy let's keep writing to make sure this is long enough to actually wrap for everyone. Oh, you can *put* **Markdown** into a blockquote.
^D
____
Blockquotes are very handy in email to emulate reply text. This line is
part of the same quote.
____

Quote break.

____
This is a very long line that will still be quoted properly when it
wraps. Oh boy let's keep writing to make sure this is long enough to
actually wrap for everyone. Oh, you can _put_ *Markdown* into a
blockquote.
____
```


Nested Blockquotes

```
% pandoc -f markdown-smart -t asciidoc
> > What's new?
>
> I've got Markdown in my AsciiDoc!
>
> > Like what?
>
> * Blockquotes
> * Headings
> * Fenced code blocks
>
> > Is there more?
>
> Yep. AsciiDoc and Markdown share a lot of common syntax already.
^D
____

________

What's new?

________

I've got Markdown in my AsciiDoc!

________

Like what?

________

* Blockquotes
* Headings
* Fenced code blocks

________

Is there more?

________

Yep. AsciiDoc and Markdown share a lot of common syntax already.

____
```


Superscript

```
% pandoc -t asciidoc
superscript^2^
^D
superscript^2^
```


Subscript

```
% pandoc -t asciidoc
CO~2~
^D
CO~2~
```



Double quoting

```
% pandoc -f markdown-smart -t asciidoc
"hello"
^D
"hello"
```


Single quoting

```
% pandoc -f markdown-smart -t asciidoc
'hello'
^D
'hello'
```


Apostroph

```
% pandoc -f markdown-smart -t asciidoc
a'a
^D
a'a
```


Ellipsis two

```
% pandoc -t asciidoc
a...a
^D
a…a
```


Em Dash

```
% pandoc -t asciidoc
a---a
^D
a—a
```


En Dash

```
% pandoc -t asciidoc
a--a
^D
a–a
```


Nbsp

```
% pandoc -t asciidoc
<< a a >>
^D
«{nbsp}a a{nbsp}»
```


Should recognize a hard line break

```
% pandoc -t asciidoc
Roses are red,\
Violets are blue.
^D
Roses are red, +
Violets are blue.
```


Strikethrough

```
% pandoc -t asciidoc
This is ~~striked~~ text
^D
This is [line-through]#striked# text
```


