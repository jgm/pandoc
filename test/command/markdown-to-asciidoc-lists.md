converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/lists.feature>

Render an unordered list

```
% pandoc -t asciidoc
* Item 1
* Item 2
* Item 3
^D
* Item 1
* Item 2
* Item 3
```


Render an unordered nested list

```
% pandoc -t asciidoc
* Item 1
    * Item 1_1
    * Item 1_2
^D
* Item 1
** Item 1_1
** Item 1_2
```


Render an ordered list

```
% pandoc -t asciidoc
1. Item 1
1. Item 2
1. Item 3
^D
. Item 1
. Item 2
. Item 3
```


Render a nested ordered list

```
% pandoc -t asciidoc
1. Item 1
    1. Item 1.1
    1. Item 1.2
^D
. Item 1
.. Item 1.1
.. Item 1.2
```


Render a nested ordered list combined with unordered list

```
% pandoc -t asciidoc
1. Item 1
    1. Item 11
        * bullet 111
        * bullet 112
            * bullet 1121
                1. Item 11211
    1. Item 12
1. Item 2
^D
. Item 1
.. Item 11
*** bullet 111
*** bullet 112
**** bullet 1121
..... Item 11211
.. Item 12
. Item 2
```


Render an ordered list combined with a nested unordered list

```
% pandoc -t asciidoc
1. Item 1

2. Item 2

    * Subitem of Item 2

3. Item 3
^D
. Item 1
. Item 2
** Subitem of Item 2
. Item 3
```


Render an ordered list of paragraphs

```
% pandoc -t asciidoc
. Paragraph 1

. Paragraph 2
^D
. Paragraph 1

. Paragraph 2
```


Render an unordered list with a link

```
% pandoc -t asciidoc
There is a Maven example project available.

* [http://github.com/geb/geb-example-maven](https://github.com/geb/geb-example-maven)
^D
There is a Maven example project available.

* https://github.com/geb/geb-example-maven[http://github.com/geb/geb-example-maven]
```


Render 4 numbered items with a code block

```
% pandoc -f markdown-smart -t asciidoc
1. Use the `browser` object explicitly (made available by the testing adapters)
2. Use the page instance returned by the `to()` and `at()` methods instead of calling through the browser
3. Use methods on the `Page` classes instead of the `content {}` block and dynamic properties
4. If you need to use content definition options like `required:` and `wait:` then you can still reference content elements defined using the DSL in methods on `Page` and `Module` classes as usual, e.g.:

       static content = {
           async(wait: true) { $(".async") }
       }

       String asyncText() {
           async.text() // Wait here for the async definition to return a non-empty Navigator...
       }
Using this “typed” style is not an all or nothing proposition.
^D
. Use the `+browser+` object explicitly (made available by the testing
adapters)
. Use the page instance returned by the `+to()+` and `+at()+` methods
instead of calling through the browser
. Use methods on the `+Page+` classes instead of the `+content {}+`
block and dynamic properties
. If you need to use content definition options like `+required:+` and
`+wait:+` then you can still reference content elements defined using
the DSL in methods on `+Page+` and `+Module+` classes as usual, e.g.:
+
....
static content = {
    async(wait: true) { $(".async") }
}

String asyncText() {
    async.text() // Wait here for the async definition to return a non-empty Navigator...
}
....
+
Using this “typed” style is not an all or nothing proposition.
```


