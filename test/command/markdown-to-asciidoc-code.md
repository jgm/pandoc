converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/code.feature>

Render a code block without language

```
% pandoc -t asciidoc
```
summary(cars$dist)
summary(cars$speed)
```
^D
----
summary(cars$dist)
summary(cars$speed)
----
```


Render a code block without language containing HTML

```
% pandoc -t asciidoc
```
No language indicated, so no syntax highlighting.
But let's throw in a <b>tag</b>.
```
^D
----
No language indicated, so no syntax highlighting.
But let's throw in a <b>tag</b>.
----
```


Render a javascript code block

```
% pandoc -t asciidoc
```javascript
var s = "JavaScript syntax highlighting";
alert(s);
```
^D
[source,javascript]
----
var s = "JavaScript syntax highlighting";
alert(s);
----
```


Render a python code block

```
% pandoc -t asciidoc
```python
s = "Python syntax highlighting"
print s
```
^D
[source,python]
----
s = "Python syntax highlighting"
print s
----
```


Render an indented code block

```
% pandoc -t asciidoc
    $ gem install asciidoctor
^D
----
$ gem install asciidoctor
----
```


Render code block adjacent to preceding paragraph

```
% pandoc -t asciidoc
Here's an example:
```javascript
var s = "JavaScript syntax highlighting";
alert(s);
```
^D
Here's an example:

[source,javascript]
----
var s = "JavaScript syntax highlighting";
alert(s);
----
```


Render inline code

```
% pandoc -t asciidoc
We defined the `add` function to
^D
We defined the `add` function to
```


