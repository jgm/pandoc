converted from: <https://github.com/bodiam/markdown-to-asciidoc/tree/9bd90bc405a7d25b03822cc91154bcb315ab39bf/src/test/resources/nl/jworks/markdown_to_asciidoc/tables.feature>

Render a table

```
% pandoc -t asciidoc
| Name of Column 1 | Name of Column 2|
| ---------------- | --------------- |
| Cell in column 1, row 1 | Cell in column 2, row 1|
| Cell in column 1, row 2 | Cell in column 2, row 2|
^D
|===
|Name of Column 1 |Name of Column 2

|Cell in column 1, row 1 |Cell in column 2, row 1
|Cell in column 1, row 2 |Cell in column 2, row 2
|===
```


Leave a trailing space at the end of each adjacent cell

```
% pandoc -t asciidoc
| Browser | Tablet |  Smartphone |
| ------- | ------ | ---------- |
| Safari 5.1+| iPad 2+ |  iOS 6+ |
^D
|===
|Browser |Tablet |Smartphone

|Safari 5.1+ |iPad 2+ |iOS 6+
|===
```


Render a table with left, center and right align columns

```
% pandoc -t asciidoc
| Tables        | Are           |  Cool|
| ------------- |:-------------:| ----:|
| col 3 is      | right-aligned | $1600|
| col 2 is      | centered      |   $12|
| zebra stripes | are neat      |    $1|
^D
[cols="<,^,>"]
|===
|Tables |Are |Cool

|col 3 is |right-aligned |$1600
|col 2 is |centered |$12
|zebra stripes |are neat |$1
|===
```


Render a markdown HTML table

```
% pandoc -t asciidoc
Care must be taken with slashes when specifying both the base URL and the relative URL as trailing and leading slashes have significant meaning. The following table illustrates the resolution of different types of URLs.

<table class="graybox" border="0" cellspacing="0" cellpadding="5">
    <tr><th>Base</th><th>Navigating To</th><th>Result</th></tr>
    <tr><td>http://myapp.com/</td><td>abc</td><td>http://myapp.com/abc</td></tr>
    <tr><td>http://myapp.com</td><td>abc</td><td>http://myapp.comabc</td></tr>
</table>

It is usually most desirable to define your base urls with trailing slashes and not to use leading slashes on relative URLs.
^D
Care must be taken with slashes when specifying both the base URL and the relative URL as trailing and leading slashes have significant meaning. The following table illustrates the resolution of different types of URLs.

|===
|Base |Navigating To |Result

|http://myapp.com/ |abc |http://myapp.com/abc
|http://myapp.com |abc |http://myapp.comabc
|===

It is usually most desirable to define your base urls with trailing slashes and not to use leading slashes on relative URLs.
```


Render a table with left, center and right align columns

```
% pandoc -t asciidoc
|              | Grouping                    ||
| First Header | Second Header | Third Header |
| ------------ | :-----------: | -----------: |
| Content      | *Long Cell*                 ||
| Content      | **Cell**      | Cell         |
| New section  | More          | Data         |
^D
[cols="<,^,>"]
|===
|  2+| Grouping

| Content 2+| _Long Cell_
| Content   | *Cell* | Cell
| New section | More | Data
|===
```


