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


