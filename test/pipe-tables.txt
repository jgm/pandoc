Simplest table without caption:

| Default1 | Default2 | Default3 | 
 |----------|----------|----------|
|12|12|12|
|123|123|123|
|1|1|1|

Simple table with caption:

| Right | Left | Default | Center |
| ----: | :--- | ------- | :----: |
|   12  |  12  |    12   |    12  |
|  123  |  123 |   123   |   123  |
|    1  |    1 |     1   |     1  |

  : Demonstration of simple table syntax.

Simple table without caption:

| Right | Left | Center | 
|------:|:-----|:------:|
|12|12|12|
|123|123|123|
|1|1|1|


Headerless table without caption:

|       |      |        |
|------:|:-----|:------:|
|12|12|12|
|123|123|123|
|1|1|1|

Table without sides:

Fruit |Quantity
------|-------:
apple |    5
orange|   17
pear  |  302

One-column:

|hi|
|--|
|lo|

Header-less one-column:

|   |
|:-:|
|hi|

Indented left column:

Number of siblings | Salary
------------------:|:------
                 3 | 33
                 4 | 44

Long pipe table with relative widths:

| Default1 | Default2 | Default3 |
 |---------|----------|---------------------------------------|
|123|this is a table cell|and this is a really long table cell that will probably need wrapping|
|123|123|123|

Pipe table with no body:

| Header |
| ------ |

Pipe table with tricky cell contents (see #2765):

|               | IP_gene8-_1st| IP_gene8+_1st|
|:--------------|-------------:|-------------:|
|IP_gene8-_1st  |     1.0000000|     0.4357325|
|IP_gene8+_1st  |     0.4357325|     1.0000000|
|foo`bar|baz`   | and\|escaped |     3.0000000|

