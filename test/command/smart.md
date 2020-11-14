```
% pandoc -f markdown+smart -t markdown-smart --markdown-headings=setext
"hi"...dog's breath---cat 5--6
^D
“hi”…dog’s breath—cat 5–6
```

```
% pandoc -f markdown+smart -t markdown+smart --markdown-headings=setext
"hi"...dog's breath---cat 5--6
^D
"hi"...dog's breath---cat 5--6
```

When we render literal quotes without smart, we need to escape:

```
% pandoc -f markdown-smart \
  -t markdown+smart --markdown-headings=setext
"hi"...dog's breath---cat 5--6
^D
\"hi\"\...dog\'s breath\-\--cat 5\--6
```

```
% pandoc -f markdown+smart -t rst-smart
"hi"...dog's breath---cat 5--6
^D
“hi”…dog’s breath—cat 5–6
```

```
% pandoc -f markdown+smart -t rst+smart
"hi"...dog's breath---cat 5--6
^D
"hi"...dog's breath---cat 5--6
```

```
% pandoc -f markdown-smart -t rst+smart
"hi"...dog's breath---cat 5--6
^D
\"hi\"\...dog\'s breath\-\--cat 5\--6
```

