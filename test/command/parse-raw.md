```
% pandoc -f latex+raw_tex -t markdown
\emph{Hi \foo{there}}
^D
*Hi \foo{there}*
```

```
% pandoc -f latex -t markdown
\emph{Hi \foo{there}}
^D
[warning] Skipped '\foo{there}' at line 1 column 21
*Hi*
```

```
% pandoc -f html+raw_html -t markdown
<em>Hi <blink>there</blink></em>
^D
*Hi <blink>there</blink>*
```

```
% pandoc -f html -t markdown
<em>Hi <blink>there</blink></em>
^D
[warning] Skipped '<blink>' at input line 1 column 8
[warning] Skipped '</blink>' at input line 1 column 20
*Hi there*
```
