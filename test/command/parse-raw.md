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
*Hi there*
```
