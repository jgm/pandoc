```
% pandoc -f latex+raw_tex -t markdown --markdown-headings=setext
\emph{Hi \foo{there}}
^D
*Hi `\foo{there}`{=latex}*
```

```
% pandoc -f latex -t markdown --markdown-headings=setext
\emph{Hi \foo{there}}
^D
*Hi*
```

```
% pandoc -f html+raw_html -t markdown --markdown-headings=setext
<em>Hi <blink>there</blink></em>
^D
*Hi `<blink>`{=html}there`</blink>`{=html}*
```

```
% pandoc -f html -t markdown --markdown-headings=setext
<em>Hi <blink>there</blink></em>
^D
*Hi there*
```
