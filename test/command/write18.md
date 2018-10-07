Handle \write18{..} as raw tex:
```
% pandoc -t native
\write18{git --version}
^D
[RawBlock (Format "tex") "\\write18{git --version}"]
```

```
% pandoc -f latex+raw_tex -t native
\write18{git --version}
^D
[RawBlock (Format "latex") "\\write18{git --version}"]
```
