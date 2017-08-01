```
% pandoc -f latex -t native
\SI[round-precision=2]{1}{m} is equal to \SI{1000}{mm}
^D
[Para [Str "1\160m",Space,Str "is",Space,Str "equal",Space,Str "to",Space,Str "1000\160mm"]]
```

```
% pandoc -f latex -t native
\SI[round-precision=2]{1}[\$]{} is equal to \SI{0.938094}{\euro}
^D
[Para [Str "$\160\&1",Space,Str "is",Space,Str "equal",Space,Str "to",Space,Str "0.938094\160\8364"]]
```


```
% pandoc -f latex -t native
\SI[round-precision=2]{\{\}}[\{\}]{\{\}}
^D
[Para [Str "{}\160{}\160{}"]]
```

```
% pandoc -f latex -t native
\num{123}     \\
\num{1234}    \\
\num{12345}   \\
\num{0.123}   \\
\num{0,1234}  \\
\num{.12345}  \\
\num{3.45d-4} \\
\num{-e10}
^D
[Para [Str "123",LineBreak,Str "1234",LineBreak,Str "12345",LineBreak,Str "0.123",LineBreak,Str "0.1234",LineBreak,Str "0.12345",LineBreak,Str "3.45 × 10⁻⁴",LineBreak,Str "−10¹⁰"]]
```
