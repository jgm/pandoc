```
% pandoc -f latex -t native
\def
\first{one}
\gdef
  \second{two}
\edef
\third{three}
\first{} \second{} \third
^D
[ Para
    [ Str "one" , Space , Str "two" , Space , Str "three" ]
]
```
