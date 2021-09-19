```
% pandoc -f latex -t native -M lang=en
\figurename\ 2
^D
[ Para [ Str "Figure\160\&2" ] ]
```

```
% pandoc -f latex -t native -M lang=de-DE
\figurename\ 2
^D
[ Para [ Str "Abbildung\160\&2" ] ]
```

```
% pandoc -f latex -t native -M lang=en
\setmainlanguage{german}
\figurename 2
^D
[ Para [ Str "Abbildung2" ] ]
```

```
% pandoc -f latex -t native -M lang=sr
\figurename~2
\figurename.
^D
[ Para [ Str "Slika\160\&2" , SoftBreak , Str "Slika." ] ]
```
