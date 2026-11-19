Package and class messages produce nothing, in block and inline contexts.

```
% pandoc -f latex -t native
\PackageError{pkg}{Something went wrong}
  {Try again.}
Text.
^D
[ Para [ Str "Text." ] ]
```

```
% pandoc -f latex -t native
\textbf{a \PackageWarning{pkg}{A warning} b}
^D
[ Para [ Strong [ Str "a" , Space , Str "b" ] ] ]
```

```
% pandoc -f latex+raw_tex -t native
First.

\ClassWarning{cls}{A warning}

\PackageInfo{pkg}{Some info}
Second.
^D
[ Para [ Str "First." ] , Para [ Str "Second." ] ]
```
