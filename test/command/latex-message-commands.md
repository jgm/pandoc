Package and class messages produce nothing, also in inline contexts.

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
