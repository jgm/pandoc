```
% pandoc -f latex -t native
\global\hyphenpenalty=10000
\global\def\hello{Hello}
\hello{} world.
^D
[ Para [ Str "Hello" , Space , Str "world." ] ]
```
