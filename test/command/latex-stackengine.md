# stackengine

```
% pandoc -f latex -t native
\addstackgap{Hello} World

^D
[Para [Str "Hello",Space,Str "World"]]
```

```
% pandoc -f latex -t native
Hello \addstackgap[12pt]{World}

^D
[Para [Str "Hello",Space,Str "World"]]
```

