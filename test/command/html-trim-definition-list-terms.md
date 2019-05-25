```
% pandoc -f html -t native
<dl>
    <dt>
    foo
    bar
    </dt>
    <dt>
    baz
    </dt>
    <dd>test</dd>
</dl>
^D
[DefinitionList
 [([Str "foo",SoftBreak,Str "bar",LineBreak,Str "baz"],
   [[Plain [Str "test"]]])]]
```
