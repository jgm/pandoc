algpseudocode's \algnewcommand and \algrenewcommand define macros.

```
% pandoc -f latex -t native
\algrenewcommand{\algorithmiccomment}[1]{\hspace{0.75em}#1}
\algnewcommand{\Foo}{foo}
Text \Foo.
^D
[ Para [ Str "Text" , Space , Str "foo." ] ]
```
