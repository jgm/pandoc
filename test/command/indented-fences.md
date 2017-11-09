`````
% pandoc -t native
  ```haskell
  let x = y
in y
   ```
^D
[CodeBlock ("",["haskell"],[]) "let x = y\nin y"]
`````
`````
% pandoc -t native
 ~~~ {.haskell}
  let x = y
 in y +
y +
 y
~~~
^D
[CodeBlock ("",["haskell"],[]) " let x = y\nin y +\ny +\ny"]
`````
