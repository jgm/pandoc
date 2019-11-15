```
% pandoc --lua-filter=command/lua-pandoc-state.lua --data-dir=foo
Hello
^D
   # input files: 1
     output file: nil
# request header: 0
   resource path: .
      source URL: nil
   user data dir: defined
           trace: false
       verbosity: WARNING
<p>Hello</p>
```
