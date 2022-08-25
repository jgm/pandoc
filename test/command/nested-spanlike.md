```
% pandoc -f markdown -t html
[test]{.foo .underline #bar .smallcaps .kbd}
^D
<p><kbd id="bar"><u><span class="smallcaps">test</span></u></kbd></p>
```
