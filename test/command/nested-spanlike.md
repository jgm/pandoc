```
% pandoc -f markdown -t html
[test]{.foo .underline #bar .smallcaps .kbd}
^D
<p><u id="bar"><span class="smallcaps"><kbd>test</kbd></span></u></p>
```
