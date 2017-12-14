```
% pandoc -f rst
`*ab*`_

.. _`*ab*`: foo
^D
<p><a href="foo">*ab*</a></p>
```

```
% pandoc -f rst
`A B
c`_

.. _A B C: foo
^D
<p><a href="foo">A B c</a></p>
```
