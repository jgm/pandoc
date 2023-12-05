```
% pandoc -f rst -t gfm
.. note::
   This is my note.
^D
> [!NOTE]
> This is my note.
```

```
% pandoc -f gfm -t rst
> [!WARNING]
> Be careful!
^D
.. warning::

   Be careful!
```

```
% pandoc -f gfm -t asciidoc
> [!TIP]
> A tip.
^D
[TIP]
====
A tip.
====
```
