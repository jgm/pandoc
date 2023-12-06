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

```
% pandoc -f gfm -t docbook
> [!TIP]
> A tip.
^D
<tip>
  <title>Tip</title>
  <para>
    A tip.
  </para>
</tip>
```

```
% pandoc -f docbook -t gfm
<tip>
  <title>Tip</title>
  <para>
    A tip.
  </para>
</tip>
^D
> [!TIP]
> A tip.
```
