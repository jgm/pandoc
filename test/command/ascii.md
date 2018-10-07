```
pandoc -t html --ascii
äéıå
^D
<p>&#228;&#233;&#305;&#229;</p>
```

```
pandoc -t latex --ascii
äéıå
^D
\"{a}\'{e}\i \r{a}
```

```
pandoc -t man --ascii
äéıå
^D
.PP
\[u00E4]\[u00E9]\[u0131]\[u00E5]
```

```
pandoc -t ms --ascii
äéıå
^D
.LP
\[u00E4]\[u00E9]\[u0131]\[u00E5]
```

```
pandoc -t docbook --ascii
äéıå
^D
<para>
  &#228;&#233;&#305;&#229;
</para>
```

```
pandoc -t jats --ascii
äéıå
^D
<p>&#228;&#233;&#305;&#229;</p>
```
