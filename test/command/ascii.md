```
pandoc -t html --ascii
äéıå
^D
<p>&auml;&eacute;&imath;&aring;</p>
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
\[:a]\['e]\[.i]\[oa]
```

```
pandoc -t ms --ascii
äéıå
^D
.LP
\[:a]\['e]\[.i]\[oa]
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

```
pandoc -t markdown-smart --ascii
"äéıå"
^D
&ldquo;&auml;&eacute;&imath;&aring;&rdquo;
```

