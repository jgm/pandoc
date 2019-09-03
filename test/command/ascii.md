```
pandoc -t html --ascii
äéıå
^D
<p>&#xE4;&#xE9;&#x131;&#xE5;</p>
```

```
pandoc -t latex --ascii
äéıå
^D
\"{a}\'{e}\i\r{a}
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
  &#xE4;&#xE9;&#x131;&#xE5;
</para>
```

```
pandoc -t jats --ascii
äéıå
^D
<p>&#xE4;&#xE9;&#x131;&#xE5;</p>
```

```
pandoc -t markdown-smart --ascii
"äéıå"
^D
&ldquo;&auml;&eacute;&imath;&aring;&rdquo;
```

# CommonMark tests

```
% pandoc -f commonmark -t commonmark --ascii
hello … ok? … bye
^D
hello &mldr; ok? &mldr; bye
```

```
% pandoc -f commonmark+smart -t commonmark-smart --ascii --wrap=none
"hi"...dog's breath---cat 5--6
^D
&ldquo;hi&rdquo;&mldr;dog&rsquo;s breath&mdash;cat 5&ndash;6
```

```
% pandoc -f commonmark+smart -t commonmark+smart --ascii
"hi"...dog's breath---cat 5--6
^D
"hi"...dog's breath---cat 5--6
```

```
% pandoc -f commonmark -t commonmark --ascii
foo &#1234; bar
^D
foo &#1234; bar
```

```
% pandoc -f commonmark -t commonmark --ascii
\[foo\](bar)
^D
\[foo\](bar)
```
