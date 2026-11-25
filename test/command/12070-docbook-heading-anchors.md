A section created from a Div and its first heading keeps both link targets
when their identifiers differ.

```
% pandoc -t docbook5 --wrap=none
See [wrapper](#wrap) and [heading](#sec-second).

::: {#wrap}
## Second {#sec-second}

Body.
:::
^D
<para>
  See <link linkend="wrap">wrapper</link> and <link linkend="sec-second">heading</link>.
</para>
<section xmlns="http://docbook.org/ns/docbook" xmlns:xlink="http://www.w3.org/1999/xlink" xml:id="wrap">
  <title><anchor xml:id="sec-second" />Second</title>
  <para>
    Body.
  </para>
</section>
```

The extra anchor uses DocBook 4's identifier attribute and the configured
prefix. A heading and its Div with the same identifier share one target.

```
% pandoc -t docbook4 --wrap=none --id-prefix=pub-
See [wrapper](#wrap), [heading](#sec-second), and [same ID](#shared).

::: {#wrap}
## Second {#sec-second}

Body.
:::

::: {#shared}
## Shared {#shared}

Body.
:::
^D
<para>
  See <link linkend="pub-wrap">wrapper</link>, <link linkend="pub-sec-second">heading</link>, and <link linkend="pub-shared">same ID</link>.
</para>
<sect1 id="pub-wrap">
  <title><anchor id="pub-sec-second" />Second</title>
  <para>
    Body.
  </para>
</sect1>
<sect1 id="pub-shared">
  <title>Shared</title>
  <para>
    Body.
  </para>
</sect1>
```
