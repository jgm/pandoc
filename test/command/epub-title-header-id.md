The identifier of the title header added before initial content
must not clash with an existing one, even inside a div.

```
% pandoc -t epub3 --metadata title=Intro -o - | pandoc -f epub -t html
See [Intro](#intro).

::: box
# Intro
:::
^D
<p><span id="title_page.xhtml"></span></p>
<p><span id="ch001.xhtml"></span></p>
<section id="ch001.xhtml_intro-1" class="unnumbered level1">
<h1 class="unnumbered">Intro</h1>
<p>See <a href="#ch002.xhtml_intro">Intro</a>.</p>
</section>
<p><span id="ch002.xhtml"></span></p>
<section id="ch002.xhtml_intro" class="level1 box">
<h1>Intro</h1>
</section>
```
