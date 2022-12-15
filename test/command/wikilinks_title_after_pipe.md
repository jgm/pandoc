# CommonMark

## Reader
```
% pandoc --from commonmark_x+wikilinks_title_after_pipe -t html --columns 90
[[https://example.org]]

[[https://example.org|title]]

[[name of page]]

[[name of page|title]]
^D
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="name of page" title="wikilink">name of page</a></p>
<p><a href="name of page" title="wikilink">title</a></p>
```

## Writer

```
% pandoc -t commonmark_x+wikilinks_title_after_pipe -f html
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="Home" title="wikilink">Home</a></p>
<p><a href="Name of page" title="wikilink">Title</a></p>
^D
[[https://example.org]]

[[https://example.org|title]]

[[Home]]

[[Name%20of%20page|Title]]
```

# Markdown
## Reader

```
% pandoc --from markdown+wikilinks_title_after_pipe -t html --columns 90
[[https://example.org]]

[[https://example.org|title]]

[[name of page]]

[[name of page|title]]
^D
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="name of page" title="wikilink">name of page</a></p>
<p><a href="name of page" title="wikilink">title</a></p>
```

## Writer

```
% pandoc -t markdown+wikilinks_title_after_pipe -f html
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="Home" title="wikilink">Home</a></p>
<p><a href="Name of page" title="wikilink">Title</a></p>
^D
[[https://example.org]]

[[https://example.org|title]]

[[Home]]

[[Name%20of%20page|Title]]
```
