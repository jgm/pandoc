# CommonMark

## Reader

```
% pandoc -f commonmark+wikilinks_title_before_pipe -t html --columns 90
[[https://example.org]]

[[title|https://example.org]]

[[Name of page]]

[[Title|Name of page]]
^D
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="Name of page" title="wikilink">Name of page</a></p>
<p><a href="Name of page" title="wikilink">Title</a></p>
```

## Writer

```
% pandoc -t commonmark_x+wikilinks_title_before_pipe -f html
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="Home" title="wikilink">Home</a></p>
<p><a href="Name of page" title="wikilink">Title</a></p>
^D
[[https://example.org]]

[[title|https://example.org]]

[[Home]]

[[Title|Name%20of%20page]]
```

## Regular links should still work

```
% pandoc -f commonmark+wikilinks_title_before_pipe -t html
[Title](Name%20of%20page)
^D
<p><a href="Name%20of%20page">Title</a></p>
```

# Markdown

## Reader

```
% pandoc -f markdown+wikilinks_title_before_pipe -t html --columns 90
[[https://example.org]]

[[title|https://example.org]]

[[Name of page]]

[[Title|Name of page]]
^D
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="Name of page" title="wikilink">Name of page</a></p>
<p><a href="Name of page" title="wikilink">Title</a></p>
```

## Writer

```
% pandoc -t markdown+wikilinks_title_before_pipe -f html
<p><a href="https://example.org" title="wikilink">https://example.org</a></p>
<p><a href="https://example.org" title="wikilink">title</a></p>
<p><a href="Home" title="wikilink">Home</a></p>
<p><a href="Name of page" title="wikilink">Title</a></p>
^D
[[https://example.org]]

[[title|https://example.org]]

[[Home]]

[[Title|Name%20of%20page]]
```
