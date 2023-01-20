tests adapted from <https://github.github.com/gfm/#task-list-items-extension->

```
% pandoc
- [ ] foo
- [x] bar
^D
<ul class="task-list">
<li><input type="checkbox" />foo</li>
<li><input type="checkbox" checked="" />bar</li>
</ul>
```


```
% pandoc
- [x] foo
  - [ ] bar
  - [x] baz
- [ ] bim
^D
<ul class="task-list">
<li><input type="checkbox" checked="" />foo<ul class="task-list">
<li><input type="checkbox" />bar</li>
<li><input type="checkbox" checked="" />baz</li>
</ul></li>
<li><input type="checkbox" />bim</li>
</ul>
```


custom html task list test:

```
% pandoc
- [ ]  unchecked
- plain item
-  [x] checked

paragraph

1. [ ] ordered unchecked
2. [] plain item
3. [x] ordered checked

paragraph

- [ ] list item with a

    second paragraph

- [x] checked
^D
<ul>
<li><input type="checkbox" />unchecked</li>
<li>plain item</li>
<li><input type="checkbox" checked="" />checked</li>
</ul>
<p>paragraph</p>
<ol type="1">
<li><input type="checkbox" />ordered unchecked</li>
<li>[] plain item</li>
<li><input type="checkbox" checked="" />ordered checked</li>
</ol>
<p>paragraph</p>
<ul class="task-list">
<li><p><input type="checkbox" />list item with a</p><p>second
paragraph</p></li>
<li><p><input type="checkbox" checked="" />checked</p></li>
</ul>
```

latex task list test:

```
% pandoc -t latex
- [ ] foo bar

  baz

- [x] ok
^D
\begin{itemize}
\item[$\square$]
  foo bar

  baz
\item[$\boxtimes$]
  ok
\end{itemize}
```

round trip:

```
% pandoc -f markdown -t markdown
- [ ] foo
- [x] bar
^D
-   [ ] foo
-   [x] bar
```
