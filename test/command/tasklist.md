tests adapted from <https://github.github.com/gfm/#task-list-items-extension->

```
% pandoc
- [ ] foo
- [x] bar
^D
<ul class="task-list">
<li><label><input type="checkbox" />foo</label></li>
<li><label><input type="checkbox" checked="" />bar</label></li>
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
<li><label><input type="checkbox" checked="" />foo</label>
<ul class="task-list">
<li><label><input type="checkbox" />bar</label></li>
<li><label><input type="checkbox" checked="" />baz</label></li>
</ul></li>
<li><label><input type="checkbox" />bim</label></li>
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
<li><label><input type="checkbox" />unchecked</label></li>
<li>plain item</li>
<li><label><input type="checkbox" checked="" />checked</label></li>
</ul>
<p>paragraph</p>
<ol type="1">
<li><label><input type="checkbox" />ordered unchecked</label></li>
<li>[] plain item</li>
<li><label><input type="checkbox" checked="" />ordered
checked</label></li>
</ol>
<p>paragraph</p>
<ul class="task-list">
<li><p><label><input type="checkbox" />list item with a</label></p>
<p>second paragraph</p></li>
<li><p><label><input type="checkbox"
checked="" />checked</label></p></li>
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