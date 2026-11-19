apacite's \cite<prefix>[suffix]{key}.

```
% pandoc -f latex -t markdown
See \cite<e.g.,>[p.~3]{a,b}, \cite<see>{c} and \cite<>[available at x]{d}.
^D
See [e.g., @a; @b p. 3], [see @c] and [@d available at x].
```
