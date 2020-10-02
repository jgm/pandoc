```
% pandoc --citeproc -t markdown-citations
---
csl: 'command/chicago-fullnote-bibliography.csl'
references:
- URL: 'https://johnmacfarlane.net/vagueness.pdf'
  id: test1
- URL: 'https://pandoc.org'
  id: test2
- URL: 'https://johnmacfarlane.net'
  id: test3
suppress-bibliography: true
---

Test 1 [@test1, pp. 93--101].

Test 2 [@test2, pp. 93--101].

Test 3 [@test3, pp. 93-101].
^D
Test 1.[^1]

Test 2.[^2]

Test 3.[^3]

[^1]: N.d., 93--101, <https://johnmacfarlane.net/vagueness.pdf>.

[^2]: N.d., 93--101, <https://pandoc.org>.

[^3]: N.d., 93--101, <https://johnmacfarlane.net>.
```
