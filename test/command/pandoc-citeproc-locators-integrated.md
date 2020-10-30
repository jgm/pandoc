```
% pandoc --citeproc -t markdown-citations
---
csl: command/locators.csl
references:
- id: citekey
  title: Title
  type: 'article-journal'
suppress-bibliography: true
---

See <https://github.com/jgm/pandoc-citeproc/pull/362>.

[@citekey, 89, and suffix]

[@citekey, 89, perfect Ibid with suffix]

[@citekey, 123-79, and suffix]

[@citekey, xi, will be entirely suffix]

[@citekey, p. xi, gives you a (page) locator xi]

[@citekey, pp. VII, 89, gives you a (pages) locator VII, 89]

[@citekey, p. VI, VII, VIII-IX, explicit romans]

[@citekey \[89\]]

[@citekey, p. \[89\]]

[@citekey and nothing else]

[@citekey, 123(4)\[5\]6, and suffix]

[@citekey, 3(a), 4.4.8, \[7.6\], 7A(2)(a)(i)-(iv)]

[@citekey, 4B.2a.i(3.4), and suffix]

[@citekey, IV.2A, and suffix]

[@citekey, \[28\], and suffix]

[@citekey, \[39-52\], and suffix]

[@citekey, \[39\]-\[52\], and suffix]

[@citekey, s 123(4)(a)(iv), and suffix]

[@citekey, ss 123(4)-(6), and suffix]

[@citekey, \[13\], and suffix]

[@citekey, p.3, and suffix]

[@citekey, (13 entirely suffix]

[@citekey, p.a entirely suffix]

[@citekey, s (a) entirely suffix]
^D
See <https://github.com/jgm/pandoc-citeproc/pull/362>.

[^1]

[^2]

[^3]

[^4]

[^5]

[^6]

[^7]

[^8]

[^9]

[^10]

[^11]

[^12]

[^13]

[^14]

[^15]

[^16]

[^17]

[^18]

[^19]

[^20]

[^21]

[^22]

[^23]

[^24]

[^1]: Title {89}, and suffix.

[^2]: Ibid, perfect Ibid with suffix.

[^3]: Ibid-with-locator {123--79}, and suffix.

[^4]: Subsequent, xi, will be entirely suffix.

[^5]: Ibid-with-locator {xi}, gives you a (page) locator xi.

[^6]: Ibid-with-locator {VII, 89}, gives you a (pages) locator VII, 89.

[^7]: Ibid-with-locator {VI, VII, VIII--IX}, explicit romans.

[^8]: Ibid-with-locator {\[89\]}.

[^9]: Ibid.

[^10]: Subsequent and nothing else.

[^11]: Ibid-with-locator {123(4)\[5\]6}, and suffix.

[^12]: Ibid-with-locator {3(a), 4.4.8, \[7.6\], 7A(2)(a)(i)--(iv)}.

[^13]: Ibid-with-locator {4B.2a.i(3.4)}, and suffix.

[^14]: Ibid-with-locator {IV.2A}, and suffix.

[^15]: Ibid-with-locator {\[28\]}, and suffix.

[^16]: Ibid-with-locator {\[39--52\]}, and suffix.

[^17]: Ibid-with-locator {\[39\]--\[52\]}, and suffix.

[^18]: Ibid-with-locator s {123(4)(a)(iv)}, and suffix.

[^19]: Ibid-with-locator ss {123(4)--(6)}, and suffix.

[^20]: Ibid-with-locator {\[13\]}, and suffix.

[^21]: Ibid-with-locator {3}, and suffix.

[^22]: Subsequent, (13 entirely suffix.

[^23]: Ibid, p.a entirely suffix.

[^24]: Ibid, s (a) entirely suffix.
```
