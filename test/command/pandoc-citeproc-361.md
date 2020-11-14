```
% pandoc --citeproc -t markdown-citations --markdown-headings=setext
---
csl: command/locators.csl
references:
- id: citekey
  title: Title
  type: 'article-journal'
- id: other
  title: Other
  type: 'article-journal'
suppress-bibliography: true
---

Content[@citekey]

Content[@citekey, 1]

Content[@citekey, 2]

Content[@citekey, 2]

Content[@citekey 2]

Content[@citekey, p. 2]

Content[@citekey p. 2]

Content[@citekey]

Content[@other]

Content[@citekey, 3]

Content[@citekey, 3]
^D
Content[^1]

Content[^2]

Content[^3]

Content[^4]

Content[^5]

Content[^6]

Content[^7]

Content[^8]

Content[^9]

Content[^10]

Content[^11]

[^1]: Title.

[^2]: Ibid-with-locator {1}.

[^3]: Ibid-with-locator {2}.

[^4]: Ibid.

[^5]: Ibid.

[^6]: Ibid.

[^7]: Ibid.

[^8]: Subsequent.

[^9]: Other.

[^10]: Subsequent {3}.

[^11]: Ibid.
```
