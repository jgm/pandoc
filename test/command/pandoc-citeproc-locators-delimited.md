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

Standard page range[@citekey {35-89, 102}]

Alphanumeric[@citekey {abcdefg1234}]

Kitchen sink[@citekey, {123(4)a-8(\[a\]12.398{8})}]

Empty braces inside[@citekey, {{}}]

Label specified[@citekey {p. a}]

Should it work outside? No. [@citekey, p. {(a)}]

Empty locator [@citekey, {}]

Empty locator to force suffix[@citekey {} 123-35 numbers are suffix]

Suffix generally [@citekey {123-35} numbers not, but text is suffix]

With preceding comma[@citekey, {p. VI}]

No commas before label[@citekey, {, p. (p. is not recognised)}]

Trim white space[@citekey, { p. 9 }]

Without delimiters[@citekey, suffix]

With rendering label[@citekey {ss IV div 4 s 128L(7)(a)(i)-(iv), 129(5),
130(b)}]

The text is apparently NOT verbatim; it is lightly processed as page
numbers. [@citekey {no comma, no label, no nothing}]

AGLC-style page \[para\] [@citekey {584 \[78\]}]

Unbalanced curly { breaks the parse[@citekey {p. suffix{suffix}suffix]

Unbalanced curly } ends early[@citekey {green}suffix}suffix]
^D
See <https://github.com/jgm/pandoc-citeproc/pull/362>.

Standard page range[^1]

Alphanumeric[^2]

Kitchen sink[^3]

Empty braces inside[^4]

Label specified[^5]

Should it work outside? No.[^6]

Empty locator[^7]

Empty locator to force suffix[^8]

Suffix generally[^9]

With preceding comma[^10]

No commas before label[^11]

Trim white space[^12]

Without delimiters[^13]

With rendering label[^14]

The text is apparently NOT verbatim; it is lightly processed as page
numbers.[^15]

AGLC-style page \[para\][^16]

Unbalanced curly { breaks the parse[^17]

Unbalanced curly } ends early[^18]

[^1]: Title {35--89, 102}.

[^2]: Ibid-with-locator {abcdefg1234}.

[^3]: Ibid-with-locator {123(4)a--8(\[a\]12.398{8})}.

[^4]: Ibid-with-locator {{}}.

[^5]: Ibid-with-locator {a}.

[^6]: Subsequent, p. {(a)}.

[^7]: Ibid.

[^8]: Ibid 123-35 numbers are suffix.

[^9]: Ibid-with-locator {123--35} numbers not, but text is suffix.

[^10]: Ibid-with-locator {VI}.

[^11]: Ibid-with-locator {, p. (p. is not recognised)}.

[^12]: Ibid-with-locator {9}.

[^13]: Subsequent, suffix.

[^14]: Ibid-with-locator ss {IV div 4 s 128L(7)(a)(i)--(iv), 129(5),
    130(b)}.

[^15]: Ibid-with-locator {no comma, no label, no nothing}.

[^16]: Ibid-with-locator {584 \[78\]}.

[^17]: Subsequent {p. suffix{suffix}suffix.

[^18]: Ibid-with-locator {green}suffix}suffix.
```
