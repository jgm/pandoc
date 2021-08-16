```
% pandoc --citeproc -t markdown-citations
---
references:
- author:
  - family: ʾUdhrī
    given: Jamīl
    non-dropping-particle: 'al-'
    note: ayn
  id: item1
- author:
  - family: ʿUdhrī
    given: Jamīl
    non-dropping-particle: 'al-'
    note: hamza
  id: item2
- author:
  - family: '\''Udhrī'
    given: Jamīl
    non-dropping-particle: 'al-'
    note: straight apostrophe
  id: item3
- author:
  - family: '‘Udhrī'
    given: Jamīl
    non-dropping-particle: 'al-'
    note: inverted apostrophe = opening single curly quote (for ayn)
  id: item4
- author:
  - family: '’Udhrī'
    given: Jamīl
    non-dropping-particle: 'al-'
    note: apostrophe = closing single curly quote (for hamza)
  id: item5
- author:
  - family: Uch
    given: Ann
  id: item6
- author:
  - family: Uebel
    given: Joe
  id: item7
- author:
  - family: Zzz
    given: Zoe
  id: item8
---

Foo [@item1; @item2; @item3; @item4; @item5; @item6; @item7; @item8].
^D
Foo (al-ʾUdhrī, n.d.; al-ʿUdhrī, n.d.; al-\'Udhrī, n.d.; al-'Udhrī,
n.d.a, n.d.b; Uch, n.d.; Uebel, n.d.; Zzz, n.d.).

::: {#refs .references .csl-bib-body .hanging-indent}
::: {#ref-item6 .csl-entry}
Uch, Ann. n.d.
:::

::: {#ref-item1 .csl-entry}
ʾUdhrī, Jamīl al-. n.d.
:::

::: {#ref-item2 .csl-entry}
ʿUdhrī, Jamīl al-. n.d.
:::

::: {#ref-item3 .csl-entry}
\'Udhrī, Jamīl al-. n.d.
:::

::: {#ref-item4 .csl-entry}
'Udhrī, Jamīl al-. n.d.a.
:::

::: {#ref-item5 .csl-entry}
---------. n.d.b.
:::

::: {#ref-item7 .csl-entry}
Uebel, Joe. n.d.
:::

::: {#ref-item8 .csl-entry}
Zzz, Zoe. n.d.
:::
:::
```
