```
% pandoc --citeproc -t plain
---
references:
- author:
  - family: ʾUdhrī
    given: Jamīl
    non-dropping-particle: 'al-'
  issued: 2000
  title: hamza
  id: item1
- author:
  - family: ʿUdhrī
    given: Jamīl
    non-dropping-particle: 'al-'
  issued: 2000
  title: ayn
  id: item2
- author:
  - family: "'Udhrī"
    given: Jamīl
    non-dropping-particle: 'al-'
  issued: 2000
  title: straight apostrophe
  id: item3
- author:
  - family: '‘Udhrī'
    given: Jamīl
    non-dropping-particle: 'al-'
  issued: 2000
  title: inverted curly apostrophe = opening single curly quote (for ayn)
  id: item4
- author:
  - family: '’Udhrī'
    given: Jamīl
    non-dropping-particle: 'al-'
  issued: 2000
  title: curly apostrophe = closing single curly quote (for hamza)
  id: item5
- author:
  - family: Uch
    given: Ann
  id: item6
  issued: 2000
- author:
  - family: Uebel
    given: Joe
  id: item7
  issued: 2000
- author:
  - family: Zzz
    given: Zoe
  id: item8
  issued: 2000
---

Foo [@item1; @item2; @item3; @item4; @item5; @item6; @item7; @item8].
^D
Foo (al-ʾUdhrī 2000; al-ʿUdhrī 2000; al-’Udhrī 2000b, 2000a; al-‘Udhrī
2000; Uch 2000; Uebel 2000; Zzz 2000).

Uch, Ann. 2000.

‘Udhrī, Jamīl al-. 2000. Inverted Curly Apostrophe = Opening Single
Curly Quote (for Ayn).

ʿUdhrī, Jamīl al-. 2000. Ayn.

’Udhrī, Jamīl al-. 2000a. Curly Apostrophe = Closing Single Curly Quote
(for Hamza).

ʾUdhrī, Jamīl al-. 2000. Hamza.

’Udhrī, Jamīl al-. 2000b. Straight Apostrophe.

Uebel, Joe. 2000.

Zzz, Zoe. 2000.

```
