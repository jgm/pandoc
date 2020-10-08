```
% pandoc -f biblatex -t markdown -s
@comment{

Entry contains url and doi fields; these should be discarded since the
options field specifies url=false, doi=false.

Exception: As in standard biblatex, in online entries url should never be
discarded, even if options contains url=false.

}

@article{item1,
    Author = {Author, Andy},
    Date = {2012},
    Doi = {1234/5678.90},
    Journal = {Journal},
    Options = {url=false, doi=false},
    Title = {Title, Any Entry Type Except online},
    Url = {http://foo.bar}
}

@online{item2,
    Author = {Author, Andy},
    Date = {2012},
    Doi = {1234/5678.90},
    Journal = {Journal},
    Options = {url=false, doi=false},
    Title = {Title, Entry Type online},
    Url = {http://foo.bar}
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Author
    given: Andy
  container-title: Journal
  id: item1
  issued: 2012
  title: Title, any entry type except online
  type: article-journal
- author:
  - family: Author
    given: Andy
  container-title: Journal
  id: item2
  issued: 2012
  title: Title, entry type online
  type: webpage
  url: "http://foo.bar"
---


```
