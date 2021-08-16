```
% pandoc -f biblatex -t markdown -s
@Book{item1,
author="John Doe",
title="First Book",
year="2005",
address="Cambridge",
publisher="Cambridge University Press"
}

@Article{item2,
author="John Doe",
title="Article",
year="2006",
journal="Journal of Generic Studies",
volume="6",
pages="33-34"
}

@InCollection{пункт3,
author="John Doe and Jenny Roe",
title="Why Water Is Wet",
booktitle="Third Book",
editor="Sam Smith",
publisher="Oxford University Press",
address="Oxford",
year="2007"
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Doe
    given: John
  id: item1
  issued: 2005
  publisher: Cambridge University Press
  publisher-place: Cambridge
  title: First book
  type: book
- author:
  - family: Doe
    given: John
  container-title: Journal of Generic Studies
  id: item2
  issued: 2006
  page: 33-34
  title: Article
  type: article-journal
  volume: 6
- author:
  - family: Doe
    given: John
  - family: Roe
    given: Jenny
  container-title: Third book
  editor:
  - family: Smith
    given: Sam
  id: пункт3
  issued: 2007
  publisher: Oxford University Press
  publisher-place: Oxford
  title: Why water is wet
  type: chapter
---


```
