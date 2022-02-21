```
% pandoc -f biblatex -t markdown -s
@comment{

Nested cross-references (see biber manual v 1.7)

}

@bookinbook{bookinbook-1,
    Crossref = {book-1},
    Title = {Macbeth [title field of bookinbook-1]},
    Chapter = {7},
    Pages = {100-200},
}

@inbook{inbook-1,
    Crossref = {book-1},
    Title = {Macbeth [title field of inbook-1]},
    Chapter = {7},
    Pages = {100-200},
}

@book{book-1,
    Crossref = {mvbook-1},
    Date = {1975},
    Title = {Tragedies [title field of book-1]},
    Volume = {3}
}

@mvbook{mvbook-1,
    Author = {Shakespeare},
    Date = {1970/1980},
    Title = {Collected Works [title field of mvbook-1]},
    Location = {Location},
    Publisher = {Publisher},
    Volumes = {4}
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Shakespeare
  chapter-number: 7
  container-author:
  - family: Shakespeare
  container-title: Collected works \[title field of mvbook-1\]
  id: bookinbook-1
  issued: 1975
  number-of-volumes: 4
  page: 100-200
  publisher: Publisher
  publisher-place: Location
  title: Macbeth \[title field of bookinbook-1\]
  type: chapter
  volume: 3
  volume-title: Tragedies \[title field of book-1\]
- author:
  - family: Shakespeare
  chapter-number: 7
  container-author:
  - family: Shakespeare
  container-title: Collected works \[title field of mvbook-1\]
  id: inbook-1
  issued: 1975
  number-of-volumes: 4
  page: 100-200
  publisher: Publisher
  publisher-place: Location
  title: Macbeth \[title field of inbook-1\]
  type: chapter
  volume: 3
  volume-title: Tragedies \[title field of book-1\]
- author:
  - family: Shakespeare
  container-author:
  - family: Shakespeare
  id: book-1
  issued: 1975
  number-of-volumes: 4
  publisher: Publisher
  publisher-place: Location
  title: Collected works \[title field of mvbook-1\]
  type: book
  volume: 3
  volume-title: Tragedies \[title field of book-1\]
- author:
  - family: Shakespeare
  id: mvbook-1
  issued: 1970/1980
  number-of-volumes: 4
  publisher: Publisher
  publisher-place: Location
  title: Collected works \[title field of mvbook-1\]
  type: book
---


```
