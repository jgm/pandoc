```
% pandoc -f biblatex -t markdown -s
@comment{
    - Dates
    - Not included in tests:
        - malformed dates and date ranges
        - literal dates ("13th century"; not supported by biblatex)
        - seasons (would have to come from parsing the issue field)
        - uncertain dates (use "doubtfuldate" from biblatex-apa?)
        - dates with single-digit day or month (not supported by biblatex)
        - negative dates (not supported by biblatex)
    - Note: biblatex supports years < 1000 but only if padded with leading zeros
    - TODO:
        - either biblio2yaml or, probably better, citeproc should strip leading zeros
         from days and months (CSL can add leading zeros to its output, but not remove
         them when they are in its input data)
}

@article{year-month-old,
    Author = {Author, Al},
    Journal = {Journal},
    Month = aug,
    Title = {Year and Month, bibtex style, supported by biblatex for backwards compatibility},
    Year = {1999}}

@article{year-month-new,
    Author = {Author, Al},
    Journal = {Journal},
    Month = {08},
    Title = {Year and Month, biblatex style; note that biblatex does not have a ``day'' field},
    Year = {1999}}

@article{dates,
    Author = {Author, Al},
    Date = {2012-12-13},
    Eventdate = {2011-10-03},
    Journal = {Journal},
    Month = may,
    Origdate = {1888-10-01},
    Title = {Dates, default biblatex style; year, month to be ignored if date exists},
    Urldate = {1999-05-23},
    Year = {9999}}

@article{date-ranges-different-years,
    Author = {Author, Al},
    Date = {1999-10-14/2010-01-23},
    Eventdate = {1999-12/2000-01},
    Journal = {Journal},
    Origdate = {1888-01-02/1913-12-13},
    Title = {Date ranges; different years},
    Urldate = {2012-10-12/2013-01-31}}

@article{date-ranges-same-year,
    Author = {Author, Al},
    Date = {1999-10-14/1999-10-15},
    Eventdate = {1999-10/1999-11},
    Journal = {Journal},
    Origdate = {1888-01-02/1888-12-13},
    Title = {Date ranges; same year},
    Urldate = {2012-10-31/2012-11-01}}

@article{date-ranges-open,
    Author = {Author, Al},
    Date = {1999-10-14/},
    Eventdate = {1999-10/},
    Journal = {Journal},
    Origdate = {1888-01-02/},
    Title = {Date ranges, open-ended},
    Urldate = {2012-10-31/}}

@article{dates-very-old,
    Author = {Author, Al},
    Date = {0712-12-13},
    Eventdate = {0311-10-03},
    Journal = {Journal},
    Month = may,
    Origdate = {0088-10-01},
    Title = {Dates, year less than 1000},
    Urldate = {0999-12-14}}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Author
    given: Al
  container-title: Journal
  id: year-month-old
  issued: 1999-08
  title: Year and month, bibtex style, supported by biblatex for
    backwards compatibility
  type: article-journal
- author:
  - family: Author
    given: Al
  container-title: Journal
  id: year-month-new
  issued: 1999-08
  title: Year and month, biblatex style; note that biblatex does not
    have a "day" field
  type: article-journal
- accessed: 1999-05-23
  author:
  - family: Author
    given: Al
  container-title: Journal
  event-date: 2011-10-03
  id: dates
  issued: 2012-12-13
  original-date: 1888-10-01
  title: Dates, default biblatex style; year, month to be ignored if
    date exists
  type: article-journal
- accessed: 2012-10-12/2013-01-31
  author:
  - family: Author
    given: Al
  container-title: Journal
  event-date: 1999-12/2000-01
  id: date-ranges-different-years
  issued: 1999-10-14/2010-01-23
  original-date: 1888-01-02/1913-12-13
  title: Date ranges; different years
  type: article-journal
- accessed: 2012-10-31/2012-11-01
  author:
  - family: Author
    given: Al
  container-title: Journal
  event-date: 1999-10/1999-11
  id: date-ranges-same-year
  issued: 1999-10-14/1999-10-15
  original-date: 1888-01-02/1888-12-13
  title: Date ranges; same year
  type: article-journal
- accessed: 2012-10-31/
  author:
  - family: Author
    given: Al
  container-title: Journal
  event-date: 1999-10/
  id: date-ranges-open
  issued: 1999-10-14/
  original-date: 1888-01-02/
  title: Date ranges, open-ended
  type: article-journal
- accessed: 0999-12-14
  author:
  - family: Author
    given: Al
  container-title: Journal
  event-date: 0311-10-03
  id: dates-very-old
  issued: 0712-12-13
  original-date: 0088-10-01
  title: Dates, year less than 1000
  type: article-journal
---


```
