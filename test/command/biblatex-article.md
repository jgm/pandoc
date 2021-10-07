```
% pandoc -f biblatex -t markdown -s
@comment{
    - contains:
        - an article entry with just the required fields
        - an article entry with required and all optional fields
    - notes:
        - year, month to be ignored if date is present
        - journal to be ignored if journaltitle is present
        - editortype, editoratype, editorbtype, editorctype, pubstate,
series contain keys which, unless corresponding CSL terms exist, require
locale-specific expansion
    - limitations:
        - annotator, commentator, eid, eprint, eprintclass, eprinttype,
issuetitle, issuesubtitle, language, origlanguage have no matching
counterparts in CSL
        - for editor, editora, editorb, editorc (plus editortype,
editoratype, editorbtype, editorctype) only a subset, editor and director,
has matching counterparts in CSL
    - kludges:
        - note + addendum -> CSL note
        - number + issue -> CSL issue
        - handling of titleaddon
        - handling of (journal) series
            - done properly, this should be mapped to some CSL variable
(version? edition? collection-number?), CSL styles would have to be adapted
            - slightly better kludge would map integer to ordinal + "ser."
("3" -> "3rd ser."); localization keys "newseries" -> "new ser.",
"oldseries" -> "old ser."; and print all other values as is -- but still
wouldn't fit all styles or locales.
    }

@article{article-req,
    Author = {Author, Ann},
    Date = {2013-07-29},
    Hyphenation = {english},
    Journaltitle = {The Journaltitle},
    Title = {An Article Entry with Just the Required Fields}}

@article{article-opt,
    Addendum = {The Addendum},
    Annotator = {Annotator, A.},
    Author = {Author, Jr., Ann A.},
    Commentator = {Commentator, C.},
    Date = {2008-12-31},
    Doi = {10.1086/520976},
    Editor = {Editor, Edward},
    Editora = {Editor, A.},
    Editorb = {Editor, B.},
    Editorc = {Editor, C.},
    Eid = {eid},
    Eprint = {eprint},
    Eprintclass = {eprintclass},
    Eprinttype = {eprinttype},
    Hyphenation = {english},
    Issn = {issn},
    Issue = {issue},
    Issuesubtitle = {The Issuesubtitle},
    Issuetitle = {The Issuetitle},
    Journalsubtitle = {The Journalsubtitle},
    Journaltitle = {The Journaltitle},
    Journal = {The Journal},
    Language = {language},
    Month = {08},
    Year = {2007},
    Note = {The Note},
    Number = {number},
    Origlanguage = {origlanguage},
    Pages = {pages},
    Pubstate = {inpress},
    Series = {newseries},
    Subtitle = {The Subtitle},
    Title = {An Article Entry with the Required and All Optional Fields},
    Titleaddon = {The Titleaddon},
    Translator = {Translator, Ted},
    Url = {http://foo.bar.baz/},
    Urldate = {2013-07-29},
    Version = {version},
    Volume = {volume},
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Author
    given: Ann
  container-title: The Journaltitle
  id: article-req
  issued: 2013-07-29
  language: en-US
  title: An article entry with just the required fields
  type: article-journal
- accessed: 2013-07-29
  author:
  - family: Author
    given: Ann A.
    suffix: Jr.
  collection-title: New series
  container-title: "The Journaltitle: The Journalsubtitle"
  doi: 10.1086/520976
  editor:
  - family: Editor
    given: Edward
  id: article-opt
  issn: issn
  issue: number, issue
  issued: 2008-12-31
  language: en-US
  note: The Note. The Addendum
  page: pages
  status: in press
  title: "An article entry with the required and all optional fields:
    The subtitle. The titleaddon"
  title-short: An article entry with the required and all optional
    fields
  translator:
  - family: Translator
    given: Ted
  type: article-journal
  url: "http://foo.bar.baz/"
  version: version
  volume: volume
---


```
