```
% pandoc -f biblatex -t markdown -s
@comment{From jgm/pandoc#1568.  Double quotes escaped using {"}.}

@ARTICLE{Koff2009-gn,
  title       = "Pan-Canadian evaluation of irreversible compression ratios
                 ({"}lossy{"} compression) for development of national
                 guidelines",
  author      = "Koff, David and Bak, Peter and Brownrigg, Paul and
                 Hosseinzadeh, Danoush and Khademi, April and Kiss, Alex and
                 Lepanto, Luigi and Michalak, Tracy and Shulman, Harry and
                 Volkening, Andrew",
  affiliation = "Sunnybrook Health Sciences Centre, 2075 Bayview Ave., Toronto,
                 ON, M4N 3M5, Canada. dkoffmcmaster.ca",
  journal     = "Journal of digital imaging",
}

^D
---
nocite: "[@*]"
references:
- author:
  - family: Koff
    given: David
  - family: Bak
    given: Peter
  - family: Brownrigg
    given: Paul
  - family: Hosseinzadeh
    given: Danoush
  - family: Khademi
    given: April
  - family: Kiss
    given: Alex
  - family: Lepanto
    given: Luigi
  - family: Michalak
    given: Tracy
  - family: Shulman
    given: Harry
  - family: Volkening
    given: Andrew
  container-title: Journal of digital imaging
  id: Koff2009-gn
  title: Pan-canadian evaluation of irreversible compression ratios
    (\"lossy\" compression) for development of national guidelines
  type: article-journal
---


```
