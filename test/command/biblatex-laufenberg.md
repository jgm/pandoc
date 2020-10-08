```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Laufenberg et al. 2006)

Laufenberg, Xaver, Dominique Eynius, Helmut Suelzle, Stephan Usbeck,
Matthias Spaeth, Miriam Neuser-Hoffmann, Christian Myrzik, et al. 2006.
“Elektrische Einrichtung und Betriebsverfahren.” European patent.


Formatted with pandoc and apa.csl, 2013-10-23:

(Laufenberg et al., 2006)

Laufenberg, X., Eynius, D., Suelzle, H., Usbeck, S., Spaeth, M.,
Neuser-Hoffmann, M., … Ebner, N. (2006, September 13). Elektrische
Einrichtung und Betriebsverfahren. European patent.


NOTES:

- biblio2yaml
	- Is there any equivalent of "holder" in CSL?

}

@Patent{laufenberg,
  author       = {Laufenberg, Xaver and Eynius, Dominique and Suelzle, Helmut
                  and Usbeck, Stephan and Spaeth, Matthias and Neuser-Hoffmann,
                  Miriam and Myrzik, Christian and Schmid, Manfred and Nietfeld,
                  Franz and Thiel, Alexander and Braun, Harald and Ebner,
                  Norbert},
  title        = {Elektrische Einrichtung und Betriebsverfahren},
  number       = 1700367,
  date         = {2006-09-13},
  holder       = {{Robert Bosch GmbH} and {Daimler Chrysler AG} and {Bayerische
                  Motoren Werke AG}},
  type         = {patenteu},
  hyphenation  = {german},
  annotation   = {This is a patent entry with a holder field.
                  Note the format of the type and location
                  fields in the database file. Compare almendro,
                  sorace, and kowalik},
  abstract     = {The invention relates to an electric device comprising a
                  generator, in particular for use in the vehicle electric
                  system of a motor vehicle and a controller for controlling the
                  generator voltage. The device is equipped with a control zone,
                  in which the voltage is controlled and zones, in which the
                  torque is controlled. The invention also relates to methods
                  for operating a device of this type.},
  file         = {http://v3.espacenet.com/textdoc?IDX=EP1700367},
}

^D
---
nocite: "[@*]"
references:
- abstract: The invention relates to an electric device comprising a
    generator, in particular for use in the vehicle electric system of a
    motor vehicle and a controller for controlling the generator
    voltage. The device is equipped with a control zone, in which the
    voltage is controlled and zones, in which the torque is controlled.
    The invention also relates to methods for operating a device of this
    type.
  annote: This is a patent entry with a holder field. Note the format of
    the type and location fields in the database file. Compare almendro,
    sorace, and kowalik
  author:
  - family: Laufenberg
    given: Xaver
  - family: Eynius
    given: Dominique
  - family: Suelzle
    given: Helmut
  - family: Usbeck
    given: Stephan
  - family: Spaeth
    given: Matthias
  - family: Neuser-Hoffmann
    given: Miriam
  - family: Myrzik
    given: Christian
  - family: Schmid
    given: Manfred
  - family: Nietfeld
    given: Franz
  - family: Thiel
    given: Alexander
  - family: Braun
    given: Harald
  - family: Ebner
    given: Norbert
  genre: European patent
  id: laufenberg
  issued: 2006-09-13
  language: de-DE
  number: 1700367
  title: Elektrische Einrichtung und Betriebsverfahren
  type: patent
---


```
