```
% pandoc -f biblatex -t markdown -s
@comment{adapted from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

TODO: Is there a CSL counterpart for the biblatex field "holder"?}


@patent{almendro,
	Annotation = {This is a patent entry with a location field. The number is given in the number field. Note the format of the location field in the database file. Compare laufenberg, sorace, and kowalik},
	Author = {Almendro, José L. and Martín, Jacinto and Sánchez, Alberto and Nozal, Fernando},
	Date = 1998,
	Hyphenation = {german},
	Location = {countryfr and countryuk and countryde},
	Number = {EU-29702195U},
	Title = {Elektromagnetisches Signalhorn}}

@patent{kowalik,
	Annotation = {This is a patent entry for a French patent request with a full date. The number is given in the number field. Note the format of the type and date fields in the database file. Compare almendro, laufenberg, and sorace},
	Author = {Kowalik, F. and Isard, M.},
	Date = {1995-01-11},
	Hyphenation = {french},
	Indextitle = {Estimateur d'un défaut de fonctionnement},
	Number = 9500261,
	Title = {Estimateur d'un défaut de fonctionnement d'un modulateur en quadrature et étage de modulation l'utilisant},
	Type = {patreqfr}}

@patent{laufenberg,
	Annotation = {This is a patent entry with a holder field. Note the format of the type and location fields in the database file. Compare almendro, sorace, and kowalik},
	Author = {Laufenberg, Xaver and Eynius, Dominique and Suelzle, Helmut and Usbeck, Stephan and Spaeth, Matthias and Neuser-Hoffmann, Miriam and Myrzik, Christian and Schmid, Manfred and Nietfeld, Franz and Thiel, Alexander and Braun, Harald and Ebner, Norbert},
	Date = {2006-09-13},
	File = {http://v3.espacenet.com/textdoc?IDX=EP1700367},
	Holder = {{Robert Bosch GmbH} and {Daimler Chrysler AG} and {Bayerische Motoren Werke AG}},
	Hyphenation = {german},
	Number = 1700367,
	Title = {Elektrische Einrichtung und Betriebsverfahren},
	Type = {patenteu},
	Abstract = {The invention relates to an electric device comprising a
                  generator, in particular for use in the vehicle electric
                  system of a motor vehicle and a controller for controlling the
                  generator voltage. The device is equipped with a control zone,
                  in which the voltage is controlled and zones, in which the
                  torque is controlled. The invention also relates to methods
                  for operating a device of this type.}}

@patent{sorace,
	Annotation = {This is a patent entry with a holder field. Note the format of the type and date fields in the database file. Compare almendro, laufenberg, and kowalik},
	Author = {Sorace, Ronald E. and Reinhardt, Victor S. and Vaughn, Steven A.},
	Date = {1997-09-16},
	Date-Modified = {2013-10-16 13:44:15 +0000},
	Holder = {{Hughes Aircraft Company}},
	Hyphenation = {american},
	Number = 5668842,
	Title = {High-Speed Digital-to-{RF} Converter},
	Type = {patentus}}

^D
---
nocite: "[@*]"
references:
- annote: This is a patent entry with a location field. The number is
    given in the number field. Note the format of the location field in
    the database file. Compare laufenberg, sorace, and kowalik
  author:
  - family: Almendro
    given: José L.
  - family: Martín
    given: Jacinto
  - family: Sánchez
    given: Alberto
  - family: Nozal
    given: Fernando
  id: almendro
  issued: 1998
  jurisdiction: France; United Kingdom; Germany
  language: de-DE
  number: EU-29702195U
  title: Elektromagnetisches Signalhorn
  type: patent
- annote: This is a patent entry for a French patent request with a full
    date. The number is given in the number field. Note the format of
    the type and date fields in the database file. Compare almendro,
    laufenberg, and sorace
  author:
  - family: Kowalik
    given: F.
  - family: Isard
    given: M.
  genre: French patent request
  id: kowalik
  issued: 1995-01-11
  language: fr-FR
  number: 9500261
  title: Estimateur d'un défaut de fonctionnement d'un modulateur en
    quadrature et étage de modulation l'utilisant
  type: patent
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
- annote: This is a patent entry with a holder field. Note the format of
    the type and date fields in the database file. Compare almendro,
    laufenberg, and kowalik
  author:
  - family: Sorace
    given: Ronald E.
  - family: Reinhardt
    given: Victor S.
  - family: Vaughn
    given: Steven A.
  genre: U.S. patent
  id: sorace
  issued: 1997-09-16
  language: en-US
  number: 5668842
  title: High-speed digital-to-RF converter
  type: patent
---


```
