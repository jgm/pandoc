```
% pandoc -f biblatex -t markdown -s
@comment{excerpt from http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/examples/biblatex-examples.bib

TODO: Where to map "file" field?
}

@report{chiu,
	Annotation = {This is a report entry for a research report. Note the format of the type field in the database file which uses a localization key. The number of the report is given in the number field. Also note the sorttitle and indextitle fields},
	Author = {Chiu, Willy W. and Chow, We Min},
	Date = 1978,
	Hyphenation = {american},
	Indextitle = {Hybrid Hierarchical Model, A},
	Institution = {IBM},
	Number = {RC-6947},
	Sorttitle = {Hybrid Hierarchical Model of a Multiple Virtual Storage (MVS) Operating System},
	Title = {A Hybrid Hierarchical Model of a {Multiple Virtual Storage} ({MVS}) Operating System},
	Type = {resreport}}

@report{padhye,
	Annotation = {This is a report entry for a technical report. Note the format of the type field in the database file which uses a localization key. The number of the report is given in the number field. Also note the sorttitle and indextitle fields},
	Author = {Padhye, Jitendra and Firoiu, Victor and Towsley, Don},
	Date = 1999,
	File = {ftp://gaia.cs.umass.edu/pub/Padhey99-markov.ps},
	Hyphenation = {american},
	Indextitle = {Stochastic Model of TCP Reno Congestion Avoidance and Control, A},
	Institution = {University of Massachusetts},
	Location = {Amherst, Mass.},
	Number = {99-02},
	Sorttitle = {A Stochastic Model of TCP Reno Congestion Avoidance and Control},
	Title = {A Stochastic Model of {TCP Reno} Congestion Avoidance and Control},
	Type = {techreport},
	Abstract = {The steady state performance of a bulk transfer TCP flow
                  (i.e., a flow with a large amount of data to send, such as FTP
                  transfers) may be characterized by three quantities. The first
                  is the send rate, which is the amount of data sent by the
                  sender in unit time. The second is the throughput, which is
                  the amount of data received by the receiver in unit time. Note
                  that the throughput will always be less than or equal to the
                  send rate due to losses. Finally, the number of non-duplicate
                  packets received by the receiver in unit time gives us the
                  goodput of the connection. The goodput is always less than or
                  equal to the throughput, since the receiver may receive two
                  copies of the same packet due to retransmissions by the
                  sender. In a previous paper, we presented a simple model for
                  predicting the steady state send rate of a bulk transfer TCP
                  flow as a function of loss rate and round trip time. In this
                  paper, we extend that work in two ways. First, we analyze the
                  performance of bulk transfer TCP flows using more precise,
                  stochastic analysis. Second, we build upon the previous
                  analysis to provide both an approximate formula as well as a
                  more accurate stochastic model for the steady state throughput
                  of a bulk transfer TCP flow.}}
^D
---
nocite: "[@*]"
references:
- annote: This is a report entry for a research report. Note the format
    of the type field in the database file which uses a localization
    key. The number of the report is given in the number field. Also
    note the sorttitle and indextitle fields
  author:
  - family: Chiu
    given: Willy W.
  - family: Chow
    given: We Min
  genre: research report
  id: chiu
  issued: 1978
  language: en-US
  number: RC-6947
  publisher: IBM
  title: A hybrid hierarchical model of a Multiple Virtual Storage (MVS)
    operating system
  type: report
- abstract: The steady state performance of a bulk transfer TCP flow
    (i.e., a flow with a large amount of data to send, such as FTP
    transfers) may be characterized by three quantities. The first is
    the send rate, which is the amount of data sent by the sender in
    unit time. The second is the throughput, which is the amount of data
    received by the receiver in unit time. Note that the throughput will
    always be less than or equal to the send rate due to losses.
    Finally, the number of non-duplicate packets received by the
    receiver in unit time gives us the goodput of the connection. The
    goodput is always less than or equal to the throughput, since the
    receiver may receive two copies of the same packet due to
    retransmissions by the sender. In a previous paper, we presented a
    simple model for predicting the steady state send rate of a bulk
    transfer TCP flow as a function of loss rate and round trip time. In
    this paper, we extend that work in two ways. First, we analyze the
    performance of bulk transfer TCP flows using more precise,
    stochastic analysis. Second, we build upon the previous analysis to
    provide both an approximate formula as well as a more accurate
    stochastic model for the steady state throughput of a bulk transfer
    TCP flow.
  annote: This is a report entry for a technical report. Note the format
    of the type field in the database file which uses a localization
    key. The number of the report is given in the number field. Also
    note the sorttitle and indextitle fields
  author:
  - family: Padhye
    given: Jitendra
  - family: Firoiu
    given: Victor
  - family: Towsley
    given: Don
  genre: technical report
  id: padhye
  issued: 1999
  language: en-US
  number: 99-02
  publisher: University of Massachusetts
  publisher-place: Amherst, Mass.
  title: A stochastic model of TCP Reno congestion avoidance and control
  type: report
---


```
