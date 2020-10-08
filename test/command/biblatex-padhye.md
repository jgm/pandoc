```
% pandoc -f biblatex -t markdown -s
@comment{

Adapted from biblatex-example.bib


Formatted with pandoc and chicago-author-date.csl, 2013-10-23:

(Padhye, Firoiu, and Towsley 1999)

Padhye, Jitendra, Victor Firoiu, and Don Towsley. 1999. “A Stochastic
Model of TCP Reno Congestion Avoidance and Control.” Technical report
99-02. Amherst, Mass.: University of Massachusetts.


Formatted with pandoc and apa.csl, 2013-10-23:

(Padhye, Firoiu, & Towsley, 1999)

Padhye, J., Firoiu, V., & Towsley, D. (1999). *A stochastic model of TCP
Reno congestion avoidance and control* (technical report No. 99-02).
Amherst, Mass.: University of Massachusetts.

}

@Report{padhye,
  author       = {Padhye, Jitendra and Firoiu, Victor and Towsley, Don},
  title        = {A Stochastic Model of {TCP Reno} Congestion Avoidance and
                  Control},
  type         = {techreport},
  institution  = {University of Massachusetts},
  date         = 1999,
  number       = {99-02},
  location     = {Amherst, Mass.},
  hyphenation  = {american},
  sorttitle    = {A Stochastic Model of TCP Reno Congestion Avoidance and
                  Control},
  indextitle   = {Stochastic Model of {TCP Reno} Congestion Avoidance and Control,
                  A},
  annotation   = {This is a report entry for a technical report. Note
                  the format of the type field in the database file
                  which uses a localization key. The number of the report is
                  given in the number field. Also note the
                  sorttitle and indextitle fields},
  abstract     = {The steady state performance of a bulk transfer TCP flow
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
                  of a bulk transfer TCP flow.},
  file         = {ftp://gaia.cs.umass.edu/pub/Padhey99-markov.ps},
}

^D
---
nocite: "[@*]"
references:
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
