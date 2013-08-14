#!/usr/bin/env python
from pandoc import toJSONFilter, rawInline

def myemph(k, v, f):
  if k == 'Emph' and f == 'latex':
    v.insert(0, rawInline("latex", "\\myemph{"))
    v.append(rawInline("latex", "}"))
    return v

if __name__ == "__main__":
  toJSONFilter(myemph)
