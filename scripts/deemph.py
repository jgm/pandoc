#!/usr/bin/env python
from pandoc import walk, toJSONFilter
from caps import caps

def deemph(k,v,f):
  if k == 'Emph' and f == 'html':
    return walk(v,caps,f)

if __name__ == "__main__":
  toJSONFilter(deemph)
