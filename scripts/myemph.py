#!/usr/bin/env python
from pandoc import toJSONFilter, rawInline

"""Pandoc filter that causes emphasis to be rendered using
the custom macro '\myemph{...}' rather than '\emph{...}'
in latex.  Other output formats are unaffected.
"""

def myemph(k, v, f):
  if k == 'Emph' and f == 'latex':
    return [rawInline("latex", "\\myemph{")] + v + [rawInline("latex","}")]

if __name__ == "__main__":
  toJSONFilter(myemph)
