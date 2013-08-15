#!/usr/bin/env python
from pandoc import toJSONFilter

"""
Pandoc filter that causes emphasis to be rendered using
the custom macro '\myemph{...}' rather than '\emph{...}'
in latex.  Other output formats are unaffected.
"""

def latex(s):
  return {'RawInline': ['latex', s]}

def myemph(k, v, f):
  if k == 'Emph' and f == 'latex':
    return [latex('\\myemph{')] + v + [latex('}')]

if __name__ == "__main__":
  toJSONFilter(myemph)
