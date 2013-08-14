#!/usr/bin/env python
from pandoc import toJSONFilter
import re

incomment = False

def comment(k,v,format=""):
  global incomment
  if k == 'RawBlock':
    f, s = v
    fmt = f['unFormat']
    if fmt == "html":
      if re.search("<!-- BEGIN COMMENT -->", s):
        incomment = True
        return []
      elif re.search("<!-- END COMMENT -->", s):
        incomment = False
        return []
  if incomment:
    return []  # suppress anything in a comment

if __name__ == "__main__":
  toJSONFilter(comment)
