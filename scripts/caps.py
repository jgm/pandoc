#!/usr/bin/env python

"""Pandoc filter to convert all regular text to uppercase.
Code, link URLs, etc. are not affected.
"""

from pandoc import toJSONFilter

def caps(key, value, format):
  if key == 'Str':
    return {'Str': value.upper()}

if __name__ == "__main__":
  toJSONFilter(caps)
