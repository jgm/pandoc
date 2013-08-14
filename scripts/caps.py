#!/usr/bin/env python
from pandoc import toJSONFilter

def caps(key, value, format):
  if key == 'Str':
    return {'Str': value.upper()}

if __name__ == "__main__":
  toJSONFilter(caps)
