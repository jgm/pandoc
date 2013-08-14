# Author: John MacFarlane <jgm@berkeley.edu>
# Copyright: (C) 2013 John MacFarlane
# License: GPL version 2 or higher

"""
Functions to aid writing python scripts that process the pandoc
AST serialized as JSON.
"""

import sys
import json

def walk(x, action, format = ""):
  """Walk a tree, applying an action to every object.
  Returns a modified tree.
  """
  if isinstance(x, list):
    array = []
    for item in x:
      if isinstance(item, dict):
        for k in item:
          res = action(k, item[k], format)
          if res is None:
            array.append(walk(item, action, format))
          elif isinstance(res, list):
            for z in res:
              array.append(walk(z, action, format))
          else:
            array.append(walk(res, action, format))
      else:
        array.append(walk(item, action, format))
    return array
  elif isinstance(x, dict):
    obj = {}
    for k in x:
      obj[k] = walk(x[k], action, format)
    return obj
  else:
    return x

def toJSONFilter(action):
  """Converts an action into a filter that reads a JSON-formatted
  pandoc document from stdin, transforms it by walking the tree
  with the action, and returns a new JSON-formatted pandoc document
  to stdout.  The argument is a function action(key, value, format),
  where key is the type of the pandoc object (e.g. 'Str', 'Para'),
  value is the contents of the object (e.g. a string for 'Str',
  a list of inline elements for 'Para'), and format is the target
  output format (which will be taken for the first command line
  argument if present).  If the function returns None, the object
  to which it applies will remain unchanged.  If it returns an
  object, the object will be replaced.  If it returns a list, the
  list will be spliced in to the list to which the target object
  belongs.  (So, returning an empty list deletes the object.)
  """
  doc = json.loads(sys.stdin.read())
  if len(sys.argv) > 1:
    format = sys.argv[1]
  else:
    format = ""
  altered = walk(doc, action, format)
  json.dump(altered, sys.stdout)

def rawInline(format, s):
  """Returns a 'RawInline' inline object.
  """
  return {"RawInline": [{"unFormat": format}, s]}

def rawBlock(format, s):
  """Returns a 'RawBlock' inline object.
  """
  return {"RawBlock": [{"unFormat": format}, s]}

def attributes(attrs):
  """Returns an attribute list, constructed from the
  dictionary attrs.
  """
  attrs = attrs or []
  ident = attrs["id"] or ""
  classes = attrs["classes"] or []
  keyvals = [x for x in attrs and x != "classes" and x != "id"]
  return [ident, classes, keyvals]
