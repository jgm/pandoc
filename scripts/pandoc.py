import sys
import json

def walk(x, action, format = ""):
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
  doc = json.loads(sys.stdin.read())
  if len(sys.argv) > 1:
    format = sys.argv[1]
  else:
    format = ""
  altered = walk(doc, action, format)
  json.dump(altered, sys.stdout)

def rawInline(format, s):
  return {"RawInline": [{"unFormat": format}, s]}

def rawBlock(format, s):
  return {"RawBlock": [{"unFormat": format}, s]}

def attributes(attrs):
  attrs = attrs or []
  ident = attrs["id"] or ""
  classes = attrs["classes"] or []
  keyvals = [x for x in attrs and x != "classes" and x != "id"]
  return [ident, classes, keyvals]
