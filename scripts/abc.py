#!/usr/bin/env python

"""
Pandoc filter to process code blocks with class "abc" containing
ABC notation into images.  Assumes that abcm2ps and ImageMagick's
convert are in the path.  Images are put in the abc-images directory.
"""

import hashlib
import os
import sys
from pandoc import toJSONFilter
from subprocess import Popen, PIPE, call

imagedir = "abc-images"

def sha1(x):
  return hashlib.sha1(x).hexdigest()

def abc2eps(abc, filetype, outfile):
  p = Popen(["abcm2ps", "-O", outfile + '.eps', "-"],stdin=PIPE)
  p.stdin.write(abc)
  p.communicate()
  p.stdin.close()
  call(["convert", outfile + '.eps', outfile + '.' + filetype])

def abc(key, value, format):
  if key == 'CodeBlock':
    [[ident,classes,keyvals], code] = value
    if "abc" in classes:
      outfile = imagedir + '/' + sha1(code)
      if format == "html":
        filetype = "png"
      elif format == "latex":
        filetype = "pdf"
      else:
        filetype = "png"
      src = outfile + '.' + filetype
      if not os.path.isfile(src):
        try:
          os.mkdir(imagedir)
          sys.stderr.write('Created directory ' + imagedir + '\n')
        except OSError:
          pass
        abc2eps(code, filetype, outfile)
        sys.stderr.write('Created image ' + src + '\n')
      return {'Para': [{'Image': [[], [src,""]]}]}

if __name__ == "__main__":
  toJSONFilter(abc)
