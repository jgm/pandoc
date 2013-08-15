#!/usr/bin/env python

"""
Pandoc filter to process raw latex tikz environments into images.
Assumes that pdflatex is in the path, and that the standalone
package is available.  Also assumes that ImageMagick's convert
is in the path. Images are put in the tikz-images directory.
"""

import hashlib
import re
import os
import sys
import shutil
from pandoc import toJSONFilter
from subprocess import Popen, PIPE, call
from tempfile import mkdtemp

imagedir = "tikz-images"

def sha1(x):
  return hashlib.sha1(x).hexdigest()

def tikz2image(tikz, filetype, outfile):
  tmpdir = mkdtemp()
  olddir = os.getcwd()
  os.chdir(tmpdir)
  f = open('tikz.tex', 'w')
  f.write("""\\documentclass{standalone}
             \\usepackage{tikz}
             \\begin{document}
             """)
  f.write(tikz)
  f.write("\n\\end{document}\n")
  f.close()
  p = call(["pdflatex", 'tikz.tex'], stdout=sys.stderr)
  os.chdir(olddir)
  if filetype == 'pdf':
    shutil.copyfile(tmpdir + '/tikz.pdf', outfile + '.pdf')
  else:
    call(["convert", tmpdir + '/tikz.pdf', outfile + '.' + filetype])
  shutil.rmtree(tmpdir)

def tikz(key, value, format):
  if key == 'RawBlock':
    [fmt, code] = value
    if fmt['unFormat'] == "latex" and re.match("\\\\begin{tikzpicture}", code):
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
        tikz2image(code, filetype, outfile)
        sys.stderr.write('Created image ' + src + '\n')
      return {'Para': [{'Image': [[], [src,""]]}]}

if __name__ == "__main__":
  toJSONFilter(tikz)
