from inspect import indentsize
"A simple shortcut"

from itertools import ifilter
from cStringIO import StringIO

stream=StringIO("""
This is a trial text.
It contains a couple of indented lines:

  "This is the first one"

and

  "This is the second one"
  
""")

print filter(indentsize,stream)
for line in ifilter(indentsize,stream):
    print line,
