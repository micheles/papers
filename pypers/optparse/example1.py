#!/usr/bin/env python
"""
Given a sequence of text files, replaces everywhere
a regular expression x with a replacement string s.

  usage: %prog files [options]
  -x, --regx=REGX: regular expression
  -r, --repl=REPL: replacement string
  -n, --nobackup: don't make backup copies
"""
import optparse, sys, re

def replace(regx,repl,files,backup_option=True):
    rx=re.compile(regx)
    for fname in files:
        txt=file(fname,"U").read()
        if backup_option:
            print >> file(fname+".bak","w"), txt ,
        print >> file(fname,"w"), rx.sub(repl,txt) ,
         
parser = optparse.OptionParser("usage: %prog files [options]")
   
parser.add_option("-x", "--regx",
                  help="regular expression")
parser.add_option("-r", "--repl",
                  help="replacement string")
parser.add_option("-n", "--nobackup",
                  action="store_false", default=True,
                  help="do not make backup copies")
  
option, files = parser.parse_args()

if not files:
    print "No files given!"
    print __doc__
elif option.regx and option.repl:
    replace(option.regx, option.repl,args, not option.nobackup)
else:
    print "Missing options or unrecognized options."

