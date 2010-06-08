#!/usr/bin/env python
"""
Given a sequence of text files, replaces everywhere
a regular expression x with a replacement string s.

  usage: %prog files [options]
  -x, --regx=REGX: regular expression
  -r, --repl=REPL: replacement string
  -n, --nobackup: don't make backup copies
  -R, --restore: restore the original from the backup
"""
import optionparse, sys, re

def replace(regx,repl,files,backup_option=True):
    rx=re.compile(regx)
    for fname in files:
        # you could a test to see if the file exists and can be read here
        txt=file(fname,"U").read()
        if backup_option:
            print >> file(fname+".bak","w"), txt ,
        print >> file(fname,"w"), rx.sub(repl,txt) ,

def restore(fname):
    if os.path.exists(fname+".bak"):
        shutil.copyfile(fname+".bak",fname)
    else:
        print "Sorry, there is no backup copy for %s" % fname
            
if __name__=='__main__':      
    option,args=optionparse.parse(__doc__)
    if not option and not args:
        optionparse.exit()
    elif option.regx and option.repl:
        replace(option.regx, option.repl,args, not option.nobackup)
    elif option.restore:
        for fname in args: restore(fname)
    else:
        print "Missing options or unrecognized options."

