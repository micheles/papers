# doct.py

"""Search text files for examples to run via doctest. Example of usage:

$ doct doc1.txt -v doc2.txt doc3.txt

This runs the tests in doc1.txt and doc3.txt in silent mode and the
tests in doc2.txt in verbose mode."""

import os,sys,doctest,time,textwrap,re

sys.path[0]=os.getcwd() # explicitly puts the current directory in the path

DOTNAME=r'\b[a-zA-Z_][\w\.]*', # identifier with or without dots
SCRIPT=re.compile(r'(?s)#<(%s)>(.*?)#</\1>' % DOTNAME)
# regular expressions to identify code blocks of the form
#<scriptname.py>
#...
#</scriptname.py>

TESTER=doctest.Tester(globs={},verbose=0),doctest.Tester(globs={},verbose=1)
# regular and verbose tester instances

def extractscript(txt):
    for MO in SCRIPT.finditer(txt):
        yield MO.group(1),textwrap.dedent(MO.group(2))
    
def rundoct(fname,tester):
    txt=file(fname,'U').read()
    scriptnames=[]; scriptdict={}
    for scriptname,script in extractscript(txt): # read scripts
        if scriptname not in scriptnames:
            scriptdict[scriptname]=script
            scriptnames.append(scriptname)
        else:
            scriptdict[scriptname]+=script
    for scriptname in scriptnames: # save scripts
        code='# '+scriptname+scriptdict[scriptname]
        print >> file(scriptname,'w'),code
    t0=time.clock()
    failed,total = tester.runstring(txt,fname)
    if failed:
        return ''
    else:
        return "%s: %s tests passed in %s seconds\n" % (
            fname,total,time.clock()-t0)

def main(args=sys.argv[1:]):
    args=iter(args)
    for a in args:
        if a=='-v': # verbose option
            v=1; a=args.next()
        else: # default, non-verbose
            v=0
        print rundoct(fname=a,tester=TESTER[v]),

if __name__=='__main__':
    if sys.argv[1:]: main() # parse arguments
    else: print __doc__ # no arguments given
   
