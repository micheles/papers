#!/usr/bin/env python2.4
# Author: michele.simionato@gmail.com
"""Run doctest on text files. Examples of usage:

$ doct file.txt # run the tests in file.txt
$ doct -v file.txt # run the tests in verbose mode
$ doct -u file.txt # run the tests as unittests
$ doct directory # run all the tests in all the text files into directory.

For simplicity, this version is not recursive.
"""

import os, sys, doctest, time, textwrap, re, types, unittest

# regular expressions to identify code blocks of the form
#<scriptname.py>
#...
#</scriptname.py>
DOTNAME = r'\b[a-zA-Z_][\w\.]*', # identifier with or without dots
SCRIPT = re.compile(r'(?s)#<(%s)>(.*?)#</\1>' % DOTNAME)

# a simple utility to extract code blocks
def extractscript(txt):
    for MO in SCRIPT.finditer(txt):
        yield MO.group(1), textwrap.dedent(MO.group(2))

class TestRunner(object):
    def __init__(self, verbose=False, unitest=False):
        self.verbose = verbose
        self.unitest = unitest
        if unitest:
            self.DocTestSuite = doctest.DocTestSuite
            self.unitTestRunner = unittest.TextTestRunner(verbosity=verbose)
        else:
            self.docTestParser = doctest.DocTestParser()
            self.docTestRunner = doctest.DocTestRunner(verbose=verbose)
        
    def utest(self, fname, txt):
        """Converts doctests to unittests."""
        # works by dynamically generating a module with a suitable docstring
        suite = self.DocTestSuite(types.ModuleType(fname, txt))
        self.unitTestRunner.run(suite)
        return ''

    def dtest(self, fname, txt):
        """Making dtest similar to utest."""
        dt = self.docTestParser.get_doctest(txt, {}, fname, fname, 0)
        t0 = time.clock()
        failed, total = self.docTestRunner.run(dt)
        if failed:
            return ''
        else:
            return "%s: %s tests passed in %s seconds\n" % (
                fname, total, time.clock() - t0)

    def run(self, txt='', fname=''):
        if fname:
            txt += file(fname, 'U').read()
        else:
            fname = '<current-buffer>'     
        scriptnames = []; scriptdict = {}
        for scriptname, script in extractscript(txt): # read scripts
            if scriptname not in scriptnames:
                scriptdict[scriptname] = script
                scriptnames.append(scriptname)
            else:
                scriptdict[scriptname] += script
        for scriptname in scriptnames: # save scripts
            code = '# ' + scriptname + scriptdict[scriptname]
            print >> file(scriptname, 'w'), code
        if self.unitest:
            return self.utest(fname, txt)
        else:
            return self.dtest(fname, txt)

if __name__=='__main__':
    # strip the option arguments
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    if not args: # print usage message
        print __doc__ 
    if len(args) > 1:
        print "Too many args"
    else: # run
        arg = args[0]
        verbose = "-v" in sys.argv
        unitest = "-u" in sys.argv
        if os.path.isdir(arg):
            for fname in os.listdir(arg):
                f = os.path.join(arg, fname)
                print TestRunner(verbose, unitest).run(fname=f)
        elif os.path.isfile(arg):
             print TestRunner(verbose, unitest).run(fname=args[0])
        else:
            raise IOError("File %s not found" % arg)
