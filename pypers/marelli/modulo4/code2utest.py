"""
Convert tests from modules into test cases. 
"""

import sys, unittest, time
from ms.misc_utils import import_

def makeSuite(name, code):
    dic = {}
    exec compile(code, name, 'exec') in dic
    tests = []
    for name, test in dic.iteritems():
        if name.startswith("test"):
            tests.append([name, test])
    TC = type(name, (unittest.TestCase,), dict(tests))
    return unittest.makeSuite(TC)

class Printer(object):
    def write(self, text):
        sys.stdout.write(text)
        
def run(modulenames, stream=Printer(), descriptions=1, verbosity=1):
    runner = unittest.TextTestRunner(stream, descriptions, verbosity)
    suites = [makeSuite(name, code) for name, code in modulenames]
    return runner.run(unittest.TestSuite(suites))

def fnames2list(fnames):
    return [(fname[:-3], file(fname).read()) for fname in fnames]

if __name__ == "__main__": # example
    print run(fnames2list('utest_1.py utest_2.py'.split()))
    
