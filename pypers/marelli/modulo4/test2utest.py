"""
Convert tests from modules into test cases. 
"""

import sys, unittest, time
from ms.misc_utils import import_

def makeSuite(module):
    tests = []
    for name, test in module.__dict__.iteritems():
        if name.startswith("test"):
            tests.append([name, test])
    TC = type(module.__name__, (unittest.TestCase,), dict(tests))
    return unittest.makeSuite(TC)

class Printer(object):
    def write(self, text):
        sys.stdout.write(text)
        
def run(modulenames, stream=Printer(), descriptions=1, verbosity=1):
    runner = unittest.TextTestRunner(stream, descriptions, verbosity)
    suites = [makeSuite(import_(module)) for module in modulenames]
    return runner.run(unittest.TestSuite(suites))
    
if __name__ == "__main__": # example
    print run(['utest_1', 'utest_2'])
    
