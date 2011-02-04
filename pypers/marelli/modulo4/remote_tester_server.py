"""Get test code from the client, run it and return the result."""

import sys, unittest, time, threading

from twisted.protocols.basic import LineReceiver
from twisted.internet import reactor, protocol
from twisted.internet.threads import deferToThread 

from ms.misc_utils import import_

PORT = 1025

class Wrapper(object): # it seems buffered :-(
    def __init__(self, obj):
        self._obj = obj
    def write(self, text):
        self._obj.write(text.replace("\n", "\r\n"))
        #sys.stdout.write(text)

# NOTE: different protocols for different clients, but same factory
# transport is an instance of twisted.internet.tcp.Server, which a
# subclass of Connection
class ServerFactory(protocol.ServerFactory):
    class protocol(LineReceiver):
        def connectionMade(self):
            print "Test case got from client %s" % self
        def connectionLost(self, reason):
            print "Connection lost!"
        def lineReceived(self, line):
            print line # good for debugging
            if line.startswith("BEGIN"): # open test file
                fname = "server_" + line.split()[1]
                self.name = fname[:-3]
                self.f = file(fname, "w")
            elif line == "END": # close test file, run tests
                self.f.close()
                de = run([self.name], Wrapper(self.transport))
                de.addErrback(sys.stdout.write)
                de.addCallback(lambda _: self.transport.write("bye\r\n"))
            else: # build test file
                print >> self.f, line # possibile race or not? 
            
def makeSuite(module):
    tests = []
    for name, test in module.__dict__.iteritems():
        if name.startswith("test"):
            tests.append([name, test])
    TC = type(module.__name__, (unittest.TestCase,), dict(tests))
    return unittest.makeSuite(TC)

@deferToThread.__get__ # make asynchronous
def run(modulenames, stream=sys.stdout, descriptions=1, verbosity=1):
    runner = unittest.TextTestRunner(stream, descriptions, verbosity)
    suites = [makeSuite(import_(module)) for module in modulenames]
    return runner.run(unittest.TestSuite(suites))
    
if __name__ == "__main__":
    reactor.listenTCP(PORT, ServerFactory())
    reactor.run()
