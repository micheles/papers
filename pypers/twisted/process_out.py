import os, sys

if sys.platform == "win32":
    base = "C:/cygwin/bin/"
    from twisted.internet import win32eventreactor
    win32eventreactor.install()
else:
    base = "/usr/bin/"

from twisted.internet import reactor
from twisted.internet.utils import getProcessOutput

def print_(result):
    print result
    return "ok"

def stop(arg):
    print arg
    reactor.stop()

de = getProcessOutput(base + "wc", ("tlk.txt",))
#print dir(de)
de.addErrback(print_).addErrback(stop).addCallback(print_).addCallback(stop)
reactor.run()
