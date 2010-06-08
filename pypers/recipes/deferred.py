# deferred.py

from twisted.internet import reactor, defer
from twisted.python import threadable; threadable.init(1)
import sys, time

# BAD IDEA
def deferred(func, *args):
    """Takes a blocking function an converts it into a deferred-valued 
    function running in a separate thread.
    """
    de = defer.Deferred()
    de.addCallback(func)
    reactor.callInThread(de.callback, *args)
    return de

def running():
    sys.stdout.write("."); sys.stdout.flush()
    reactor.callLater(.1, running)

# GOOD IDEA

from twisted.internet.threads import deferToThread as deferred

@deferred.__get__
def sleep(sec):
  print 'start sleep %s' % sec
  time.sleep(sec)
  print '\nend sleep %s' % sec
  return "ok"

def print_(result):
  print result

if __name__ == "__main__":
   sleep(2).addBoth(print_)
   reactor.callLater(.1, running)
   reactor.callLater(3, reactor.stop)
   reactor.run() 


