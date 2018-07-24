from twisted.internet import reactor
import sys

def printdots():
    sys.stdout.write(".")
    sys.stdout.flush()
    reactor.callLater(.1, printdots)
    
def print_hello_and_exit():
    print "Hello!"
    reactor.stop()

reactor.callLater(0, printdots)
reactor.callLater(2, print_hello_and_exit)
print "Starting mainloop",
reactor.run()
                  
