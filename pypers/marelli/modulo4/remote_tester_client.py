"""Read code from a file, send it to the server and display the result.

usage: %prog testfile [options]

"""

import os, sys

from twisted.protocols.basic import LineReceiver
from twisted.internet import reactor, protocol

from remote_tester_server import PORT

class ClientFactory(protocol.ClientFactory):
    class protocol(LineReceiver):
        def connectionMade(self):
            print "Sending data to server ..."
            send(self.factory.name_code, self.transport)
        def connectionLost(self, arg):
            print "Connection lost!", arg
        def lineReceived(self, line):
            print line
            if line == "bye":
                pass
                #self.transport.loseConnection("regular exit")
                #reactor.stop()
    def __init__(self, name, lines):
        self.name_code = name, lines
            
def send((name, rfile), transport):
    transport.write("BEGIN %s\r\n" % name)
    for line in rfile:
        transport.write(line.rstrip() + "\r\n")
    transport.write("END\r\n") 

def send_and_wait(fname, lines):
    reactor.connectTCP("localhost", PORT, ClientFactory(fname, lines))
    reactor.run()

if __name__ == "__main__":
    try:
        testfile = sys.argv[1]
    except IndexError:
        print __doc__
    else:
        send_and_wait(os.path.basename(testfile), file(testfile, "U"))
