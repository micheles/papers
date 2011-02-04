## NOTE: THE UNBUFFERED BIT IS ESSENTIAL!

import sys, time
from twisted.internet import reactor, protocol
from twisted.protocols import basic
from ms.concurrency import Popen, PIPE

PORT = 8000

class LineInterpreter(basic.LineReceiver):
    #from os import sep as delimiter
    
    def connectionMade(self):
        self.interpreter = Popen(self.factory.cmd, stdin=PIPE, stdout=PIPE)
        self.inp = self.interpreter.stdin
        self.out = self.interpreter.stdout
        self.transport.write("\r\n%s\r\n" % " ".join(self.factory.cmd))
        time.sleep(.1)
        print "".join(self.read_output())

    def connectionLost(self, reason):
        print >> self.inp, "quit"        
              
    def lineReceived(self, line):
        print line
        if line == "quit":
            self.transport.loseConnection()
        else:
            print >> self.inp, line
            self.transport.write("\r\n%s" % "\r".join(self.read_output()))
            
    def read_output(self):
        while True:
            outline = self.out.readline()
            yield outline   
            if outline.startswith("(Cmd) "):
                #yield outline[6:]
                break

        
if __name__== "__main__":
    # cmd = sys.argv[1:]
    cmd = sys.executable, "-u", "tester.py"
    if cmd:
        factory = protocol.ServerFactory()
        factory.protocol = LineInterpreter
        factory.cmd = cmd
        reactor.listenTCP(PORT, factory)
        try:
            reactor.run()
        except Exception, e:
            print e
            reactor.stop()
    else:
        print "usage: %prog line-interpreter-command"
