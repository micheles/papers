"""
Portable twisted server for pythonic command interpreters. Multiple clients
can connect to it with

$ telnet localhost 1025

The interpreter prompt is expected to end with chr(0).
"""

import sys
from twisted.protocols import basic
from twisted.internet import protocol, reactor
from subprocess import Popen, PIPE

def read(inp, sep=chr(0), fix_nl=True):
    """Read data from ``inp`` until a separator ``sep`` is found. If
    ``fix_nl`` is True, it converts newlines in \r\n, the standard for
    Internet line protocols."""
    out = []
    while True:
        char = inp.read(1)
        if char == sep:
            break
        elif char == "\n" and fix_nl:
            out.append("\r\n")
        else:
            out.append(char)
    return "".join(out) + " "

# Poor man using Popen, not ProcessProtocol as in ptyserv
class InterpreterProtocol(basic.LineReceiver):
    def connectionMade(self):
        print "Got new client!"
        self.cli = Popen([sys.executable, "-u", self.factory.cmd_interpreter],
                         stdin=PIPE, stdout=PIPE)
        self.inp = self.cli.stdin
        self.out = self.cli.stdout
        self.transport.write(read(self.out))

    def connectionLost(self, reason):
        print "Lost a client!"
        print >> self.inp, "quit"

    def lineReceived(self, line):
        print "received", repr(line)
        if line == "quit":
            self.transport.loseConnection()
        else:
            print >> self.inp, line
            self.transport.write(read(self.out))

def run(py_cmd_interpr, port=1025):
    factory = protocol.ServerFactory()
    factory.protocol = InterpreterProtocol
    factory.cmd_interpreter = py_cmd_interpr
    reactor.listenTCP(port, factory)
    reactor.run()

if __name__ == "__main__":
    run("tester.py", 1025)
