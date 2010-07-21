"""
Twisted based downloader server.
"""

import os, sys, cmd, time

from twisted.internet import reactor, protocol, task
from twisted.protocols.basic import LineReceiver

from  ms.async_utils import multi_iter
from downloader1 import Downloader

class DownloaderProtocol(LineReceiver, Downloader):
    
    def do_download (self, arg):
        self.downloads.append(self.download(arg))
        
    def download(self, fname):
        self.out.write("BEGIN %s" % fname)
        for line in file(fname):
            time.sleep(.1)
            self.out.write(".")
            yield None
        self.out.write(" END %s\n" % fname)

    def postloop(self):
        pass # don't close self.out too early
    
    def connectionMade(self):
        self.stdout = self.transport
        self.preloop()
        intro = getattr(self, "intro", None)
        if intro:
            self.stdout.write(intro.replace("\n", "\r\n") + "\r\n")
        self.stdout.write(self.prompt)
        self.downloads = []
        mi = multi_iter(self.downloads, terminate=False)
        self.iterloop = task.LoopingCall(mi.next)
        self.iterloop.start(0)
        
    def lineReceived(self, line):
        line = self.precmd(line)
        stop = self.onecmd(line)
        stop = self.postcmd(stop, line)
        if stop:
            self.transport.loseConnection()
        else:
            self.stdout.write(self.prompt)

    def connectionLost(self, reason):
        print "connection lost"
        self.iterloop.stop()
        self.postloop()

def run_server(port):
    factory = protocol.ServerFactory()
    factory.protocol = DownloaderProtocol
    reactor.listenTCP(port, factory)
    reactor.run()
    
if __name__ == "__main__":
    run_server(1025)
    #from twisted.internet.stdio import StandardIO
    #StandardIO(DownloaderProtocol()); reactor.run() # does NOT work
    
