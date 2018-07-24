"""
A model for a file downloader:  Synchronous,  Threaded and Asynchronous.
"""

import os, sys, cmd, time, threading
from ms.debug_utils import TkWindow
from  ms.async_utils import MultiIter, run_iter

from twisted.internet import reactor
from downloader1 import Downloader

class AsynchronousDownloader(Downloader):
    "Twisted based."
    mi = MultiIter()
    def postloop(self):
        reactor.stop()
        while reactor.running: pass # until the stop message is received
        Downloader.postloop(self)
    def do_download(self, arg):
        self.mi.append(self.download(arg))
    def download(self, fname):
        self.out.write("BEGIN %s" % fname)
        for line in file(fname):
            time.sleep(.1)
            self.out.write(".")
            yield None
        self.out.write(" END %s\n" % fname)

def ended(_):
    print _, "ended"
    #d.out.close()
    
if __name__ == "__main__":
    d = AsynchronousDownloader()
    threading.Thread(None, d.cmdloop).start()
    run_iter(d.mi, reactor.callLater, 0)
    reactor.run()
