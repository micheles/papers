"""
A model for a file downloader:  Synchronous,  Threaded and Asynchronous.
This version does not require an asynchronous command loop, so it works
on Windows too.
"""

import os, sys, cmd, time, threading
from ms.debug_utils import FifoWindow, TkWindow
from ms.async_utils import ThreadedLoopingCall, multi_iter

class Downloader(cmd.Cmd):
    intro = "You may download the following files:\n%s" % \
             "\n".join(f for f in os.listdir(".") if f.endswith(".html"))
    
    out = TkWindow() # class level, since there is only one
    
    def postloop(self):
        self.out.close()
    def do_quit(self, arg):
        return True
    def download(self, fname):
        self.out.write("BEGIN %s" % fname)
        for line in file(fname):
            time.sleep(.1)
            self.out.write(".")
        self.out.write(" END %s\n" % fname)

class SynchronousDownloader(Downloader):
    def do_download(self, arg):
        self.download(arg)
        
class ThreadedDownloader(Downloader):
    def do_download(self, arg):
        self.thread = threading.Thread(target=self.download, args=(arg,))
        self.thread.start()
    def postloop(self):
        self.thread.join() # wait for the thread to end before closing self.out
        Downloader.postloop(self)
        
class QuasiAsynchronousDownloader(Downloader):
    def preloop(self):
        self.downloads = []
        mi = multi_iter(self.downloads, terminate=False)
        self.lc = ThreadedLoopingCall(mi.next)
        self.lc.start()
    def postloop(self):
        self.lc.stop()
        Downloader.postloop(self)
    def do_download(self, arg):
        self.downloads.append(self.download(arg))
    def download(self, fname):
        self.out.write("BEGIN %s" % fname)
        for line in file(fname):
            time.sleep(.1)
            self.out.write(".")
            yield None
        self.out.write(" END %s\n" % fname)
    
if __name__ == "__main__":
    #SynchronousDownloader().cmdloop()
    ThreadedDownloader().cmdloop()
    #QuasiAsynchronousDownloader().cmdloop()
