"""
A model for a file downloader:  Synchronous,  Threaded and Asynchronous.
"""

import os, sys, cmd, time, select
from ms.debug_utils import TkWindow
from ms.async_utils import multi_iter
from downloader1 import Downloader

class AsynchronousDownloader(Downloader):
    """Works completely without threads, using an asynchronous command loop."""
    def preloop(self):
        self.downloads = []
        self.mi = multi_iter(self.downloads, terminate=False)

    def do_download(self, arg):
        self.downloads.append(self.download(arg))

    def download(self, fname):
        self.out.write("BEGIN %s" % fname)
        for line in file(fname):
            time.sleep(.1)
            self.out.write(".")
            yield None
        self.out.write(" END %s\n" % fname)

    def cmdloop(self, intro=None):
        """Select-based asynchronous command loop. It only works on Unix and
        it does not use the readline library. The advantage is that the command
        loop is nonblocking. The loop runs the multi iterator self.mi.
        """
        self.preloop()
        if intro is not None:
            self.intro = intro
        if self.intro:
            self.stdout.write(str(self.intro) + "\n")
        self.stdout.write(self.prompt)
        self.stdout.flush()
        stop = None
        while not stop:
            i, o, e = select.select([self.stdin], [], [], 0)
            if i:
                line = i[0].readline()
                if not len(line):
                    line = 'EOF'
                else:
                    line = line[:-1] # chop \n
                line = self.precmd(line)
                stop = self.onecmd(line)
                stop = self.postcmd(stop, line)
                self.stdout.write(self.prompt)
                self.stdout.flush()
            self.mi.next()
        self.postloop()
  
if __name__ == "__main__":
    AsynchronousDownloader().cmdloop()
