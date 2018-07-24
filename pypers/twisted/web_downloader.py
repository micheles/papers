"""
It is not so easy to tinker with the HTTPServer mainloop
(handle_request is blocking). One should use the Twisted server. 
"""

import os, sys, time
sys.path.append(os.environ["HOME"] + "/md/scripts")
from quixote.publish import Publisher
from quixote.directory import Directory
from twisted.internet import task, reactor
from fakemodal import multiform
from fakemodal.controller import Controller
from downloader1 import Downloader, multi_iter

class Interaction(Directory, Downloader):
    _q_exports = ["downloader"]
    downloads = []
    mi = multi_iter(downloads, terminate=False)
    lc = task.LoopingCall(mi.next)
    lc.start(0)
    
    downloader_html = """\
    <html>
    <body>
    <h3>Web downloader</h3>
    <form action="$nextname">
    %s
    <input type="submit" name="exit" value="quit">
    </form>
    </body>
    </html>
    """ % "<br/>\n".join(
        'download <input type="submit" name="download" value="%s">' % f
        for f in os.listdir(".") if f.endswith(".html"))

    @multiform
    def downloader(self, fdict, fd):
        arg = fdict.get("download")
        if arg: self.downloads.append(self.download(arg))
        return fdict

    def download(self, fname):
        self.out.write("BEGIN %s" % fname)
        for line in file(fname):
            time.sleep(.1)
            self.out.write(".")
            yield None
        self.out.write(" END %s\n" % fname)

    def quit(self, fd):
        self.lc.stop()
        reactor.callLater(1, self.stop) # shutdown in 1 second
        return "shutdown in 1 second ..."

    def stop(self):
        self.out.close()
        reactor.stop()

class Home(Directory):
    _q_exports = ["micheles", "pippo"]
    micheles = Interaction() # in principle reserved to user micheles
    pippo = Interaction() # in principle reserved to user pippo

if __name__ == '__main__':
    from quixote.server import twisted_server
    c = Controller('localhost', 7080, twisted_server.run)
    reactor.callLater(1, c.open_browser, "micheles/downloader")
    c.run(lambda : Publisher(Home()))
