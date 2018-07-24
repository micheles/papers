import os
from quixote.directory import Directory
from ms.html_utils import simplepage, makelink
from quixote.publish import Publisher
from quixote import get_field, redirect
from quixote.server.simple_server import run
from quixote.form.form import Form
from ms.misc_utils import Popen # fix Popen

MUSIC = "/mnt/smb/Music"
PLAYER = "/usr/bin/mpg123"

class Current:
    player = None
    song = None
    
def play(song, player = PLAYER):
    print "Playing %s ..." % song
    return Popen([player, song])

def add_to(obj):
    def wrapper(f):
        setattr(obj, f.__name__, f)
        return f
    return wrapper

top = Directory()

top._q_exports= ["", "stopper"]

songs=[os.path.join(MUSIC, x) for x in os.listdir(MUSIC) if x.endswith(".mp3")]

@add_to(top)
def _q_index():
    return simplepage(
        title="Welcome to the Web player!",
        body=selector(songs))

def selector(songs):
    global player, song
    chosen = get_field("select")
    if chosen:
        song = chosen
        player = play(song)
        redirect("stopper") # works with Mozilla, but not with lynx/elinks
    else:
        f = Form()
        f.add_single_select("select", options=songs)
        f.add_submit("play", "Play!")
        return f.render()

def stopper():
    stop = get_field("stop")
    if stop:
        player.kill()
        return simplepage(body = "%s stopped." % song)
    else:
        f = Form()
        f.add_submit("stop", "Stop")
        return simplepage(body= ("Playing %s" % song) +
                          f.render())

top.stopper = lambda : stopper()

if __name__ == '__main__':
    print 'Listening on http://localhost:8080 ...'
    run(lambda : Publisher(top), '', 8080)
