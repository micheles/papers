import os, random, threading
from ms.file_utils import ifiles
from ms.concurrency import Popen, locked

PLAYER = "mplay32 /play /close".split()
MUSICDIR = os.environ.get("HOMEPATH", os.environ["HOME"]) + "/Desktop/Music"

songs = list(ifiles(MUSICDIR, lambda f : f.endswith(".mp3")))

def run(func, *args, **kw):
    threading.Thread(None, func, args=args, kwargs=kw).start()
    
def gen_songs():
    for i in range(2):
        yield random.choice(songs)

def play(song):
    return Popen(PLAYER + [song])

@locked  
def play_many(user):
    for song in gen_songs():
        print user, song
        player = play(song)
        player.wait()
    
if __name__ == "__main__":
    run(play_many, "user1")
    run(play_many, "user2")
    run(play_many, "user3")
    
