import os, time
from subprocess import Popen

def play(fname):
    player = Popen(["/usr/bin/mpg123", fname])
    return player.pid
