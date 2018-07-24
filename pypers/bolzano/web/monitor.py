from quixote_utils import RootDirectory

import os, time

def second_counter():
    global nsec
    nsec = 0
    while True:
        nsec += 1
        os.NSEC = str(nsec)
        time.sleep(1)


class Root(RootDirectory):
    def _q_index(self):
        return "Numero di secondi passati: %s" % os.NSEC

if __name__ == "__main__":
    os.NSEC = "0"
    result = os.fork()
    if result:
        print "This the pid of the child process: %s. Now I am in the parent."\
              % result
        Root().publish_show("")
    else:
        print "I am in the child process."
        second_counter()

        
