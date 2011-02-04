from quixote_utils import RootDirectory
import os, time, threading

class Counter(threading.Thread):
    def run(self):
        global nsec
        nsec = 0
        self.running = True
        while self.running:
            nsec += 1
            time.sleep(1)
    def stop(self):
        self.running = False


class Root(RootDirectory):
    def _q_index(self):
        return "Numero di secondi passati: %s" % nsec

if __name__ == "__main__":
    counter = Counter()
    counter.start()
    try:
        Root().publish_show("")
    finally:
        counter.stop()

