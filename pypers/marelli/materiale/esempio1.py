# esempio1.py

import threading, time, sys

def print_hello():
    for i in range(10):
        print "hello"
        if i == 5:
            raise RuntimeError("Problema a runtime")
        time.sleep(1)
        
def print_world():
    for i in range(10):
        print "world"
        time.sleep(1)

threading.Thread(target=print_hello).start()
threading.Thread(target=print_world).start()


