import threading, time

def print_hello():
    for i in range(10):
        print "hello"
        time.sleep(1)
        if i == 5:
            import pdb; pdb.set_trace()
            raise RuntimeError("Ahia!")


def print_world():
    for i in range(10):
        print "world"
        time.sleep(1)
        
threading.Thread(target=print_hello).start()

threading.Thread(target=print_world).start()

