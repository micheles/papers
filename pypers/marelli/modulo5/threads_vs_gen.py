import threading
from ms.file_utils import ifiles

def count(it):
    n = 0
    for line in it:
        n += len(line)
    return n

#print count(ifiles("/home/micheles", lambda f: f.endswith(".txt"))) # 4610

def synchronous():
    for f in ifiles("/home/micheles", lambda f: f.endswith(".txt")):
        #print f, count(file(f))
        count(file(f))

# python -mtimeit -n 1 -s "from threads_vs_gen import synchronous" "synchronous()"
def threaded():
    for f in ifiles("/home/micheles", lambda f: f.endswith(".txt")):
        threading.Thread(None, count, args=(file(f),)).start()

        
    
# python -mtimeit -n 1 -s "from threads_vs_gen import threaded" "threaded()"

def count_gen(f):
    n = 0
    for _ in file(f):
        n += 1
        yield n

def asynchronous():
    iters = map(count_gen, ifiles("/home/micheles",
                                 lambda f: f.endswith(".txt")))
    for it in iters: # do not iterate on exhausted iterators
        it.next()
 
