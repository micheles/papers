import os, time
from subprocess import call
from SimpleHTTPServer import test

if __name__ == "__main__":
    result = os.fork()
    if result:
        time.sleep(0.5)
        print "This the pid of the child process: %s. Now I am in the parent."\
              % result
        call(["konqueror", "http://localhost:8000"])
    else:
        print "I am in the child process."
        test()
        

        
