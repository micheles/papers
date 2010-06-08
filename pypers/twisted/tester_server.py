import sys
from ms.twisted_utils import run

if __name__ == "__main__":
    print "listening on localhost 1025 ..."
    run([sys.executable, "tester.py"], 1025)
