from subprocess import Popen
from time import sleep
from os import getpid

print >> file("parent.pid", "w"), getpid()
child = Popen(["python", "child.py"])
print "Parent id: %s" % getpid()
print "Child id: %s" % child.pid
sleep(60)
